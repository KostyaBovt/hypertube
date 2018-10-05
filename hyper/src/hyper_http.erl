-module(hyper_http).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-define(BODY(Req), jsone:decode(read_body(Req))).
-define(BASE_URL(Req), iolist_to_binary(cowboy_req:uri(Req, #{path => undefined, qs => undefined}))).
-define(REPLY(Code, Body, Req),
        cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Body), Req)).
-define(FRONTEND_ORIGIN, <<"http://localhost:3000">>).

-include("hyper.hrl").

-import(hyper_lib, [gv/2, gv/3]).

init(#{path := <<"/api/", Path/binary>>, method := Method} = Req0, State) ->
    Req1 = set_cors_headers(Req0),
    Req2 = case Method of
               <<"GET">> -> get(Path, Req1);
               <<"POST">> -> post(Path, Req1);
               <<"OPTIONS">> -> cowboy_req:reply(200, Req1);
               _ -> cowboy_req:reply(405, Req1)
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get(<<"auth">>, Req) -> ok;
get(<<"auth/google/", Action/binary>>, Req) ->
    social_auth(<<"google">>, Action, Req);
get(<<"auth/github/", Action/binary>>, Req) ->
    social_auth(<<"github">>, Action, Req);
get(<<"auth/registration/confirm">>, Req) ->
    auth_reply(hyper_auth:register(cowboy_req:parse_qs(Req)), Req);
get(<<"auth/lostpass/confirm">>, Req) ->
    auth_reply(hyper_auth:recover_password(cowboy_req:parse_qs(Req)), Req);
get(<<"profile/email/confirm">>, Req) ->
    reply(hyper_user:update_email(cowboy_req:parse_qs(Req)), Req);
get(Path, Req) -> protected_action(Path, Req, get/3).

get(<<"user/", Uname>>, Req, UData) ->
    reply(hyper_user:update_email(cowboy_req:parse_qs(Req)), Req);
get(<<"profile">>, Req, UData) ->
    reply(hyper_user:update_email(cowboy_req:parse_qs(Req)), Req);
get(_, Req0, _) -> cowboy_req:reply(404, Req0).


post(<<"auth/logout">>, Req) ->
    auth_reply(hyper_auth:logout(cowboy_req:parse_cookies(Req)), Req);
post(<<"auth/login">>, Req) ->
    auth_reply(hyper_auth:login(password, ?BODY(Req)), Req);
post(<<"auth/registration">>, Req) ->
    auth_reply(hyper_auth:register(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/lostpass">>, Req) ->
    auth_reply(hyper_auth:recover_password(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/lostpass/newpass">>, Req) ->
    auth_reply(hyper_auth:set_new_password(?BODY(Req), cowboy_req:parse_cookies(Req)), Req);
post(Path, Req) -> protected_action(Path, Req, post/3).


post(<<"profile">>, Req, UData) -> reply(with_validation(update_profile, Req, UData), Req);
post(<<"profile/email">>, Req, UData) -> reply(with_validation(update_email, Req, UData), Req);
post(<<"profile/pass">>, Req, UData) -> reply(with_validation(update_pass, Req, UData), Req);
post(<<"comment">>, Req, UData) -> reply(with_validation(live_comment, Req, UData), Req);
post(_, Req, _) -> cowboy_req:reply(404, Req).



%% HELPERS

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->  << Acc/binary, Data/binary >>;
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

social_auth(Network, Action, #{qs := Qs} = Req) ->
    case hyper_oauth2:dispatch(?BASE_URL(Req), Network, Action, Qs) of
        {redirect, Uri} -> cowboy_req:reply(302, #{<<"location">> => Uri}, Req);
        {send_html, Html} -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req);
        {profile, Profile} ->
            lager:info("Profile: ~p", [Profile]),
            auth_reply(hyper_auth:login(social, Profile), Req);
        {error, E} ->
            lager:error("Error: ~p", [E]),
            cowboy_req:reply(500, Req)
    end.

protected_action(Path, Req, Fun) ->
    case hyper:parse_token(Req) of
        {ok, UData} -> Fun(Path, Req, UData);
        error -> cowboy_req:reply(404, Req)
    end.

with_validation(FunName, Req) ->
    case liver:validate(hyper_validation:get_schema(FunName), ?BODY(Req)) of
        {ok, Payload} -> hyper:FunName(Payload);
        E -> E
    end.

with_validation(FunName, Req, UData) ->
    case liver:validate(hyper_validation:get_schema(FunName), ?BODY(Req)) of
        {ok, Payload} -> hyper:FunName(Payload, UData);
        E -> E
    end.

reply(ok, Req) -> ?REPLY(200, #{status => ok}, Req);
reply({ok, Data}, Req) ->  ?REPLY(200, #{status => ok, data => Data}, Req);
reply({error, Reason}, Req) -> ?REPLY(200, #{status => error, reason => Reason}, Req);
reply({redirect, Uri}, Req) ->
    TUri = <<(?FRONTEND_ORIGIN)/binary, Uri/binary>>,
    cowboy_req:reply(302, #{<<"location">> => TUri}, Req).

auth_reply(ok, Req) -> reply(ok, Req);
auth_reply({ok, Mod}, Req) -> reply(ok, mod_req(Mod, Req));
auth_reply({ok, Data, Mod}, Req) -> reply({ok, Data}, mod_req(Mod, Req));
auth_reply({redirect, _} = R, Req) -> reply(R, Req);
auth_reply({redirect, Uri, Mod}, Req) -> reply({redirect, Uri}, mod_req(Mod, Req));
auth_reply({error, Reason} = E, Req) ->
    lager:error("Error: ~p", [Reason]),
    reply(E, Req).

mod_req({delete_cookie, Name}, Req) -> cowboy_req:set_resp_cookie(Name, <<>>, Req, #{path => "/", max_age => 0});
mod_req({set_cookie, Name, Value}, Req) -> cowboy_req:set_resp_cookie(Name, Value, Req, #{path => "/"}).

set_cors_headers(Req) ->
    Headers = #{<<"Access-Control-Allow-Origin">> => ?FRONTEND_ORIGIN,
                <<"Access-Control-Allow-Methods">> => <<"GET, POST, OPTIONS">>,
                <<"Access-Control-Allow-Headers">> => <<"Content-Type, Access-Control-Allow-Headers, ",
                                                        "Authorization, X-Requested-With">>,
		        <<"Access-Control-Allow-Credentials">> => <<"true">>},
    cowboy_req:set_resp_headers(Headers, Req).
