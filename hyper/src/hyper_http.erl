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
               <<"POST">> ->
                   case validate_post(Path, ?BODY(Req)) of
                       {ok, Body} -> post(Path, Body, Req1);
                       E -> E
                   end,
               <<"OPTIONS">> -> cowboy_req:reply(200, Req1);
               _ -> cowboy_req:reply(405, Req1)
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

get(<<"auth">>, Req) -> ok;
get(<<"auth/google/", Action/binary>>, Req) ->
    reply(social_auth(<<"google">>, Action, Req), Req);
get(<<"auth/github/", Action/binary>>, Req) ->
    reply(social_auth(<<"github">>, Action, Req), Req);
get(<<"auth/registration/confirm">>, Req) ->
    reply(hyper_auth:register(cowboy_req:parse_qs(Req)), Req);
get(<<"auth/lostpass/confirm">>, Req) ->
    reply(hyper_auth:recover_password(cowboy_req:parse_qs(Req)), Req);
get(<<"profile/email/confirm">>, Req) ->
    reply(hyper_user:update_email(cowboy_req:parse_qs(Req)), Req);
get(Path, Req) ->
    case hyper:parse_token(Req) of
        {ok, UData} -> get(Path, Req, UData);
        error -> cowboy_req:reply(404, Req)
    end.

get(<<"user/", Uname>>, Req, UData) ->
    reply(hyper:get_user(cowboy_req:parse_qs(Req)), Req);
get(<<"profile">>, Req, UData) ->
    reply(hyper:get_profile(cowboy_req:parse_qs(Req)), Req);
get(_, Req0, _) -> cowboy_req:reply(404, Req0).


post(<<"auth/logout">>, Body, Req) ->
    reply(hyper_auth:logout(cowboy_req:parse_cookies(Req)), Req);
post(<<"auth/login">>, Body, Req) ->
    reply(hyper_auth:login(password, ?BODY(Req)), Req);
post(<<"auth/registration">>, Body, Req) ->
    reply(hyper_auth:register(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/lostpass">>, Body, Req) ->
    reply(hyper_auth:recover_password(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/lostpass/newpass">>, Body, Req) ->
    reply(hyper_auth:set_new_password(?BODY(Req), cowboy_req:parse_cookies(Req)), Req);
post(Path, Body, Req) -> protected_action(Path, Body, Req, post/4).

post(<<"profile">>, Body, Req, UData) ->
    reply(hyper:update_profile(Body, UData), Req);
post(<<"profile/email">>, Body, Req, UData) ->
    reply(hyper:update_email(Body, UData), Req);
post(<<"profile/pass">>, Body, Req, UData) ->
    reply(hyper:update_pass(Body, UData), Req);
post(<<"comment">>, Body, Req, UData) ->
    reply(hyper:live_comment(Body, UData), Req);
post(_, _, Req, _) -> cowboy_req:reply(404, Req).



%% HELPERS

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->  << Acc/binary, Data/binary >>;
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

social_auth(Network, Action, #{qs := Qs} = Req) ->
    case hyper_oauth2:dispatch(?BASE_URL(Req), Network, Action, Qs) of
        {profile, Profile} ->
            lager:info("Profile: ~p", [Profile]),
            hyper_auth:login(social, Profile);
        E -> E
    end.

protected_action(Path, Body, Req, Fun) ->
    case hyper:parse_token(Req) of
        {ok, UData} -> Fun(Path, Body, Req, UData);
        error -> cowboy_req:reply(404, Req)
    end.


reply(ok, Req) ->
    ?REPLY(200, #{status => ok}, Req);
reply({ok, Data = #{}}, Req) ->
    ?REPLY(200, #{status => ok, payload => Data}, Req);
reply({ok, Mod}, Req) when is_tuple(Mod) ->
    reply(ok, mod_req(Mod, Req));
reply({ok, Data = #{}, Mod}, Req) when is_tuple(Mod) ->
    reply({ok, Data}, mod_req(Mod, Req));
reply({error, Reason}, Req) ->
    ?REPLY(200, #{status => error, reason => Reason}, Req);
reply({redirect, Uri}, Req) ->
    TUri = <<(?FRONTEND_ORIGIN)/binary, Uri/binary>>,
    cowboy_req:reply(302, #{<<"location">> => TUri}, Req);
reply({redirect, Uri, Mod}, Req) when is_tuple(Mod) ->
    reply({redirect, Uri}, mod_req(Mod, Req));
reply({send_html, Html}, Req) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req).

mod_req({delete_cookie, Name}, Req) -> cowboy_req:set_resp_cookie(Name, <<>>, Req, #{path => "/", max_age => 0});
mod_req({set_cookie, Name, Value}, Req) -> cowboy_req:set_resp_cookie(Name, Value, Req, #{path => "/"}).

set_cors_headers(Req) ->
    Headers = #{<<"Access-Control-Allow-Origin">> => ?FRONTEND_ORIGIN,
                <<"Access-Control-Allow-Methods">> => <<"GET, POST, OPTIONS">>,
                <<"Access-Control-Allow-Headers">> => <<"Content-Type, Access-Control-Allow-Headers, ",
                                                        "Authorization, X-Requested-With">>,
		        <<"Access-Control-Allow-Credentials">> => <<"true">>},
    cowboy_req:set_resp_headers(Headers, Req).
