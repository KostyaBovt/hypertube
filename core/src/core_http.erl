-module(core_http).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).
-export([get_session/1]).

-define(BODY(Req), jsone:decode(read_body(Req))).
-define(BASE_URL(Req), iolist_to_binary(cowboy_req:uri(Req, #{path => undefined, qs => undefined}))).
-define(REPLY(Code, Body, Req),
        cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Body), Req)).
-define(FRONTEND_ORIGIN, <<"http://localhost:3000">>).

-include("matcha.hrl").

-import(matcha_lib, [gv/2, gv/3]).

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

get(<<"auth/google/", Action/binary>>, Req) ->
    social_auth(<<"google">>, Action, Req);
get(<<"auth/github/", Action/binary>>, Req) ->
    social_auth(<<"github">>, Action, Req);
get(<<"auth/registration/confirm_email">>, Req) ->
    auth_reply(matcha_auth:register(cowboy_req:parse_qs(Req)), Req);
get(<<"auth/password_recovering/confirm_email">>, Req) ->
    auth_reply(matcha_auth:recover_password(cowboy_req:parse_qs(Req)), Req);
get(<<"update_profile/confirm_email">>, Req) ->
    reply(matcha_user:update_email(cowboy_req:parse_qs(Req)), Req); % TODO
get(_, Req0) -> cowboy_req:reply(404, Req0).

post(<<"auth">>, Req) ->
    auth_reply(matcha_auth:login(password, ?BODY(Req)), Req);
post(<<"auth/logout">>, Req) ->
    auth_reply(matcha_auth:logout(cowboy_req:parse_cookies(Req)), Req);
post(<<"auth/registration">>, Req) ->
    auth_reply(matcha_auth:register(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/password_recovering">>, Req) ->
    auth_reply(matcha_auth:recover_password(?BODY(Req), ?BASE_URL(Req)), Req);
post(<<"auth/password_recovering/set_new_password">>, Req) ->
    auth_reply(matcha_auth:set_new_password(?BODY(Req), cowboy_req:parse_cookies(Req)), Req);

post(Path, Req) ->
    case get_session(Req) of
        #session{is_complete = Complete} = S
        when (not Complete andalso (Path =:= <<"get_profile">> orelse Path =:= <<"update_profile">>)) orelse Complete ->
            protected_endpoint(Path, Req, S);
        #session{} ->
            reply({error, profile_not_complete}, Req);
        null ->
            reply({error, not_authorized}, Req)
    end.

protected_endpoint(<<"get_profile">>, Req, Session) ->
    reply(matcha_user:get_profile(?BODY(Req), Session), Req);
protected_endpoint(<<"get_profiles">>, Req, Session) ->
    reply(matcha_user:get_profiles(?BODY(Req), Session), Req);
protected_endpoint(<<"get_chat_history">>, Req, Session) ->
    reply(matcha_user:get_chat_history(?BODY(Req), Session), Req);
protected_endpoint(<<"get_visit_history">>, Req, Session) ->
    reply(matcha_user:get_visit_history(Session), Req);
protected_endpoint(<<"get_likes">>, Req, Session) ->
    reply(matcha_user:get_likes(Session), Req);
protected_endpoint(<<"get_chats">>, Req, Session) ->
    reply(matcha_user:get_chats(Session), Req);

protected_endpoint(<<"update_profile">>, Req, Session) ->
    reply(matcha_user:update_profile(?BODY(Req), Session), Req);
protected_endpoint(<<"update_password">>, Req, Session) ->
    reply(matcha_user:update_password(?BODY(Req), Session), Req);
protected_endpoint(<<"update_email">>, Req, Session) ->
    reply(matcha_user:update_email(?BODY(Req), Session, ?BASE_URL(Req)), Req);

protected_endpoint(<<"send">>, Req, Session) ->
    reply(matcha_user:send(?BODY(Req), Session), Req);
protected_endpoint(<<"like">>, Req, Session) ->
    reply(matcha_user:like(?BODY(Req), Session), Req);
protected_endpoint(<<"unlike">>, Req, Session) ->
    reply(matcha_user:unlike(?BODY(Req), Session), Req);
protected_endpoint(<<"rate">>, Req, Session) ->
    reply(matcha_user:rate(?BODY(Req), Session), Req);
protected_endpoint(<<"block">>, Req, Session) ->
    reply(matcha_user:block(?BODY(Req), Session), Req);
protected_endpoint(<<"unblock">>, Req, Session) ->
    reply(matcha_user:unblock(?BODY(Req), Session), Req);
protected_endpoint(<<"report">>, Req, Session) ->
    reply(matcha_user:report(?BODY(Req), Session), Req);

protected_endpoint(_, Req, _) -> cowboy_req:reply(404, Req).


get_session(Req) -> matcha_auth:get_session(cowboy_req:parse_cookies(Req)).

%% HELPERS

read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->  << Acc/binary, Data/binary >>;
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

social_auth(Network, Action, #{qs := Qs} = Req) ->
    case matcha_oauth2:dispatch(?BASE_URL(Req), Network, Action, Qs) of
        {redirect, Uri} -> cowboy_req:reply(302, #{<<"location">> => Uri}, Req);
        {send_html, Html} -> cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req);
        {profile, Profile} ->
            lager:info("Profile: ~p", [Profile]),
            auth_reply(matcha_auth:login(social, Profile), Req);
        {error, E} ->
            lager:error("Error: ~p", [E]),
            cowboy_req:reply(500, Req)
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
