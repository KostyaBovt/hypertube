-module(hyper_http).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-define(FRONTEND_ORIGIN, <<"http://localhost:3000">>).

-type mod() :: {delete_cookie, Name::binary()} | {set_cookie, Name::binary(), Value::binary()}.
-type handler_ret() :: ok | {ok, map() | mod()} | {ok, map(), mod()} |
                       {redirect, Uri::binary()} | {redirect, Uri::binary(), mod()} |
                       {send_html, Html::binary()} | {error, map() | binary() | integer()}.

-export_type([handler_ret/0]).

-record(state, {handler  :: fun((Req::cowboy_req:req()) -> handler_ret()),
                v_schema :: map() | undefined,
                scope    :: user | admin | undefined}).

-include("hyper.hrl").

-import(hyper_lib, [gv/2, gv/3]).

init(#{path := <<"/api/", Path/binary>>} = Req0, State) ->
    Req1 = set_cors_headers(Req0),
    Req2 = reply(handle_request(Path, Req1), Req1),
    {ok, Req2, State}.

handle_request(Path, #{method := Method} = Req) ->
  case get_exec_state(Method, Path) of
    #state{} = S -> check_permissions(S, Req);
    E -> E
  end.

get_exec_state(<<"GET">>, Path) -> ?MODULE:get(Path);
get_exec_state(<<"POST">>, Path) -> ?MODULE:post(Path);
get_exec_state(<<"OPTIONS">>, _) -> ok;
get_exec_state(_, _) -> {error, 405}.

check_permissions(#state{scope = undefined} = S, Req) -> validate_body(S, Req);
check_permissions(#state{scope = user} = S, Req) ->
    case hyper_auth:get_udata(Req) of
        {ok, #{<<"scope">> := user} = UData} -> validate_body(S, Req#{_hyper_udata => UData});
        E -> E
    end.

validate_body(#state{v_schema = undefined, handler = Handler}, Req) -> Handler(Req);
validate_body(#state{v_schema = VSchema, handler = Handler}, Req) ->
    try liver:validate(VSchema, body(Req)) of
        {ok, Body} -> Handler(Req#{_hyper_body = Body});
        E -> E
    catch Class:Reason:Stacktrace ->
        lager:error("Exception: ~p:~p~nStacktrace: ~p", [Class, Reason, Stacktrace]),
        {error, 400}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

get(<<"auth/udata">>) ->
    #state{};

get(<<"auth/google/", Action/binary>>) ->
    #state{handler = fun(Req) -> hyper_auth:social(<<"google">>, Action, base_url(Req), qs(Req)) end};

get(<<"auth/github/", Action/binary>>) ->
    #state{handler = fun(Req) -> hyper_auth:social(<<"github">>, Action, base_url(Req), qs(Req)) end};

get(<<"auth/registration/confirm">>) ->
    #state{handler = fun(Req) -> hyper_auth:register(qs(Req)) end};

get(<<"auth/lostpass/confirm">>) ->
    #state{handler = fun(Req) -> hyper_auth:recover_password(qs(Req)) end};

get(<<"profile/email/confirm">>) ->
    #state{handler = fun(Req) -> hyper:update_email(qs(Req)) end};

get(<<"user/", Uname>>) ->
    #state{handler = fun(_) -> hyper:get_user(Uname) end,
           scope = user};

get(<<"profile">>) ->
    #state{handler = fun(#{_hyper_udata := UData}) -> hyper:get_profile(UData) end,
           scope = user};

get(_) -> {reply, 404}.

post(<<"auth/logout">>) ->
    #state{handler = fun(_) -> hyper_auth:logout() end};

post(<<"auth/login">>) ->
    #state{handler = fun(#{_hyper_body := Body}) -> hyper_auth:login(password, Body) end};

post(<<"auth/registration">>) ->
    #state{handler = fun(#{_hyper_body := Body} = Req) -> hyper_auth:register(Body, base_url(Req)) end};

post(<<"auth/lostpass">>) ->
    #state{handler = fun(#{_hyper_body := Body} = Req) -> hyper_auth:recover_password(Body, base_url(Req)) end};

post(<<"auth/lostpass/newpass">>) ->
    #state{handler = fun(#{_hyper_body := Body} = Req) -> hyper_auth:set_new_password(Body, cookies(Req)) end};

post(<<"profile">>) ->
    #state{handler = fun(#{_hyper_body := Body, _hyper_udata => UData}) -> hyper:update_profile(Body, UData) end,
           scope = user};

post(<<"profile/email">>) ->
    #state{handler = fun(#{_hyper_body := Body, _hyper_udata => UData}) -> hyper:update_email(Body, UData) end,
           scope = user};

post(<<"profile/pass">>) ->
    #state{handler = fun(#{_hyper_body := Body, _hyper_udata => UData}) -> hyper:update_pass(Body, UData) end,
           scope = user};

post(<<"comment">>) ->
    #state{handler = fun(#{_hyper_body := Body, _hyper_udata => UData}) -> hyper:live_comment(Body, UData) end,
           scope = user};

post(_) -> {reply, 404}.



%% HELPERS

body(Req) -> body(Req, <<>>).
body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->  << Acc/binary, Data/binary >>;
        {more, Data, Req} -> body(Req, << Acc/binary, Data/binary >>)
    end.

base_url(Req) -> iolist_to_binary(cowboy_req:uri(Req, #{path => undefined, qs => undefined})).

qs(Req) -> cowboy_req:parse_qs(Req).

cookies(Req) -> cowboy_req:parse_cookies(Req).

-spec reply(handler_ret(), cowboy_req:req()) -> cowboy_req:req().
reply(ok, Req) ->
    reply(200, #{status => ok}, Req);
reply({ok, Data = #{}}, Req) ->
    reply(200, #{status => ok, payload => Data}, Req);
reply({ok, Mod}, Req) when is_tuple(Mod) ->
    reply(ok, mod_req(Mod, Req));
reply({ok, Data = #{}, Mod}, Req) when is_tuple(Mod) ->
    reply({ok, Data}, mod_req(Mod, Req));
reply({error, Reason}, Req) when is_binary(Reason); is_map(Reason) ->
    reply(200, #{status => error, reason => Reason}, Req);
reply({error, Code}, Req) when is_integer(Code) ->
    cowboy_req:reply(Code, Req);
reply({redirect, Uri}, Req) ->
    TUri = <<(?FRONTEND_ORIGIN)/binary, Uri/binary>>,
    cowboy_req:reply(302, #{<<"location">> => TUri}, Req);
reply({redirect, Uri, Mod}, Req) when is_tuple(Mod) ->
    reply({redirect, Uri}, mod_req(Mod, Req));
reply({send_html, Html}, Req) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req).

-spec reply(Code::non_neg_integer(), Body::map(), Req::cowboy_req:req()) -> cowboy_req:req().
reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jsone:encode(Body), Req).

mod_req({delete_cookie, Name}, Req) -> cowboy_req:set_resp_cookie(Name, <<>>, Req, #{path => "/", max_age => 0});
mod_req({set_cookie, Name, Value}, Req) -> cowboy_req:set_resp_cookie(Name, Value, Req, #{path => "/"}).

set_cors_headers(Req) ->
    Headers = #{<<"Access-Control-Allow-Origin">> => ?FRONTEND_ORIGIN,
                <<"Access-Control-Allow-Methods">> => <<"GET, POST, OPTIONS">>,
                <<"Access-Control-Allow-Headers">> => <<"Content-Type, Access-Control-Allow-Headers, ",
                                                        "Authorization, X-Requested-With">>,
		        <<"Access-Control-Allow-Credentials">> => <<"true">>},
    cowboy_req:set_resp_headers(Headers, Req).
