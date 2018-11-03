-module(hyper_http).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-export([get/1, post/1]).

-type mod() :: {delete_cookie, Name::binary()} | {set_cookie, Name::binary(), Value::binary()}.
-type handler_ret() :: ok | {ok, map() | [map()] | mod()} | {ok, map() | [map()], mod()} |
                       {redirect, Uri::binary()} | {redirect, Uri::binary(), mod()} |
                       {send_html, Html::binary()} |
                       {error, map() | binary() | 300..511}.

-export_type([handler_ret/0]).

-record(state, {handler  :: fun((Req::cowboy_req:req()) -> handler_ret()),
                v_schema :: map() | undefined,
                scope    :: user | undefined}).

-include("hyper.hrl").

-import(hyper_lib, [gv/2, gv/3]).

-spec init(Req::cowboy_req:req(), State::term()) -> {ok, cowboy_req:req(), term()}.
init(#{path := <<"/api/", Path/binary>>} = Req0, State) ->
    Req1 = set_cors_headers(Req0),
    Req2 = reply(handle_request(Path, Req1), Req1),
    {ok, Req2, State}.

-spec handle_request(Path::binary(), Req::cowboy_req:req()) -> handler_ret().
handle_request(Path, #{method := Method} = Req) ->
  case get_exec_state(Method, Path) of
    #state{} = S -> check_permissions(S, Req);
    E -> E
  end.

-spec get_exec_state(Method::binary(), Path::binary()) -> #state{} | ok | {error, 405}.
get_exec_state(<<"GET">>, Path) -> ?MODULE:get(Path);
get_exec_state(<<"POST">>, Path) -> ?MODULE:post(Path);
get_exec_state(<<"OPTIONS">>, _) -> ok;
get_exec_state(_, _) -> {error, 405}.

-spec check_permissions(S::#state{}, Req::cowboy_req:req()) -> handler_ret().
check_permissions(#state{scope = undefined} = S, Req) -> validate_body(S, Req);
check_permissions(#state{scope = user} = S, Req) ->
    case hyper_auth:get_udata(cookies(Req)) of
        {ok, #{<<"scp">> := <<"user">>} = UData} -> validate_body(S, Req#{'_hyper_udata' => UData});
        {error, _} -> {error, 403}
    end.

-spec validate_body(S::#state{}, Req::cowboy_req:req()) -> handler_ret().
validate_body(#state{v_schema = undefined, handler = Handler}, Req) -> Handler(Req);
validate_body(#state{v_schema = VSchema, handler = Handler}, Req) ->
    try liver:validate(VSchema, body(Req)) of
        {ok, Body} -> Handler(Req#{'_hyper_body' => Body});
        E -> E
    catch Class:Reason:Stacktrace ->
        io:format("Exception: ~p:~p~nStacktrace: ~p~n", [Class, Reason, Stacktrace]),
        {error, 400}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

-spec get(Path::binary()) -> #state{} | {error, 404}.
get(<<"auth/udata">>) ->
    #state{handler = fun(#{'_hyper_udata' := #{<<"iss">> := Id, <<"loc">> := Locale, <<"scp">> := Scope}}) ->
                         {ok, #{<<"id">> => Id, <<"locale">> => Locale, <<"scope">> => Scope}}
                     end,
           scope = user};

get(<<"auth/google/", Action/binary>>) when Action =:= <<"login">>; Action =:= <<"callback">> ->
    #state{handler = fun(#{qs := Qs} = Req) -> hyper_auth:social(<<"google">>, Action, base_url(Req), Qs) end};

get(<<"auth/github/", Action/binary>>) when Action =:= <<"login">>; Action =:= <<"callback">> ->
    #state{handler = fun(#{qs := Qs} = Req) -> hyper_auth:social(<<"github">>, Action, base_url(Req), Qs) end};

get(<<"auth/intra/", Action/binary>>) when Action =:= <<"login">>; Action =:= <<"callback">> ->
    #state{handler = fun(#{qs := Qs} = Req) -> hyper_auth:social(<<"intra">>, Action, base_url(Req), Qs) end};

get(<<"auth/registration/confirm">>) ->
    #state{handler = fun(Req) -> hyper_auth:register(qs(Req)) end};

get(<<"auth/lostpass/confirm">>) ->
    #state{handler = fun(Req) -> hyper_auth:recover_password(qs(Req)) end};

get(<<"profile/email/confirm">>) ->
    #state{handler = fun(Req) -> hyper:update_email(qs(Req)) end};

get(<<"user/", Uname/binary>>) ->
    #state{handler = fun(_) -> hyper:get_user(Uname) end,
           scope = user};

get(<<"profile">>) ->
    #state{handler = fun(#{'_hyper_udata' := #{<<"iss">> := UId}}) -> hyper:get_profile(UId) end,
           scope = user};

get(<<"comments">>) ->
    #state{handler = fun(Req) -> hyper:get_comments(qs(Req)) end,
           scope = user};

get(<<"social_avatar">>) ->
    #state{handler = fun(#{'_hyper_udata' := #{<<"iss">> := UId}}) -> hyper:import_social_avatar(UId) end,
           scope = user};

get(_) -> {error, 404}.


-spec post(Path::binary()) -> #state{} | {error, 404}.
post(<<"auth/login">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"uname">> := Login, <<"password">> := Password}}) ->
                         hyper_auth:login(Login, Password)
                     end,
           v_schema = #{<<"uname">>    => [required, string],
                        <<"password">> => [required, string]}};

post(<<"auth/logout">>) ->
    #state{handler = fun(_) -> hyper_auth:logout() end};

post(<<"auth/registration">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"email">> := Email, <<"uname">> := Uname, <<"fname">> := Fname,
                                              <<"lname">> := Lname, <<"password">> := Pass}} = Req) ->
                         hyper_auth:register(Email, Uname, Fname, Lname, Pass, base_url(Req))
                     end,
           v_schema = #{<<"email">> => [required, unique_email],
                        <<"uname">> => [required, unique_uname],
                        <<"fname">> => [required, {length_between, [?MIN_FNAME_LENGTH, ?MAX_FNAME_LENGTH]}],
                        <<"lname">> => [required, {length_between, [?MIN_LNAME_LENGTH, ?MAX_LNAME_LENGTH]}],
                        <<"password">> => [required, strong_password]}};

post(<<"auth/lostpass">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"email">> := Email}} = Req) ->
                         hyper_auth:recover_password(Email, base_url(Req))
                     end,
           v_schema = #{<<"email">> => [required, email]}};

post(<<"auth/lostpass/newpass">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"password">> := NewPass}} = Req) ->
                         hyper_auth:update_password(NewPass, cookies(Req))
                     end,
           v_schema = #{<<"password">> => [required, strong_password]}};

post(<<"profile">>) ->
    #state{handler = fun(#{'_hyper_body' := B, '_hyper_udata' := #{<<"iss">> := UId}}) ->
                         hyper:update_profile(UId, gv(<<"uname">>, B), gv(<<"fname">>, B),
                                              gv(<<"lname">>, B), gv(<<"bio">>, B), gv(<<"avatar">>, B))
                     end,
           v_schema = #{<<"uname">> => [unique_uname],
                        <<"fname">> => [{length_between, [?MIN_FNAME_LENGTH, ?MAX_FNAME_LENGTH]}],
                        <<"lname">> => [{length_between, [?MIN_LNAME_LENGTH, ?MAX_LNAME_LENGTH]}],
                        <<"bio">> => [{max_length, ?MAX_BIO_LENGTH}],
                        <<"avatar">> => [string]},
           scope = user};

post(<<"profile/email">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"email">> := Email}, '_hyper_udata' := #{<<"iss">> := UId}} = Req) ->
                         hyper:update_email(UId, Email, base_url(Req))
                     end,
           v_schema = #{<<"email">> => [required, unique_email]},
           scope = user};

post(<<"profile/pass">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"old_password">> := OldPass, <<"new_password">> := NewPass},
                           '_hyper_udata' := #{<<"iss">> := UId}}) -> hyper:update_pass(UId, OldPass, NewPass)
                     end,
           v_schema = #{<<"old_password">> => [required, string],
                        <<"new_password">> => [required, strong_password]},
           scope = user};

post(<<"profile/locale">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"locale">> := NewLocale},
                           '_hyper_udata' := UData}) ->
                         hyper:update_locale(UData, NewLocale)
                     end,
           v_schema = #{<<"locale">> => [required, {one_of, [<<"en">>, <<"ru">>]}]},
           scope = user};


post(<<"comments">>) ->
    #state{handler = fun(#{'_hyper_body' := #{<<"imdb_id">> := ImdbId, <<"text">> := Text},
                           '_hyper_udata' := #{<<"iss">> := UId}}) ->
                         hyper:create_comment(UId, ImdbId, Text)
                     end,
           v_schema = #{<<"imdb_id">> => [required, string],
                        <<"text">> =>    [required, string, not_empty]},
           scope = user};

post(_) -> {error, 404}.

%% HELPERS

body(Req) -> body(Req, <<>>).
body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> jsone:decode(<< Acc/binary, Data/binary >>);
        {more, Data, Req} -> body(Req, << Acc/binary, Data/binary >>)
    end.

base_url(Req) -> iolist_to_binary(cowboy_req:uri(Req, #{path => undefined, qs => undefined})).

qs(Req) -> cowboy_req:parse_qs(Req).

cookies(Req) -> cowboy_req:parse_cookies(Req).

-spec reply(handler_ret(), cowboy_req:req()) -> cowboy_req:req().
reply(ok, Req) ->
    reply(200, #{status => ok}, Req);
reply({ok, Data}, Req) when is_map(Data); is_list(Data) ->
    reply(200, #{status => ok, payload => Data}, Req);
reply({ok, Mod}, Req) when is_tuple(Mod) ->
    reply(ok, mod_req(Mod, Req));
reply({ok, Data, Mod}, Req) ->
    reply({ok, Data}, mod_req(Mod, Req));
reply({error, Reason}, Req) when is_binary(Reason); is_map(Reason) ->
    reply(200, #{status => error, reason => Reason}, Req);
reply({error, Code}, Req) when is_integer(Code) ->
    cowboy_req:reply(Code, Req);
reply({redirect, Uri}, Req) ->
    cowboy_req:reply(302, #{<<"location">> => Uri}, Req);
reply({redirect, Uri, Mod}, Req) ->
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
