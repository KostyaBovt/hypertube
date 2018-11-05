-module(hyper_auth).

%% API

-export([get_udata/1,
         social/4,
         login/2,
         logout/0,
         register/1, register/6,
         recover_password/1, recover_password/2,
         update_password/2,
         update_token_locale/2]).

-include("hyper.hrl").

-import(hyper_lib, [gv/2, gv/3, sv/3]).

-define(LOGIN_REDIRECT_PATH, <<?FRONTEND_ORIGIN, "/">>).
-define(PASSWORD_RECOVERING_REDIRECT_PATH, <<?FRONTEND_ORIGIN, "/auth/lostpass/newpass">>).
-define(PASSWORD_RECOVERING_COOKIE_NAME, <<"recovering-token">>).
-define(AUTH_COOKIE_NAME, <<"x-auth-token">>).
-define(JWT_KEY, <<"change_me">>). % TODO
-define(JWT_ALGO, <<"HS256">>).
-define(DEFAULT_LOCALE, <<"en">>).

-spec get_udata(Cookies::proplists:proplist()) -> {ok, map()} | {error, atom()}.
get_udata(Cookies) ->
    case gv(?AUTH_COOKIE_NAME, Cookies) of
      <<Token/binary>> ->
        case jwt:decode(Token, ?JWT_KEY) of
          {ok, UData} when is_list(UData) -> {ok, maps:from_list(UData)};
          E -> E
        end;
      _ -> {error, token_missed}
    end.


-spec social(Network::binary(), Action::binary(), Url::binary(), Qs::binary()) ->
    hyper_http:handler_ret().
social(Network, Action, Url, Qs) ->
    case hyper_oauth2:dispatch(Url, Network, Action, Qs) of
        {profile, Profile} ->
            io:format("Profile: ~p~n", [Profile]),
            Provider = gv(<<"provider">>, Profile),
            Id = gv(<<"id">>, Profile),
            case hyper_db:get_user_by_social(Provider, Id) of
                {ok, #{<<"id">> := UId} = User} ->
                    hyper_db:update_social_token(UId, gv(<<"access_token">>, Profile)),
                    redirect_with_auth_token(User);
                null ->
                    {ok, User} = create_user_from_social_profile(Profile, Provider, Id),
                    redirect_with_auth_token(User)
            end;
        E -> E
    end.

-spec login(Login::binary(), Pass::binary()) -> hyper_http:handler_ret().
login(Login, Pass) ->
    case hyper_db:get_user_by_uname(Login) of
        {ok, User} -> check_password(Pass, User);
        null ->
            case hyper_db:get_user_by_email(Login) of
                {ok, User} -> check_password(Pass, User);
                null -> {error, #{<<"login">> => incorrect}}
            end
    end.

-spec check_password(Pass::binary(), User::map()) -> hyper_http:handler_ret().
check_password(Pass, #{<<"password">> := PassHash} = User0) ->
    case erlpass:match(Pass, PassHash) of
        true ->
            Token = create_auth_token(User0),
            User1 = maps:with([<<"uname">>, <<"fname">>, <<"lname">>, <<"bio">>,
                               <<"locale">>, <<"avatar">>, <<"email">>, <<"social_provider">>],
                              hyper:prefix_avatar_path(User0)),
            {ok, User1, {set_cookie, ?AUTH_COOKIE_NAME, Token}};
        false -> {error, #{<<"password">> => incorrect}}
    end.

-spec logout() -> hyper_http:handler_ret().
logout() -> {ok, {delete_cookie, ?AUTH_COOKIE_NAME}}.

-spec register(Email::binary(), Uname::binary(), Fname::binary(),
               Lname::binary(), Pass::binary(), BaseUrl::binary()) ->
    ok.
register(Email, Uname, Fname, Lname, Pass, BaseUrl) ->
    Token = hyper_lib:rand_str(16),
    hyper_mnesia:create_temp_account(Uname, Fname, Lname, Pass, Email, Token),
    Url = <<BaseUrl/binary, "/api/auth/registration/confirm?token=", Token/binary>>,
    {ok, _} = erlydtl:compile_file("priv/templates/registration.html", registration_email),
    {ok, EmailBody} = registration_email:render([{username, Uname}, {link, Url}]),
    <<_/binary>> = hyper_lib:send_email(<<"hyper registration">>, iolist_to_binary(EmailBody), Email),
    ok.

-spec register(Qs::proplists:proplist()) -> hyper_http:handler_ret().
register(Qs) ->
    Token = gv(<<"token">>, Qs),
    case hyper_mnesia:get_temp_account(Token) of
        #temp_account{uname = Uname, fname = Fname, lname = Lname, password = Pass, email = Email} ->
            {ok, User} = hyper_db:create_user(Uname, Fname, Lname, erlpass:hash(Pass), Email, ?DEFAULT_LOCALE),
            hyper_mnesia:delete_temp_account(Token),
            redirect_with_auth_token(User);
        null ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

-spec redirect_with_auth_token(User::map()) -> hyper_http:handler_ret().
redirect_with_auth_token(User) ->
    Token = create_auth_token(User),
    {redirect, ?LOGIN_REDIRECT_PATH, {set_cookie, ?AUTH_COOKIE_NAME, Token}}.

-spec create_auth_token(User::map()) -> binary().
create_auth_token(#{<<"id">> := UId, <<"locale">> := Loc}) ->
    Claims = #{<<"iss">> => UId, <<"loc">> => Loc, <<"scp">> => <<"user">>},
    {ok, Token} = jwt:encode(?JWT_ALGO, Claims, ?JWT_KEY),
    Token.

-spec recover_password(Email::binary(), BaseUrl::binary()) -> hyper_http:handler_ret().
recover_password(Email, BaseUrl) ->
    case hyper_db:get_user_by_email(Email) of
        {ok, #{<<"uname">> := Uname, <<"id">> := Id}} ->
            Token = hyper_lib:rand_str(16),
            hyper_mnesia:create_password_recovering_state(Id, Token),
            Url = <<BaseUrl/binary, "/api/auth/lostpass/confirm?token=", Token/binary>>,
            {ok, _} = erlydtl:compile_file("priv/templates/password_recovering.html", password_recovering_email),
            {ok, EmailBody} = password_recovering_email:render([{username, Uname}, {link, Url}]),
            <<_/binary>> = hyper_lib:send_email(<<"Hypertube password recovering">>,
                                                iolist_to_binary(EmailBody), Email),
            ok;
        null ->
            {error, #{<<"email">> => <<"doesn't exist">>}}
    end.

-spec recover_password(Qs::proplists:proplist()) -> hyper_http:handler_ret().
recover_password(Qs) ->
    Token = gv(<<"token">>, Qs),
    case hyper_mnesia:get_password_recovering_state(Token) of
        #password_recovering_state{stage = confirmation} = S ->
            NewToken = hyper_lib:rand_str(16),
            hyper_mnesia:update_password_recovering_state(S, NewToken),
            {redirect, ?PASSWORD_RECOVERING_REDIRECT_PATH, {set_cookie, ?PASSWORD_RECOVERING_COOKIE_NAME, NewToken}};
        _ ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

-spec update_password(NewPass::binary(), Cookies::proplists:proplist()) -> hyper_http:handler_ret().
update_password(NewPass, Cookies) ->
    Token = gv(?PASSWORD_RECOVERING_COOKIE_NAME, Cookies),
    case hyper_mnesia:get_password_recovering_state(Token) of
        #password_recovering_state{stage = new_password, id = UserId} ->
            PassHash = erlpass:hash(NewPass),
            true = hyper_db:update_user_password(UserId, PassHash),
            hyper_mnesia:delete_password_recovering_state(UserId),
            {ok, {delete_cookie, ?PASSWORD_RECOVERING_COOKIE_NAME}};
        _ ->
            {error, #{<<"password">> => <<"cannot update">>}}
    end.

-spec create_user_from_social_profile(Profile::proplists:proplist(), Provider::binary(), SocialId::binary()) ->
    {ok, User::map()}.
create_user_from_social_profile(Profile, Provider, SocialId) ->
    {Fname, Lname} = case gv(<<"name">>, Profile) of
                         Name when is_binary(Name) ->
                             case binary:split(Name, <<" ">>) of
                                 [Fn, Ln | _] -> {Fn, Ln};
                                 [Fn] -> {Fn, <<>>}
                             end;
                         _ -> {<<>>, <<>>}
                     end,
    Uname = <<"user_", (hyper_lib:rand_str(16))/binary>>,
    SocialToken = gv(<<"access_token">>, Profile),
    {ok, _} = hyper_db:create_user_from_social_info(Provider, SocialId, SocialToken,
                                                    Uname, Fname, Lname, ?DEFAULT_LOCALE).

-spec update_token_locale(UData::map(), Locale::binary()) -> hyper_http:handler_ret().
update_token_locale(UData, Locale) ->
    {ok, Token} = jwt:encode(?JWT_ALGO, sv(<<"loc">>, Locale, UData), ?JWT_KEY),
    {ok, {set_cookie, ?AUTH_COOKIE_NAME, Token}}.
