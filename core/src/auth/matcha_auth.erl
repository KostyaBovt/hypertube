-module(matcha_auth).

%% API

-export([get_session/1,
         login/2,
         logout/1,
         register/1, register/2,
         recover_password/1, recover_password/2,
         set_new_password/2]).

-include("matcha.hrl").

-import(matcha_lib, [gv/2, gv/3]).

-define(CONFIRMATION_REDIRECT_PATH, <<"/auth">>).
-define(LOGIN_REDIRECT_PATH, <<"/my_profile">>).
-define(PASSWORD_RECOVERING_REDIRECT_PATH, <<"/auth/password_recovering/set_new_password">>).
-define(PASSWORD_RECOVERING_COOKIE_NAME, <<"recovering_token">>).
-define(SESSION_COOKIE_NAME, <<"x-auth-token">>).

-spec get_session(Cookies::proplists:proplist()) -> #session{} | null.
get_session(Cookies) ->
	io:format("COOKIES: ~p~n", [Cookies]),
	matcha_mnesia:get_session(gv(?SESSION_COOKIE_NAME, Cookies)).

-spec login(password | social, proplists:proplist()) -> ok.
login(password, Credentials) ->
    VSchema =
        #{<<"uname">> => [required, {min_length, ?MIN_UNAME_LENGTH}, {max_length, ?MAX_UNAME_LENGTH}],
          <<"password">> => [required, strong_password]},
    case liver:validate(VSchema, Credentials) of
        {ok, #{<<"uname">> := Uname, <<"password">> := Pass}} ->
            case matcha_db:get_user_by_username(Uname) of
                {ok, #{<<"password">> := PassHash, <<"id">> := UId, <<"is_complete">> := IsComplete,
                       <<"location">> := Location} = P0} ->
                    io:format("~p:~p", [Pass, PassHash]),
                    case erlpass:match(Pass, PassHash) of
                        true ->
                            P1 = maps:remove(<<"password">>, P0),
                            P2 = maps:remove(<<"id">>, P1),
                            P3 = matcha_lib:conv_locaction(P2),
                            SID = matcha_mnesia:create_session(Uname, UId, Location, IsComplete),
                            {ok, P3, {set_cookie, <<"x-auth-token">>, SID}};
                        false -> {error, #{<<"password">> => <<"invalid password">>}}
                    end;
                null ->
                    {error, #{<<"uname">> => <<"invalid username">>}}
            end;
        E -> E
    end;
login(social, Profile) ->
    Provider = gv(<<"provider">>, Profile),
    SocialId = gv(<<"id">>, Profile),
    case matcha_db:get_user_by_social_info(Provider, SocialId) of
        {ok, #{<<"username">> := Uname, <<"id">> := UId, <<"is_complete">> := IsComplete}} ->
            return_sid(Uname, UId, IsComplete, ?LOGIN_REDIRECT_PATH);
        null ->
            {ok, #{<<"username">> := Uname, <<"id">> := UId}} =
                create_account_from_social_profile(Profile, Provider, SocialId),
            return_sid(Uname, UId, false, ?LOGIN_REDIRECT_PATH)
    end.

logout(Cookies) ->
    matcha_mnesia:delete_session(gv(?SESSION_COOKIE_NAME, Cookies)),
    {ok, {delete_cookie, ?SESSION_COOKIE_NAME}}.

-spec register(Data::map(), BaseUrl::binary()) -> term().
register(Data0, BaseUrl) ->
    VSchema =
        #{<<"uname">> => [required, unique_uname],
          <<"fname">> => [required, {min_length, ?MIN_FNAME_LENGTH}, {max_length, ?MAX_FNAME_LENGTH}],
          <<"lname">> => [required, {min_length, ?MIN_LNAME_LENGTH}, {max_length, ?MAX_LNAME_LENGTH}],
          <<"password">> => [required, strong_password],
          <<"email">> => [required, unique_email]},
    case liver:validate(VSchema, Data0) of
        {ok, Data1} -> send_registration_confirmation(Data1, BaseUrl);
        E -> E
    end.

-spec register(ConfirmationToken::binary()) -> term().
register(Qs) ->
    Token = gv(<<"token">>, Qs),
    case matcha_mnesia:get_temp_account(Token) of
        #temp_account{uname = Uname, fname = Fname, lname = Lname, password = Pass, email = Email} ->
            PassHash = erlpass:hash(Pass),
            {ok, #{<<"id">> := UId}} = matcha_db:create_user(Uname, Fname, Lname, PassHash, Email),
            matcha_mnesia:delete_temp_account(Token),
            return_sid(Uname, UId, false, ?CONFIRMATION_REDIRECT_PATH);
        false ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.


recover_password(Data0, BaseUrl) ->
    VSchema = #{<<"email">> => [required, email]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"email">> := Email}} ->
            case matcha_db:get_user_by_email(Email) of
                {ok, #{<<"username">> := Uname, <<"id">> := Id}} ->
                    send_password_recovering_confirmation(Id, Uname, Email, BaseUrl);
                null ->
                    {error, #{<<"email">> => <<"cannot find such email">>}}
            end;
        E -> E
    end.

recover_password(Qs) ->
    case matcha_mnesia:get_password_recovering_state(gv(<<"token">>, Qs)) of
        #password_recovering_state{stage = confirmation} = S ->
            NewToken = matcha_lib:rand_str(16),
            matcha_mnesia:update_password_recovering_state(S, NewToken),
            {redirect, ?PASSWORD_RECOVERING_REDIRECT_PATH,
             {set_cookie, ?PASSWORD_RECOVERING_COOKIE_NAME, NewToken}};
        _ ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

set_new_password(Data, Cookies) ->
    Token = gv(?PASSWORD_RECOVERING_COOKIE_NAME, Cookies),
    case matcha_mnesia:get_password_recovering_state(Token) of
        #password_recovering_state{stage = new_password, id = AccountId} ->
            VSchema = #{<<"new_password">> => [required, strong_password]},
            case liver:validate(VSchema, Data) of
                {ok, #{<<"new_password">> := NewPassword}} ->
                    PassHash = erlpass:hash(NewPassword),
                    true = matcha_db:update_user_password(AccountId, PassHash),
                    matcha_mnesia:delete_password_recovering_state(AccountId),
                    {ok, {delete_cookie, ?PASSWORD_RECOVERING_COOKIE_NAME}};
                E -> E
            end;
        E -> io:format("PASS STATE: ~p", [E]), {error, #{<<"new_password">> => <<"cannot update password">>}}
    end.

-spec send_registration_confirmation(map(), BaseUrl::binary()) -> ok.
send_registration_confirmation(#{<<"uname">> := Uname, <<"fname">> := Fname, <<"lname">> := Lname,
                                 <<"password">> := Pass, <<"email">> := Email}, BaseUrl) ->
    Token = matcha_lib:rand_str(16),
    matcha_mnesia:create_temp_account(Uname, Fname, Lname, Pass, Email, Token),
    Url = <<BaseUrl/binary, "/api/auth/registration/confirm_email?token=", Token/binary>>,
    {ok, _} = erlydtl:compile_file("priv/templates/registration.html",
                                   registration_verification_email),
    {ok, Body} = registration_verification_email:render([{username, Uname}, {link, Url}]),
    <<_/binary>> = matcha_lib:send_email(<<"matcha registration">>, iolist_to_binary(Body), Email),
    ok.

send_password_recovering_confirmation(AccountId, Uname, Email, BaseUrl) ->
    Token = matcha_lib:rand_str(16),
    matcha_mnesia:create_password_recovering_state(AccountId, Token),
    Url = <<BaseUrl/binary, "/api/auth/password_recovering/confirm_email?token=", Token/binary>>,
    {ok, _} = erlydtl:compile_file("priv/templates/password_recovering.html",
                                   password_recovering_verification_email),
    {ok, Body} = password_recovering_verification_email:render([{username, Uname}, {link, Url}]),
    <<_/binary>> = matcha_lib:send_email(<<"matcha password recovering">>, iolist_to_binary(Body), Email),
    ok.

create_account_from_social_profile(Profile, Provider, SocialId) ->
    {Fname, Lname} = case gv(<<"name">>, Profile) of
                         Name when is_binary(Name) ->
                             case binary:split(Name, <<" ">>) of
                                 [Fn, Ln] -> {Fn, Ln};
                                 [Fn] -> {Fn, <<>>}
                             end;
                         _ -> {<<>>, <<>>}
                     end,
    Uname = <<"user_", (matcha_lib:rand_str(16))/binary>>,
    matcha_db:create_user_from_social_info(Provider, SocialId, Uname, Fname, Lname).

return_sid(Uname, UId, IsComplete, RedirectPath) ->
    SID = matcha_mnesia:create_session(Uname, UId, null, IsComplete),
    {redirect, RedirectPath, {set_cookie, ?SESSION_COOKIE_NAME, SID}}.

