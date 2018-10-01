-module(matcha_validation).

%% API
-export([load/0]).

-export([unique_email/3,
         unique_uname/3,
	     strong_password/3]).

-include("matcha.hrl").

-define(RULES, [unique_email, unique_uname, strong_password]).

-define(ERRORS,
    #{email_already_taken => <<"email already taken">>,
      uname_already_taken => <<"username already taken">>}).

load() ->
    [liver:add_rule(Rule, ?MODULE) || Rule <- ?RULES],
    maps:map(fun(ErrCode, ErrMsg) -> liver:custom_error(ErrCode, ErrMsg) end, ?ERRORS),
    ok.

unique_email(Args, Email, Opts) ->
    case liver_livr_rules:email(Args, Email, Opts) of
        {ok, _} -> case matcha_mnesia:get_temp_account_by_email(Email) of
                       null -> case matcha_db:user_with_email_exists(Email) of
                                   false -> {ok, Email};
                                   true -> io:format("L2~n"), {error, email_already_taken}
                               end;
                       E -> io:format("L1 ~p~n", [E]), {error, email_already_taken}
                   end;
        E -> E
    end.

unique_uname(_Args, Uname, _Opts) ->
    case length(unicode:characters_to_list(Uname)) of
        Len when Len < ?MIN_UNAME_LENGTH -> {error, too_short};
        Len when Len > ?MAX_UNAME_LENGTH -> {error, too_long};
        _ ->
            case matcha_mnesia:get_temp_account_by_uname(Uname) of
                null -> case matcha_db:user_with_uname_exists(Uname) of
                            false -> {ok, Uname};
                            true -> {error, uname_already_taken}
                        end;
                _ -> {error, uname_already_taken}
            end
    end.

strong_password(_Args, Password, _Opts) ->
    case length(unicode:characters_to_list(Password)) of
        Len when Len < ?MIN_PASS_LENGTH -> {error, too_short};
        Len when Len > ?MAX_PASS_LENGTH -> {error, too_long};
        _ -> {ok, Password}
    end.


