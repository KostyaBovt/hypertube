-module(hyper_validation).

%% API
-export([load/0]).

-export([unique_email/3,
         unique_uname/3,
	     strong_password/3]).

-include("hyper.hrl").

-define(RULES, [unique_email, unique_uname, strong_password]).

-define(ERRORS,
    #{email_already_taken => <<"email already taken">>,
      uname_already_taken => <<"username already taken">>,
      invalid_password => <<"invalid_password">>}).

load() ->
    [liver:add_rule(Rule, ?MODULE) || Rule <- ?RULES],
    maps:map(fun(ErrCode, ErrMsg) -> liver:custom_error(ErrCode, ErrMsg) end, ?ERRORS),
    ok.

unique_email(Args, Email, Opts) ->
    case liver_livr_rules:email(Args, Email, Opts) of
        {ok, _} -> case hyper_mnesia:get_temp_account_by_email(Email) of
                       null -> case hyper_db:user_with_email_exists(Email) of
                                   false -> {ok, Email};
                                   true -> {error, email_already_taken}
                               end;
                       _ -> {error, email_already_taken}
                   end;
        E -> E
    end.

unique_uname(_Args, Uname, _Opts) ->
    case length(unicode:characters_to_list(Uname)) of
        Len when Len < ?MIN_UNAME_LENGTH -> {error, too_short};
        Len when Len > ?MAX_UNAME_LENGTH -> {error, too_long};
        _ ->
            case hyper_mnesia:get_temp_account_by_uname(Uname) of
                null -> case hyper_db:user_with_uname_exists(Uname) of
                            false -> {ok, Uname};
                            true -> {error, uname_already_taken}
                        end;
                _ -> {error, uname_already_taken}
            end
    end.

strong_password(_Args, Password, _Opts) ->
    PatternsList = ["^[0-9 | A-Z | a-z]+$", "[0-9]+", "[A-Z]+", "[a-z]+"],
    case lists:all(fun(Pattern) -> re:run(Password, Pattern) =/= nomatch end, PatternsList) of
        true ->
            case length(unicode:characters_to_list(Password)) of
                Len when Len < ?MIN_PASS_LENGTH -> {error, too_short};
                Len when Len > ?MAX_PASS_LENGTH -> {error, too_long};
                _ -> {ok, Password}
            end;
        false -> {error, invalid_password}
    end.



