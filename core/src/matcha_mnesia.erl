-module(matcha_mnesia).
-author("aklymchuk").

%% API
-export([init/0,
         create_temp_account/6,
         get_temp_account/1,
         get_temp_account_by_email/1,
         get_temp_account_by_uname/1,
         delete_temp_account/1,
         create_password_recovering_state/2,
         get_password_recovering_state/1,
         update_password_recovering_state/2,
         delete_password_recovering_state/1,
         create_session/4,
         get_session/1,
         update_session/4,
         delete_session/1,
         create_email_updating_state/3,
         get_email_updating_state/1,
         delete_email_updating_state/1]).

% Tools
-export([print_table/1]).

-include("matcha.hrl").

-define(WRITE(Rec), mnesia:dirty_write(Rec)).
-define(READ(Tab, Key), mnesia:dirty_read(Tab, Key)).
-define(MATCH(Obj), mnesia:dirty_match_object(Obj)).

init() ->
    application:stop(mnesia),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(temp_account,
                        [{attributes, record_info(fields, temp_account)},
                         {disc_copies, [node()]}]),
    Ret = mnesia:create_table(session,
                        [{attributes, record_info(fields, session)},
                         {disc_copies, [node()]}]),
    io:format("RET: ~p~n", [Ret]),
    mnesia:create_table(password_recovering_state,
                        [{attributes, record_info(fields, password_recovering_state)},
                         {disc_copies, [node()]}]),
    mnesia:create_table(email_updating_state,
                        [{attributes, record_info(fields, email_updating_state)},
                         {disc_copies, [node()]}]),
    mnesia:wait_for_tables([temp_account, session, password_recovering_state, email_updating_state], 2000).

print_table(Table_name)->
  Iterator = fun(Rec, _)->
                 io:format("~p~n~n",[Rec]),
                 []
             end,
  case mnesia:is_transaction() of
      true ->
          mnesia:foldl(Iterator,[],Table_name);
      false ->
          Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
          mnesia:activity(transaction, Exec, [{Iterator,Table_name}], mnesia_frag)
  end.

create_temp_account(Uname, Fname, Lname, Pass, Email, Token) ->
    ?WRITE(#temp_account{uname = Uname, fname = Fname, lname = Lname,
                         password = Pass, email = Email, token = Token}).

get_temp_account(Token) -> read_one(temp_account, Token).

get_temp_account_by_email(Email) -> match_one(#temp_account{email = Email, _ = '_'}).

get_temp_account_by_uname(Uname) -> match_one(#temp_account{uname = Uname, _ = '_'}).

delete_temp_account(Token) -> delete(#temp_account{token = Token, _ = '_'}).

create_password_recovering_state(Id, Token) ->
    ?WRITE(#password_recovering_state{id = Id, token = Token}).

get_password_recovering_state(Token) ->
    match_one(#password_recovering_state{token = Token, _ = '_'}).

update_password_recovering_state(Prs, NewToken) ->
    ?WRITE(Prs#password_recovering_state{token = NewToken, stage = new_password}).

delete_password_recovering_state(Id) ->
    delete(#password_recovering_state{id = Id, _ = '_'}).


create_email_updating_state(UId, Email, Token) ->
    ?WRITE(#email_updating_state{uid = UId, email = Email, token = Token}).

get_email_updating_state(Token) ->
    match_one(#email_updating_state{token = Token, _ = '_'}).

delete_email_updating_state(Token) ->
    io:format("Token: ~p~n", [Token]),
    Res = delete(#email_updating_state{token = Token, _ = '_'}),
    io:format("RES: ~p~n", [Res]).


create_session(Uname, UId, Location, IsComplete) ->
    delete(#session{uid = UId, _ = '_'}),
    Token = matcha_lib:rand_str(16),
    ?WRITE(#session{id = Token, uname = Uname, uid = UId, is_complete = IsComplete, location = Location}),
    Token.

update_session(S, Uname, Interests, Location) ->
    ?WRITE(S#session{uname = Uname, interests = Interests, location = Location, is_complete = true}).

get_session(SID) when is_binary(SID) -> read_one(session, SID);
get_session(_) -> null.

delete_session(SID) -> delete(#session{id = SID, _ = '_'}).

read_one(Tab, Key) ->
    case ?READ(Tab, Key) of
        [O|_] -> O;
        [] -> null
    end.

match_one(Obj) ->
    case ?MATCH(Obj) of
        [O|_] -> O;
        [] -> null
    end.

delete(Obj) ->
    [mnesia:dirty_delete_object(X) || X <- mnesia:dirty_match_object(Obj)],
    ok.
