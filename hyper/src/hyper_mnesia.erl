-module(hyper_mnesia).
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
         create_email_updating_state/3,
         get_email_updating_state/1,
         delete_email_updating_state/1]).

% Tools
-export([print_table/1]).

-include("hyper.hrl").

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
    mnesia:create_table(password_recovering_state,
                        [{attributes, record_info(fields, password_recovering_state)},
                         {disc_copies, [node()]}]),
    mnesia:create_table(email_updating_state,
                        [{attributes, record_info(fields, email_updating_state)},
                         {disc_copies, [node()]}]),
    mnesia:wait_for_tables([temp_account, password_recovering_state, email_updating_state], 2000).

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
    delete(#email_updating_state{token = Token, _ = '_'}).


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
