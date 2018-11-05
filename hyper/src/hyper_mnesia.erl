-module(hyper_mnesia).
-author("aklymchuk").

-behaviour(gen_server).

%% reaper API
-export([start_reaper/0]).

%% API
-export([init_tables/0,
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

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(REAP_INTERVAL, 1000 * 20). % 20 sec
-define(TEMP_ACCOUNT_TTL, 60 * 1). % 1 min
-define(EMAIL_UPDATING_STATE_TTL, 60 * 1). % 1 min
-define(PASSWORD_RECOVERING_STATE_TTL, 60 * 1). % 1 min

-include("hyper.hrl").

init_tables() ->
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
    write(#temp_account{uname = Uname, fname = Fname, lname = Lname, password = Pass, email = Email, token = Token}).

get_temp_account(Token) ->
    read_one(temp_account, Token).

get_temp_account_by_email(Email) ->
    match_one(#temp_account{email = Email, _ = '_'}).

get_temp_account_by_uname(Uname) ->
    match_one(#temp_account{uname = Uname, _ = '_'}).

delete_temp_account(Token) ->
    match_delete(#temp_account{token = Token, _ = '_'}).

create_password_recovering_state(Id, Token) ->
    write(#password_recovering_state{id = Id, token = Token}).

get_password_recovering_state(Token) ->
    match_one(#password_recovering_state{token = Token, _ = '_'}).

update_password_recovering_state(Prs, NewToken) ->
    write(Prs#password_recovering_state{token = NewToken, stage = new_password}).

delete_password_recovering_state(Id) ->
    match_delete(#password_recovering_state{id = Id, _ = '_'}).

create_email_updating_state(UId, Email, Token) ->
    write(#email_updating_state{uid = UId, email = Email, token = Token}).

get_email_updating_state(Token) ->
    match_one(#email_updating_state{token = Token, _ = '_'}).

delete_email_updating_state(Token) ->
    match_delete(#email_updating_state{token = Token, _ = '_'}).

%% reaper API

start_reaper() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server callbacks

init([]) ->
    erlang:send_after(?REAP_INTERVAL, self(), reap_old_records),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(reap_old_records, State) ->
    delete_old(email_updating_state),
    delete_old(password_recovering_state),
    delete_old(temp_account),
    erlang:send_after(?REAP_INTERVAL, self(), reap_old_records),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

read_one(Tab, Key) ->
    case mnesia:dirty_read(Tab, Key) of
        [O | _] -> O;
        [] -> null
    end.

match_one(Obj) ->
    case mnesia:dirty_match_object(Obj) of
        [O | _] -> O;
        [] -> null
    end.

write(Rec) ->
    mnesia:dirty_write(Rec).

match_delete(Obj) ->
    [mnesia:dirty_delete_object(X) || X <- mnesia:dirty_match_object(Obj)],
    ok.

delete(Tab, Keys) when is_list(Keys) ->
    [mnesia:dirty_delete(Tab, K) || K <- Keys],
    ok.

delete_old(email_updating_state) ->
    Keys = mnesia:dirty_select(email_updating_state,[{#email_updating_state{token = '$1', created_at = '$2', _ = '_'},
                                                     [{'<', '$2', ?NOW_SEC - ?EMAIL_UPDATING_STATE_TTL}], ['$1']}]),
    delete(email_updating_state, Keys);
delete_old(password_recovering_state) ->
    Keys = mnesia:dirty_select(password_recovering_state,
				[{#password_recovering_state{id = '$1', created_at = '$2', _ = '_'},
                                 [{'<', '$2', ?NOW_SEC - ?PASSWORD_RECOVERING_STATE_TTL}], ['$1']}]),
    delete(password_recovering_state, Keys);
delete_old(temp_account) ->
    Keys = mnesia:dirty_select(temp_account,[{#temp_account{token = '$1', created_at = '$2', _ = '_'},
                                             [{'<', '$2', ?NOW_SEC - ?EMAIL_UPDATING_STATE_TTL}], ['$1']}]),
    delete(temp_account, Keys).

