-module(matcha_ws).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("matcha.hrl").

-record(state, {uid :: non_neg_integer()}).

init(Req, State) ->
	io:format("CONNECTION ...~n"),
  	case matcha_http:get_session(Req) of
		#session{is_complete = true, uid = UId} ->
			io:format("ACCEPTED~n"),
            {cowboy_websocket, Req, #state{uid = UId}};
        _ ->
            cowboy_req:reply(403, Req),
            {ok, Req, State}
	end.

websocket_init(#state{uid = UId} = S) ->
	io:format("INIT~n"),
    matcha_lib:subscribe(UId),
	matcha_db:update_last_seen_at(UId),
	{ok, S}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};

websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(Msg, State) ->
	io:format("msg: ~p~n", [Msg]),
	{reply, {text, jsone:encode(Msg)}, State}.

terminate(_Reason, _Req, #state{uid = UId}) ->
    matcha_db:update_last_seen_at(UId),
	ok.
