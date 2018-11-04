-module(hyper_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 1, 5}, [#{id => mnesia_reaper, start => {hyper_mnesia, start_reaper, []}}]}}.
