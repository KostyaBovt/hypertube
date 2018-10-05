-module(hyper).
-author("aklimchu").

%% API
-export([start/0]).

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(hyper),
    ok.
    