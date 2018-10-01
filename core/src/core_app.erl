-module(core_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Paths = [{"/api/[...]", matcha_http, []},
             {"/ws", matcha_ws, []},
             {"/static/[...]", cowboy_static, {priv_dir, matcha, "./static/"}},
             {"/[...]", cowboy_static, {priv_file, matcha, "static/index.html"}}],
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    {ok, _} = cowboy:start_clear(matcha_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    matcha_mnesia:init(),
    matcha_validation:load(),
    pgapp:connect([{size, 10}, {database, "vagrant"}, {username, "vagrant"}, {password, "12345"}, {port, 5432}]),
    matcha_sup:start_link().

stop(_State) ->
	ok.

