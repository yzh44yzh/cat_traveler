-module(cat_traveler_app).
-behaviour(application).

-export([start/2, stop/1]).


-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).

start(_StartType, _StartArgs) ->
    %% TODO get towns from config
    Towns = [
        {town, <<"Minsk">>},
        {town, <<"Barcelona">>},
        {town, <<"Praha">>},
        {town, <<"Munchen">>},
        {town, <<"Amsterdam">>},
        {town, <<"Stockholm">>}
    ],
    cat_traveler_sup:start_link(Towns).


-spec(stop(State :: term()) -> term()).

stop(_State) ->
    ok.
