-module(cat_traveler_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cat_traveler_sup:start_link().

stop(_State) ->
    ok.
