-module(cat_traveler_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("otp_specs.hrl").

-spec start(otp_app_start_type(), term()) -> otp_app_start_ret().
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


-spec stop(term()) -> term().
stop(_State) ->
    ok.
