-module(cat_traveler_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("otp_specs.hrl").

-spec start(otp_app_start_type(), term()) -> otp_app_start_ret().
start(_StartType, _StartArgs) ->
    start_cowboy(),
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


-spec start_cowboy() -> ok.
start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/ping", ct_handler_ping, no_state}]}
    ]),
    {ok, _} = cowboy:start_clear(ct_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.