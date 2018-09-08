-module(cat_traveler_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("otp_specs.hrl").

-spec start(otp_app_start_type(), term()) -> otp_app_start_ret().
start(_StartType, _StartArgs) ->
    start_cowboy(),
    {ok, Towns} = application:get_env(towns),
    cat_traveler_sup:start_link(Towns).


-spec stop(term()) -> term().
stop(_State) ->
    ok.


-spec start_cowboy() -> ok.
start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/enter", ct_handler_api, no_state},
            {"/leave", ct_handler_api, no_state},
            {"/dwell", ct_handler_api, no_state},
            {"/where_is_cat", ct_handler_api, no_state},
            {"/who_is_in_town", ct_handler_api, no_state},
            {"/ping", ct_handler_ping, no_state}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(ct_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.