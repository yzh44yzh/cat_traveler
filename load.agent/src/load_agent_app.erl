-module(load_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("otp_specs.hrl").


-spec start(otp_app_start_type(), term()) -> otp_app_start_ret().
start(_StartType, _StartArgs) ->
    load_agent_sup:start_link().


-spec stop(term()) -> term().
stop(_State) ->
    ok.
