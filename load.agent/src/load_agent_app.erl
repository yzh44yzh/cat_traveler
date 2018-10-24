-module(load_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("otp_specs.hrl").


-spec start(otp_app_start_type(), term()) -> otp_app_start_ret().
start(_StartType, _StartArgs) ->
    Res = load_agent_sup:start_link(),
    run_agents(),
    Res.


-spec stop(term()) -> term().
stop(_State) ->
    ok.


-spec run_agents() -> ok.
run_agents() ->
    load_agent_sup:run_agent(<<"Tihon">>, [<<"Minsk">>, <<"Barcelona">>]),
    load_agent_sup:run_agent(<<"Marfa">>, [<<"Minsk">>, <<"Munchen">>, <<"Amsterdam">>]),
    load_agent_sup:run_agent(<<"Murka">>, [<<"Stockholm">>, <<"Minsk">>, <<"Praha">>]),
    load_agent_sup:run_agent(<<"Vasjka">>, [<<"Barcelona">>, <<"Minsk">>, <<"Amsterdam">>]),
    ok.

