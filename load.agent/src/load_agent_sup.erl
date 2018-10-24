-module(load_agent_sup).
-behaviour(supervisor).

-export([start_link/0, run_agent/2, init/1]).

-include("otp_specs.hrl").

-spec start_link() -> otp_sup_start_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec run_agent(binary(), [binary()]) -> supervisor:startchild_ret().
run_agent(Name, Towns) ->
    supervisor:start_child(?MODULE, [Name, Towns]).


-spec init(list()) -> otp_sup_init_ret().
init([]) ->
    Spec = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one
        intensity => 10, % max restarts
        period => 1000 % in period of time
    },
    Childs =
        [
            #{
                id => cat_agent,
                start => {cat_agent, start_link, []},
                restart => transient, % permanent | transient | temporary
                shutdown => 2000, % milliseconds | brutal_kill | infinity
                type => worker, % worker | supervisor
                modules => [cat_agent]
            }
        ],
    {ok, {Spec, Childs}}.
