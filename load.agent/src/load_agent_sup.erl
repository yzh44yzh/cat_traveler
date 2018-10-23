-module(load_agent_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("otp_specs.hrl").

-spec start_link() -> otp_sup_start_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init(list()) -> otp_sup_init_ret().
init([]) ->
    Spec = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one
        intensity => 10, % max restarts
        period => 1000 % in period of time
    },
    Childs =
        [
%%            #{
%%                id => cat_traveler_srv,
%%                start => {cat_traveler_srv, start_link, [Towns]},
%%                restart => permanent, % permanent | transient | temporary
%%                shutdown => 2000, % milliseconds | brutal_kill | infinity
%%                type => worker, % worker | supervisor
%%                modules => [cat_traveler_srv]
%%            }
        ],
    {ok, {Spec, Childs}}.
