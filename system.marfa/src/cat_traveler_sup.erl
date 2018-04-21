-module(cat_traveler_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("cat_traveler_types.hrl").
-include("otp_specs.hrl").


-spec start_link([town()]) -> otp_sup_start_ret().
start_link(Towns) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Towns).


-spec init([town()]) -> otp_sup_init_ret().
init(Towns) ->
    Spec = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one
        intensity => 10, % max restarts
        period => 1000 % in period of time
    },
    Childs =
        [
            #{
                id => cat_traveler_srv,
                start => {cat_traveler_srv, start_link, [Towns]},
                restart => permanent, % permanent | transient | temporary
                shutdown => 2000, % milliseconds | brutal_kill | infinity
                type => worker, % worker | supervisor
                modules => [cat_traveler_srv]
            }
        ],
    {ok, {Spec, Childs}}.

