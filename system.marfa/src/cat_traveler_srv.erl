-module(cat_traveler_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("cat_traveler_types.hrl").
-include("otp_specs.hrl").

-record(state, {
    cats = #{} :: #{cat() => town()},
    towns = #{} :: #{town() => sets:set(cat())}
}).


-spec start_link([town()]) -> otp_gen_srv_start_ret().
start_link(Towns) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Towns, []).


%%% gen_server callbacks

-spec init([town()]) -> otp_gen_srv_init_ret().
init(TownsList) ->
    TownsMap = maps:from_list(
        lists:map(
            fun(Town) -> {Town, sets:new()} end,
            TownsList
        )),
    {ok, #state{towns = TownsMap}}.


-spec handle_call(term(), {pid(), term()}, #state{}) -> otp_gen_srv_call_ret().
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(term(), #state{}) -> otp_gen_srv_cast_ret().
handle_cast(_Request, State) ->
    {noreply, State}.


-spec handle_info(term(), #state{}) -> otp_gen_srv_info_ret().
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(term(), #state{}) -> term().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

