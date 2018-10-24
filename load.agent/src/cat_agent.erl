-module(cat_agent).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("otp_specs.hrl").

-define(min_wait, 2000).
-define(max_wait, 7000).

-record(state, {
    name :: binary(),
    curr_town :: binary(),
    towns :: [binary()]
}).


-spec start_link(binary(), [binary()]) -> otp_gen_srv_start_ret().
start_link(Name, Towns) ->
    gen_server:start_link(?MODULE, {Name, Towns}, []).


%%% gen_server callbacks

-spec init({binary(), [binary()]}) -> otp_gen_srv_init_ret().
init({Name, Towns}) ->
    schedule_acting(),
    {ok, #state{
        name = Name,
        towns = Towns
    }}.


-spec handle_call(term(), {pid(), term()}, #state{}) -> otp_gen_srv_call_ret().
handle_call(_Request, _From, State) ->
    {reply, {error, invalid_call}, State}.


-spec handle_cast(term(), #state{}) -> otp_gen_srv_cast_ret().
handle_cast(_Request, State) ->
    {noreply, State}.


-spec handle_info(term(), #state{}) -> otp_gen_srv_info_ret().
handle_info(act, State = #state{
    name = Name,
    curr_town = CurrTown,
    towns = Towns
}) ->
    State2 =
        case CurrTown of
            undefined ->
                Town = get_random(Towns),
                cats_api:enter_town(Name, Town),
                State#state{curr_town = Town};
            Town ->
                cats_api:leave_town(Name, Town),
                State#state{curr_town = undefined}
        end,
    schedule_acting(),
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(term(), #state{}) -> term().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

-spec schedule_acting() -> reference().
schedule_acting() ->
    T = rand:uniform(?max_wait - ?min_wait) + ?min_wait,
    erlang:send_after(T, self(), act).


-spec get_random([binary()]) -> binary().
get_random(List) ->
    N = rand:uniform(length(List)),
    lists:nth(N, List).