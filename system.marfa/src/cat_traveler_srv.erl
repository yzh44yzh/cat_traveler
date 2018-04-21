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

-record(state, {
    cats = #{} :: #{cat() => town()},
    towns = #{} :: #{town() => sets:set(cat())}
}).


-spec(start_link([town()]) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Towns) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Towns, []).



%%% gen_server callbacks

-spec(init(Towns :: [town()]) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init(TownsList) ->
    TownsMap = maps:from_list(
        lists:map(
            fun(Town) -> {Town, sets:new()} end,
            TownsList
        )),
    {ok, #state{towns = TownsMap}}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
    {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info(_Info, State) ->
    {noreply, State}.


-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, _State) ->
    ok.


-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Internal functions

