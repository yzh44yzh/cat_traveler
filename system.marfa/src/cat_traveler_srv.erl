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
handle_call({enter, Cat, NewTown}, _From, State = #state{cats = Cats, towns = Towns}) ->
    case maps:find(NewTown, Towns) of
        {ok, CatsInNewTown} ->
            case maps:find(Cat, Cats) of
                {ok, NewTown} ->
                    {reply, {error, already_in_town}, State};

                {ok, OldTown} -> % move cat from OldTown to NewTown
                    {ok, CatsInOldTown} = maps:find(OldTown, Towns),
                    CatsInOldTown2 = sets:del_element(Cat, CatsInOldTown),
                    CatsInNewTown2 = sets:add_element(Cat, CatsInNewTown),
                    Towns2 = Towns#{
                        NewTown => CatsInNewTown2,
                        OldTown => CatsInOldTown2
                    },
                    Cats2 = Cats#{Cat => NewTown},
                    {reply, ok, State#state{cats = Cats2, towns = Towns2}};

                error -> % cat not in any town, just put it ot NewTown
                    CatsInNewTown2 = sets:add_element(Cat, CatsInNewTown),
                    Towns2 = Towns#{NewTown => CatsInNewTown2},
                    Cats2 = Cats#{Cat => NewTown},
                    {reply, ok, State#state{cats = Cats2, towns = Towns2}}
            end;
        error -> {reply, {error, invalid_town}, State}
    end;

handle_call({leave, Cat, Town}, _From, State = #state{cats = Cats, towns = Towns}) ->
    case maps:find(Town, Towns) of
        {ok, CatsInTown} ->
            case maps:find(Cat, Cats) of
                {ok, Town} ->
                    CatsInTown2 = sets:del_element(Cat, CatsInTown),
                    Towns2 = Towns#{Town => CatsInTown2},
                    Cats2 = maps:remove(Cat, Cats),
                    {reply, ok, State#state{cats = Cats2, towns = Towns2}};
                _ -> {reply, {error, not_in_town}, State}
            end;
        error -> {reply, {error, invalid_town}, State}
    end;

handle_call({dwell, Cat, Town}, _From, State = #state{towns = Towns}) ->
    case maps:find(Town, Towns) of
        {ok, CatsInTown} ->
            Reply = sets:is_element(Cat, CatsInTown),
            {reply, {ok, Reply}, State};
        error -> {reply, {error, invalid_town}, State}
    end;

handle_call({where_is_cat, Cat}, _From, State = #state{cats = Cats}) ->
    case maps:find(Cat, Cats) of
        {ok, Town} -> {reply, {ok, Town}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({who_is_in_town, Town}, _From, State = #state{towns = Towns}) ->
    case maps:find(Town, Towns) of
        {ok, CatsInTown} ->
            Cats = sets:to_list(CatsInTown),
            {reply, {ok, Cats}, State};
        error -> {reply, {error, invalid_town}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_call}, State}.


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

