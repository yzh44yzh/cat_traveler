-module(cat_traveler).

%% Attention! Before using this API you should listen
%% A Cool Cat in Town. TAPE FIVE.
%% https://www.youtube.com/watch?v=qCRO964l_nE

-export([
    enter/2,
    leave/2,
    dwell/2,
    where_is_cat/1,
    who_is_in_town/1
]).

-include("cat_traveler_types.hrl").

-define(srv, cat_traveler_srv).


-spec enter(cat(), town()) -> ok | {error, already_in_town} | {error, invalid_town}.
enter(Cat, Town) ->
    gen_server:call(?srv, {enter, Cat, Town}).


-spec leave(cat(), town()) -> ok | {error, not_in_town} | {error, invalid_town}.
leave(Cat, Town) ->
    gen_server:call(?srv, {leave, Cat, Town}).


-spec dwell(cat(), town()) -> {ok, boolean()} | {error, invalid_town}.
dwell(Cat, Town) ->
    gen_server:call(?srv, {dwell, Cat, Town}).


-spec where_is_cat(cat()) -> {ok, town()} | {error, not_found}.
where_is_cat(Cat) ->
    gen_server:call(?srv, {where_is_cat, Cat}).


-spec who_is_in_town(town()) -> {ok, [cat()]} | {error, invalid_town}.
who_is_in_town(Town) ->
    gen_server:call(?srv, {who_is_in_town, Town}).
