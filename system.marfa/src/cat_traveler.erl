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


-spec enter(cat(), town()) -> ok | {error, already_in_town}.
enter(_Cat, _Town) ->
    {error, already_in_town}.


-spec leave(cat(), town()) -> ok | {error, not_in_town}.
leave(_Cat, _Town) ->
    {error, not_in_town}.


-spec dwell(cat(), town()) -> boolean().
dwell(_Cat, _Town) ->
    false.


-spec where_is_cat(cat()) -> town() | {error, not_found}.
where_is_cat(_Cat) ->
    {error, not_found}.


-spec who_is_in_town(town()) -> [cat()].
who_is_in_town(_Town) ->
    [].
