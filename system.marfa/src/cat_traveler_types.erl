-module(cat_traveler_types).

-export([
    new_cat/1,
    cat_to_json/1,
    new_town/1,
    town_to_json/1
]).

-include("cat_traveler_types.hrl").


-spec new_cat(binary()) -> cat().
new_cat(Name) -> {cat, Name}.


-spec cat_to_json(cat()) -> json().
cat_to_json({cat, Name}) ->
    #{<<"cat">> => Name}.


-spec new_town(binary()) -> town().
new_town(Name) -> {town, Name}.


-spec town_to_json(town()) -> json().
town_to_json({town, Name}) ->
    #{<<"town">> => Name}.