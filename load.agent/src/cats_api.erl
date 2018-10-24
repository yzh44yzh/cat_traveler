-module(cats_api).

-export([enter_town/2, leave_town/2]).


-spec enter_town(binary(), binary()) -> ok.
enter_town(CatName, Town) ->
    io:format("~p enters ~p~n", [CatName, Town]),
    %% TODO api call
    ok.

-spec leave_town(binary(), binary()) -> ok.
leave_town(CatName, Town) ->
    io:format("~p leaves ~p~n", [CatName, Town]),
    %% TODO api call
    ok.