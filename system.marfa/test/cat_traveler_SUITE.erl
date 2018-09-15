-module(cat_traveler_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-spec all() -> [atom()].
all() ->
    [
        enter_test,
        leave_test,
        move_test,
        error_paths_test
    ].


init_per_testcase(_Test, Config) ->
    {ok, _} = application:ensure_all_started(cat_traveler),
    Config.


end_per_testcase(_Test, Config) ->
    ok = application:stop(cat_traveler),
    ok = application:stop(cowboy),
    ok = application:stop(ranch),
    Config.


enter_test(_Config) ->
    Minsk = cat_traveler_types:new_town(<<"Minsk">>),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Minsk)),

    Barcelona = cat_traveler_types:new_town(<<"Barcelona">>),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Barcelona)),

    Tihon = cat_traveler_types:new_cat(<<"Tihon">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({error, not_found}, cat_traveler:where_is_cat(Tihon)),

    Marfa = cat_traveler_types:new_cat(<<"Marfa">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({error, not_found}, cat_traveler:where_is_cat(Marfa)),

    ok = cat_traveler:enter(Tihon, Minsk),
    ?assertEqual({error, already_in_town}, cat_traveler:enter(Tihon, Minsk)),
    ?assertEqual({ok, [Tihon]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Barcelona)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Tihon)),
    ?assertEqual({error, not_found}, cat_traveler:where_is_cat(Marfa)),

    ok = cat_traveler:enter(Marfa, Barcelona),
    ?assertEqual({error, already_in_town}, cat_traveler:enter(Marfa, Barcelona)),
    ?assertEqual({ok, [Tihon]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, [Marfa]}, cat_traveler:who_is_in_town(Barcelona)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Tihon)),
    ?assertEqual({ok, Barcelona}, cat_traveler:where_is_cat(Marfa)),
    ok.


leave_test(_Config) ->
    Minsk = cat_traveler_types:new_town(<<"Minsk">>),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Minsk)),
    Tihon = cat_traveler_types:new_cat(<<"Tihon">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    Marfa = cat_traveler_types:new_cat(<<"Marfa">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),

    ok = cat_traveler:enter(Tihon, Minsk),
    ?assertEqual({ok, [Tihon]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),

    ok = cat_traveler:enter(Marfa, Minsk),
    {ok, Cats} = cat_traveler:who_is_in_town(Minsk),
    ?assert(lists:member(Tihon, Cats)),
    ?assert(lists:member(Marfa, Cats)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Minsk)),

    ok = cat_traveler:leave(Tihon, Minsk),
    ?assertEqual({error, not_in_town}, cat_traveler:leave(Tihon, Minsk)),
    ?assertEqual({ok, [Marfa]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Minsk)),

    ok = cat_traveler:leave(Marfa, Minsk),
    ?assertEqual({error, not_in_town}, cat_traveler:leave(Marfa, Minsk)),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),
    ok.


move_test(_Config) ->
    Minsk = cat_traveler_types:new_town(<<"Minsk">>),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Minsk)),
    Barcelona = cat_traveler_types:new_town(<<"Barcelona">>),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Barcelona)),

    Tihon = cat_traveler_types:new_cat(<<"Tihon">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    Marfa = cat_traveler_types:new_cat(<<"Marfa">>),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),

    ok = cat_traveler:enter(Tihon, Minsk),
    ok = cat_traveler:enter(Marfa, Minsk),
    {ok, Cats} = cat_traveler:who_is_in_town(Minsk),
    ?assert(lists:member(Tihon, Cats)),
    ?assert(lists:member(Marfa, Cats)),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Barcelona)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Tihon)),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Marfa)),

    ok = cat_traveler:enter(Tihon, Barcelona),
    ?assertEqual({ok, [Marfa]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({ok, [Tihon]}, cat_traveler:who_is_in_town(Barcelona)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({ok, Barcelona}, cat_traveler:where_is_cat(Tihon)),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Marfa)),

    ok = cat_traveler:enter(Marfa, Barcelona),
    ?assertEqual({ok, []}, cat_traveler:who_is_in_town(Minsk)),
    {ok, Cats2} = cat_traveler:who_is_in_town(Barcelona),
    ?assert(lists:member(Tihon, Cats2)),
    ?assert(lists:member(Marfa, Cats2)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({ok, false}, cat_traveler:dwell(Marfa, Minsk)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Tihon, Barcelona)),
    ?assertEqual({ok, true}, cat_traveler:dwell(Marfa, Barcelona)),
    ?assertEqual({ok, Barcelona}, cat_traveler:where_is_cat(Tihon)),
    ?assertEqual({ok, Barcelona}, cat_traveler:where_is_cat(Marfa)),
    ok.


error_paths_test(_Config) ->
    Minsk = cat_traveler_types:new_town(<<"Minsk">>),
    Pinsk = cat_traveler_types:new_town(<<"Pinsk">>),
    Tihon = cat_traveler_types:new_cat(<<"Tihon">>),

    ?assertEqual(ok, cat_traveler:enter(Tihon, Minsk)),
    ?assertEqual({error, already_in_town}, cat_traveler:enter(Tihon, Minsk)),
    ?assertEqual({error, invalid_town}, cat_traveler:enter(Tihon, Pinsk)),

    ?assertEqual(ok, cat_traveler:leave(Tihon, Minsk)),
    ?assertEqual({error, not_in_town}, cat_traveler:leave(Tihon, Minsk)),
    ?assertEqual({error, invalid_town}, cat_traveler:leave(Tihon, Pinsk)),

    ?assertEqual({ok, false}, cat_traveler:dwell(Tihon, Minsk)),
    ?assertEqual({error, invalid_town}, cat_traveler:dwell(Tihon, Pinsk)),

    ?assertEqual({error, not_found}, cat_traveler:where_is_cat(Tihon)),
    ok = cat_traveler:enter(Tihon, Minsk),
    ?assertEqual({ok, Minsk}, cat_traveler:where_is_cat(Tihon)),

    ?assertEqual({ok, [Tihon]}, cat_traveler:who_is_in_town(Minsk)),
    ?assertEqual({error, invalid_town}, cat_traveler:who_is_in_town(Pinsk)),
    ok.
