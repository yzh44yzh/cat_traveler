-module(ct_handler_api).
-behaviour(cowboy_handler).

-export([init/2]).

-include("cat_traveler_types.hrl").

-type method() :: binary().
-type path() :: binary().
-type status_code() :: non_neg_integer().


%%% cowboy_handler behaviour

-spec init(cowboy_req:req(), no_state) -> {ok, cowboy_req:req(), no_state}.
init(#{method := Method, path := Path} = Req0, no_state) ->
    {StatusCode, Reply} = handle(Method, Path, Req0),
    ReplyHeaders = #{
        <<"content-type">> => <<"application/json">>
    },
    Req1 = cowboy_req:reply(StatusCode, ReplyHeaders, jsx:encode(Reply), Req0),
    {ok, Req1, no_state}.


%%% Handle

-spec handle(method(), path(), cowboy_req:req()) -> {status_code(), json()}.
handle(<<"PUT">>, <<"/enter">>, Req) ->
    case decode_body(Req) of
        #{<<"cat">> := CatName, <<"town">> := TownName} ->
            Cat = cat_traveler_types:new_cat(CatName),
            Town = cat_traveler_types:new_town(TownName),
            case cat_traveler:enter(Cat, Town) of
                ok ->
                    {200, #{
                        <<"message">> => format("~s has entered ~s", [CatName, TownName])
                    }};
                {error, Reason} -> {400, #{<<"error">> => Reason}}
            end;
        Data ->
            warning("/enter got invalid data:~p", [Data]),
            {400, #{<<"error">> => <<"invalid data">>}}
    end;

handle(<<"PUT">>, <<"/leave">>, Req) ->
    case decode_body(Req) of
        #{<<"cat">> := CatName, <<"town">> := TownName} ->
            Cat = cat_traveler_types:new_cat(CatName),
            Town = cat_traveler_types:new_town(TownName),
            case cat_traveler:leave(Cat, Town) of
                ok ->
                    {200, #{
                        <<"message">> => format("~s has leaved ~s", [CatName, TownName])
                    }};
                {error, Reason} -> {400, #{<<"error">> => Reason}}
            end;
        Data ->
            warning("/leave got invalid data:~p", [Data]),
            {400, #{<<"error">> => <<"invalid data">>}}
    end;

handle(<<"GET">>, <<"/dwell">>, Req) ->
    Qs = cowboy_req:parse_qs(Req),
    MaybeCatName = proplists:get_value(<<"cat">>, Qs),
    MaybeTownName = proplists:get_value(<<"town">>, Qs),
    case {MaybeCatName, MaybeTownName} of
        {undefined, _} ->
            {400, #{<<"error">> => <<"invalid data">>}};
        {_, undefined} ->
            {400, #{<<"error">> => <<"invalid data">>}};
        {CatName, TownName} ->
            Cat = cat_traveler_types:new_cat(CatName),
            Town = cat_traveler_types:new_town(TownName),
            case cat_traveler:dwell(Cat, Town) of
                {ok, Reply} -> {200, #{<<"reply">> => Reply}};
                {error, Reason} -> {400, #{<<"error">> => Reason}}
            end
    end;

handle(<<"GET">>, <<"/where_is_cat">>, Req) ->
    Qs = cowboy_req:parse_qs(Req),
    MaybeCatName = proplists:get_value(<<"cat">>, Qs),
    case MaybeCatName of
        undefined ->
            {400, #{<<"error">> => <<"invalid data">>}};
        CatName ->
            Cat = cat_traveler_types:new_cat(CatName),
            case cat_traveler:where_is_cat(Cat) of
                {ok, Town} -> {200, #{<<"reply">> => cat_traveler_types:town_to_json(Town)}};
                {error, Reason} -> {400, #{<<"error">> => Reason}}
            end
    end;

handle(<<"GET">>, <<"/who_is_in_town">>, Req) ->
    Qs = cowboy_req:parse_qs(Req),
    MaybeTownName = proplists:get_value(<<"town">>, Qs),
    case MaybeTownName of
        undefined ->
            {400, #{<<"error">> => <<"invalid data">>}};
        TownName ->
            Town = cat_traveler_types:new_town(TownName),
            case cat_traveler:who_is_in_town(Town) of
                {ok, Cats} ->
                    Reply = lists:map(fun cat_traveler_types:cat_to_json/1, Cats),
                    {200, #{<<"reply">> => Reply}};
                {error, Reason} -> {400, #{<<"error">> => Reason}}
            end
    end;

handle(_Method, _Path, _Req) ->
    {400, #{<<"error">> => <<"invalid query">>}}.


%%% Inner Functions

-spec decode_body(cowboy_req:req()) -> json().
decode_body(Req) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    jsx:decode(Body, [return_maps]).


-spec format(string(), list()) -> binary().
format(FormatStr, Args) ->
    unicode:characters_to_binary(io_lib:format(FormatStr, Args)).


-spec warning(string(), list()) -> ok.
warning(FormatStr, Args) ->
    io:format("WARN: " ++ FormatStr ++ "\n", Args).
