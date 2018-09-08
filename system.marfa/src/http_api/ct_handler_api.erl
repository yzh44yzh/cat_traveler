-module(ct_handler_api).

-export([init/2]).

-type method() :: binary().
-type path() :: binary().
-type status_code() :: non_neg_integer().
-type json() :: jsx:json_term().


-spec init(cowboy_req:req(), no_state) -> {ok, cowboy_req:req(), no_state}.
init(#{method := Method, path := Path} = Req0, no_state) ->
    {StatusCode, Reply} = handle(Method, Path, Req0),
    ReplyHeaders = #{
        <<"content-type">> => <<"application/json">>
    },
    Req1 = cowboy_req:reply(StatusCode, ReplyHeaders, jsx:encode(Reply), Req0),
    {ok, Req1, no_state}.


-spec handle(method(), path(), cowboy_req:req()) -> {status_code(), json()}.
handle(<<"PUT">>, <<"/enter">>, Req) ->
    % QsVals = cowboy_req:parse_qs(Req0),
    case decode_body(Req) of
        #{<<"cat">> := CatName, <<"town">> := TownName} ->
            Cat = cat_traveler:new_cat(CatName),
            Town = cat_traveler:new_town(TownName),
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

%%    Method:<<"PUT">>, Path:<<"/leave">>, QsVals:[],
%%        Body:<<"{\"cat\": \"Tihon\", \"town\": \"Minsk\"}">>
%%    Method:<<"GET">>, Path:<<"/dwell">>, QsVals:[{<<"cat">>,<<"Tihon">>},
%%        {<<"town">>,<<"Minsk">>}], Body:<<>>
%%    Method:<<"GET">>, Path:<<"/where_is_cat">>, QsVals:[{<<"cat">>,<<"Tihon">>}], Body:<<>>
%%    Method:<<"GET">>, Path:<<"/who_is_in_town">>, QsVals:[{<<"town">>,
%%        <<"Minsk">>}], Body:<<>>

handle(_Method, _Path, _Req) ->
    {400, #{<<"error">> => <<"invalid query">>}}.


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
