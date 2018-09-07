-module(ct_handler_api).

-export([init/2]).

-spec init(cowboy_req:req(), no_state) -> {ok, cowboy_req:req(), no_state}.
init(#{method := Method, path := Path} = Req0, no_state) ->
    QsVals = cowboy_req:parse_qs(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    io:format("Method:~p, Path:~p, QsVals:~p, Body:~p~n", [Method, Path, QsVals, Body]),
    Headers = #{
        <<"content-type">> => <<"application/json">>
    },
    Reply = #{
        <<"answer">> => 42
    },
    Req2 = cowboy_req:reply(200, Headers, jsx:encode(Reply), Req1),
    {ok, Req2, no_state}.