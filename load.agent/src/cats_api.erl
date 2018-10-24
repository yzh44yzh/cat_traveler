-module(cats_api).

-export([enter_town/2, leave_town/2]).

-define(URL, "http://localhost:8080"). % TODO move to configuration

-spec enter_town(binary(), binary()) -> ok.
enter_town(CatName, Town) ->
    io:format("~p enters ~p~n", [CatName, Town]), % TODO ?log_info
    Payload = <<
        "{\"cat\": \"", CatName/binary,
        "\", \"town\": \"", Town/binary, "\"}"
    >>,
    call(put, ?URL ++ "/enter", Payload).


-spec leave_town(binary(), binary()) -> ok.
leave_town(CatName, Town) ->
    io:format("~p leaves ~p~n", [CatName, Town]),
    Payload = <<
        "{\"cat\": \"", CatName/binary,
        "\", \"town\": \"", Town/binary, "\"}"
    >>,
    call(put, ?URL ++ "/leave", Payload).


-spec call(atom(), string(), binary()) -> ok.
call(Method, Url, Payload) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, Url, Headers, Payload) of
        {ok, 200, _, _} -> ok;
        {ok, Status, _, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            io:format("~p ~p ~p ~p~n", [Url, Payload, Status, Body])
    end.
