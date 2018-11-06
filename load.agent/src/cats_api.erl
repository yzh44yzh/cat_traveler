-module(cats_api).

-export([enter_town/2, leave_town/2]).

-include("logger.hrl").

-spec enter_town(binary(), binary()) -> ok.
enter_town(CatName, Town) ->
    ?log_info("~s enters ~s", [CatName, Town]),
    Payload = <<
        "{\"cat\": \"", CatName/binary,
        "\", \"town\": \"", Town/binary, "\"}"
    >>,
    call(put, "/enter", Payload).


-spec leave_town(binary(), binary()) -> ok.
leave_town(CatName, Town) ->
    ?log_info("~s leaves ~s", [CatName, Town]),
    Payload = <<
        "{\"cat\": \"", CatName/binary,
        "\", \"town\": \"", Town/binary, "\"}"
    >>,
    call(put, "/leave", Payload).


-spec call(atom(), string(), binary()) -> ok.
call(Method, Url, Payload) ->
    {ok, API_URL} = application:get_env(load_agent, api_url),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(Method, API_URL ++ Url, Headers, Payload) of
        {ok, 200, _, ClientRef} ->
            hackney:skip_body(ClientRef),
            ok;
        {ok, Status, _, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            ?log_error("invalid request ~p ~p ~p ~p", [Url, Payload, Status, Body]);
        {error, Error} ->
            ?log_error("invalid request ~p ~p ~p", [Url, Payload, Error])
    end.
