-module(ct_handler_ping).

-export([init/2]).

-spec init(cowboy_req:req(), no_state) -> {ok, cowboy_req:req(), no_state}.
init(Req0, no_state) ->
    Headers = #{
        <<"content-type">> => <<"application/json">>
    },
    {ok, Version} = application:get_key(cat_traveler, vsn),
    Reply = #{
        <<"version">> => list_to_binary(Version),
        <<"node">> => node(),
        <<"connected">> => nodes()
    },
    Req = cowboy_req:reply(200, Headers, jsx:encode(Reply), Req0),
    {ok, Req, no_state}.