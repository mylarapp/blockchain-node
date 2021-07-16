-module(bn_info).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

handle_rpc(<<"info">>, _) ->
    {ok, LocalHeight} = blockchain:height(blockchain_worker:blockchain()),
    upstream_data(#{local_height => LocalHeight});

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

upstream_data(#{local_height := LocalHeight} = Result) ->
    Url = hackney_url:make_url("https://api.helium.io", ["v1", "blocks", "height"], []),
    case fetched_upstream_data(hackney:get(Url, [])) of
        {ok, Height} ->
            Result#{upstream_height => Height, difference => LocalHeight - Height};
        {error, Reason} ->
            Result#{error => Reason}
    end.

fetched_upstream_data({ok, 200, _Headers, ClientRef}) ->
    {ok, Body} = hackney:body(ClientRef),
    #{<<"data">> := #{<<"height">> := Height}} = jsone:decode(Body, []),
    {ok, Height};
fetched_upstream_data({ok, StatusCode, _, _}) ->
    lager:info("Failed to height from api.helium.io: ~p~n", [StatusCode]),
    {error, failed_lookup}.
