-module(bn_peer).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-define(EPOCH, calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).

-export([handle_rpc/2]).

handle_rpc(<<"peer">>, {Param}) ->
    B58Address = binary_to_list(?jsonrpc_get_param(<<"address">>, Param, undefined)),
    {ok, AnimalName} = erl_angry_purple_tiger:animal_name(B58Address),
    lager:info("~p (~p)", [B58Address, AnimalName]),

    Result0 = #{
        address => list_to_binary(B58Address),
        name => list_to_binary(AnimalName),
        errors => []
    },

    Result1 = gateway_data(Result0, B58Address),
    Result2 = maker_data(Result1, B58Address),
    Result3 = stale_peer_data(Result2, B58Address),
    Result4 = peer_data(Result3, B58Address),
    Result5 = ip_data(Result4, B58Address),
    Result6 = distance(Result5),

    Result6;

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

distance(#{
        ip_info := #{ lat := IpLat, lng := IpLng} = IpInfo,
        asserted_location := #{ lat := AssertLat, lng := AssertLng}} = Result) ->
    {ok, Distance} = vincenty:distance({IpLat, IpLng}, {AssertLat, AssertLng}),
    Result#{ ip_info => IpInfo#{ distance => Distance } };
distance(Result) ->
    Result.

gateway_data(Result, B58Address) ->
    Address = libp2p_crypto:b58_to_bin(B58Address),

    case blockchain_ledger_v1:find_gateway_info(Address, blockchain:ledger()) of
        {ok, Gateway} ->
            case blockchain_ledger_gateway_v2:location(Gateway) of
                undefined ->
                    Result#{errors => [not_asserted|maps:get(errors, Result)]};
                Location ->
                    {Lat, Lng} = h3:to_geo(Location),
                    Result#{
                        asserted_location => #{
                            lat => Lat,
                            lng => Lng,
                            h3 => list_to_binary(h3:to_string(Location))
                        }
                    }
            end;
        {error, Reason} ->
            Result#{errors => [Reason|maps:get(errors, Result)]}
    end.

maker_data(Result, Address) when is_map(Result) ->
    Url = hackney_url:make_url("https://onboarding.dewi.org", ["api", "v2", "hotspots", Address], []),
    case fetched_maker_data(hackney:get(Url, []), Address) of
        {ok, Maker} ->
            Result#{maker => Maker};
        {error, Reason} ->
            Result#{errors => [Reason|maps:get(errors, Result)]}
    end;
maker_data(Result, _) ->
    Result.

fetched_maker_data({ok, 200, _Headers, ClientRef}, _) ->
    {ok, Body} = hackney:body(ClientRef),
    #{<<"data">> := #{<<"maker">> := #{<<"name">> := Maker}}} = jsone:decode(Body, []),
    {ok, Maker};
fetched_maker_data({ok, StatusCode, _, _}, Address) ->
    lager:info("Failed to fetch Maker information for ~p: ~p~n", [Address, StatusCode]),
    {error, failed_maker_lookup}.

ip_data(#{ip := IpAddress} = Result, Address) when is_map(Result) ->
    Url = hackney_url:make_url("https://tools.keycdn.com", ["geo.json"], [{host, IpAddress}]),
    Headers = [{<<"User-Agent">>, <<"keycdn-tools:https://explorer.helium.com">>}],
    case fetched_ip_data(hackney:get(Url, Headers), Address) of
        {ok, IpData} ->
            Result#{ip_info => ip_h3(IpData)};
        {error, Reason} ->
            Result#{errors => [Reason|maps:get(errors, Result)]}
    end;
ip_data(Result, _) ->
    Result.

ip_h3(#{ lat := Lat, lng := Lng} = IpData) ->
    IpData#{ h3 => list_to_binary(h3:to_string(h3:from_geo({Lat, Lng}, 12))) };
ip_h3(Result) ->
    Result.

fetched_ip_data({ok, 200, _Headers, ClientRef}, _) ->
    {ok, Body} = hackney:body(ClientRef),
    #{<<"data">> := #{<<"geo">> := Geo}} = jsone:decode(Body, []),
    {ok, extract_ip_data(Geo)};
fetched_ip_data({ok, StatusCode, _, _}, Address) ->
    lager:info("Failed to fetch IP information for ~p: ~p~n", [Address, StatusCode]),
    {error, failed_ip_lookup}.

extract_ip_data(Geo) when is_map(Geo) ->
    extract_ip_data(Geo, #{}).
extract_ip_data(#{<<"continent_code">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"continent_code">>], Geo), Acc#{continent_code => V});
extract_ip_data(#{<<"country_code">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"country_code">>], Geo), Acc#{country_code => V});
extract_ip_data(#{<<"city">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"city">>], Geo), Acc#{city => V});
extract_ip_data(#{<<"metro_code">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"metro_code">>], Geo), Acc#{metro_code => V});
extract_ip_data(#{<<"postal_code">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"postal_code">>], Geo), Acc#{postal_code => V});
extract_ip_data(#{<<"latitude">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"latitude">>], Geo), Acc#{lat => V});
extract_ip_data(#{<<"longitude">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"longitude">>], Geo), Acc#{lng => V});
extract_ip_data(#{<<"asn">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"asn">>], Geo), Acc#{asn => V});
extract_ip_data(#{<<"isp">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"isp">>], Geo), Acc#{isp => V});
extract_ip_data(#{<<"region_code">> := V} = Geo, Acc) ->
    extract_ip_data(maps:without([<<"region_code">>], Geo), Acc#{region_code => V});
extract_ip_data(_Geo, Acc) ->
    Acc.

stale_peer_data(Result, Address) ->
    PubkeyBin = libp2p_crypto:p2p_to_pubkey_bin("/p2p/" ++ Address),
    Peerbook = libp2p_swarm:peerbook(blockchain_swarm:tid()),

    case refreshed_peer(PubkeyBin, Peerbook) of
        {ok, Peer} ->
            Result#{
                listen_addrs => listen_addrs(Peer),
                connected_peers => connected_peers(Peer),
                peer_at => epoch_seconds_to_datetime(peer_time(Peer)),
                peer_delta_seconds => seconds_since(peer_time(Peer))
            };
        {error, Reason} ->
            lager:info("Failed to refresh peer ~p: ~p~n", [Address, Reason]),
            Result#{errors => [failed_to_refresh|maps:get(errors, Result)]}
    end.

peer_data(Result, Address) ->
    P2PAddress = "/p2p/" ++ Address,
    SwarmTID = blockchain_swarm:tid(),
    case libp2p_swarm:connect(SwarmTID, P2PAddress) of
        {ok, _} ->
            lager:info("Connected to ~p successfully~n", [Address]),

            {ok, Peer} = refreshed_peer(libp2p_crypto:p2p_to_pubkey_bin(P2PAddress), libp2p_swarm:peerbook(SwarmTID)),
            {ok, Pid} = libp2p_config:lookup_session(SwarmTID, P2PAddress),
            {ok, AddrInfo} = libp2p_config:lookup_session_addr_info(SwarmTID, Pid),

            {IpAddress, Port, inet, []} = libp2p_transport_tcp:tcp_addr(lists:last(tuple_to_list(AddrInfo))),

            Height = peer_height(Peer),
            {ok, CurrentHeight} = blockchain:height(blockchain_worker:blockchain()),

            PeerLastBlockAddTime = peer_last_block_add_time(Peer),

            Result#{
                ip => list_to_binary(inet:ntoa(IpAddress)),
                listen_addrs => listen_addrs(Peer),
                connected_peers => connected_peers(Peer),
                port => Port,
                height => Height,
                height_delta => CurrentHeight - Height,
                peer_at => epoch_seconds_to_datetime(peer_time(Peer)),
                peer_delta_seconds => seconds_since(peer_time(Peer)),
                last_block_added_at => epoch_seconds_to_datetime(PeerLastBlockAddTime),
                last_block_added_delta_seconds => seconds_since(PeerLastBlockAddTime)
            };
        {error, [{_,Reason1Type}] = Reason1} ->
            lager:info("Failed to connect to ~p: ~p~n", [Address, Reason1]),
            Result#{errors => [Reason1Type|maps:get(errors, Result)]};
        {error, Reason} ->
            lager:info("Failed to connect to ~p: ~p~n", [Address, Reason]),
            Result#{errors => [failed_to_connect|maps:get(errors, Result)]}
    end.

refreshed_peer(Address, PeerBook) ->
    refreshed_peer(Address, PeerBook, true).

refreshed_peer(Address, PeerBook, Refresh) ->
    case libp2p_peerbook:get(PeerBook, Address) of
        {ok, Peer} ->
            case libp2p_peer:is_stale(Peer, 3600000) of
                true when Refresh ->
                    libp2p_peerbook:refresh(PeerBook, Address),
                    %% ARP should be quick so give it a short while
                    timer:sleep(100),
                    refreshed_peer(Address, PeerBook, false);
                _ ->
                    {ok, Peer}
            end;
        {error, _} when Refresh ->
            libp2p_peerbook:refresh(PeerBook, Address),
            %% ARP should be quick so give it a short while
            timer:sleep(100),
            refreshed_peer(Address, PeerBook, false);
        _ ->
            {error, unable_to_refresh}
    end.

listen_addrs(Peer) ->
    [list_to_binary(ListenAddress) || ListenAddress <- libp2p_peer:listen_addrs(Peer)].

connected_peers(Peer) ->
    [list_to_binary(libp2p_crypto:bin_to_b58(PubkeyBin)) || PubkeyBin <- libp2p_peer:connected_peers(Peer)].

peer_time(Peer) ->
    libp2p_peer:timestamp(Peer) div 1000.

peer_height(Peer) ->
    peer_metadata(<<"height">>, Peer).

peer_last_block_add_time(Peer) ->
    peer_metadata(<<"last_block_add_time">>, Peer).

peer_metadata(Key, Peer) ->
    libp2p_peer:signed_metadata_get(Peer, Key, undefined).

epoch_seconds_to_datetime(Seconds) when Seconds > 0 ->
    calendar:gregorian_seconds_to_datetime(?EPOCH + Seconds).

current_epoch_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?EPOCH.

seconds_since(EpochSeconds) ->
    current_epoch_seconds() - EpochSeconds.
