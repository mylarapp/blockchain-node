-module(bn_swarm_keys).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-define(EPOCH, calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).

-export([handle_rpc/2]).

handle_rpc(<<"swarm_keys">>, {Param}) ->
    Adjectives = [binary_to_list(Ad) || Ad <- ?jsonrpc_get_param(<<"adjective">>, Param, [])],
    Colors = [binary_to_list(C) || C <- ?jsonrpc_get_param(<<"color">>, Param, [])],
    Animals = [binary_to_list(An) || An <- ?jsonrpc_get_param(<<"animal">>, Param, [])],
    Tries = ?jsonrpc_get_param(<<"tries">>, Param, 10000),

    case generate_key(Tries, Adjectives, Colors, Animals) of
        Keys = #{public := Pub} ->
            #{
                address => list_to_binary(libp2p_crypto:pubkey_to_b58(Pub)),
                name => list_to_binary(blockchain_utils:addr2name(libp2p_crypto:pubkey_to_bin(Pub))),
                swarm_key => base64:encode(libp2p_crypto:keys_to_bin(Keys))
            };
        {error, Error} ->
            #{error => Error};
        _ ->
            #{error => unknown}
    end;

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

generate_key(0, _, _, _) ->
    {error, exhausted_tries};
generate_key(Tries, _, _, _) when Tries > 1000000 ->
    {error, too_many_tries};
generate_key(Tries, Adjectives, Colors, Animals) ->
    #{public := Pub, secret := _} = Keys = libp2p_crypto:generate_keys(ed25519),

    AnimalName = blockchain_utils:addr2name(libp2p_crypto:pubkey_to_bin(Pub)),
    lager:info("~p (~p)", [libp2p_crypto:pubkey_to_b58(Pub), AnimalName]),

    [GAdjective, GColor, GAnimal] = string:split(AnimalName, "-", all),

    case [check(GAdjective, Adjectives), check(GColor, Colors), check(GAnimal, Animals), check_key(Keys)] of
        [ok, ok, ok, ok] -> Keys;
        _ -> generate_key(Tries - 1, Adjectives, Colors, Animals)
    end.

check(_Generated, []) ->
    ok;
check(Generated, Wanted) when is_list(Wanted) ->
    check_list(Generated, Wanted).

check_list(_, []) ->
    fail;
check_list(Same, [Same|_]) ->
    ok;
check_list(Generated, [_Wanted|Rest]) ->
    check_list(Generated, Rest).

check_key(#{ network := mainnet, public := Pub, secret := Priv }) ->
    check_signature(libp2p_crypto:verify(<<"hello world">>, (libp2p_crypto:mk_sig_fun(Priv))(<<"hello world">>), Pub));
check_key(_) ->
    fail.

check_signature(true) ->
    ok;
check_signature(false) ->
    fail.
