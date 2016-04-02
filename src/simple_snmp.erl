-module(simple_snmp).
-behaviour(application).

% Application
-export([
    start/2,
    stop/1
]).

% API
-export([
    get/2,
    get/3
]).

start(normal, _Args) ->
    snmpm:start(),
    snmpm:register_user(simple_snmp_user, simple_snmp_user, undefined),
    {ok, self()}.

stop(_State) ->
    ok.

get(Address, Oid) ->
    get(v2, Address, "public", Oid).

get(Address, Community, Oid) ->
    get(v2, Address, Community, Oid).

get(Version, Address, Community, Oid) ->
    ok = prepare(Version, Address, Community),

    case snmpm:sync_get(simple_snmp_user, "simple_snmp_user", [Oid]) of
        {ok, {_, _, Result}, _Remaining} ->
            [{_, Oid, Type, Value, _}] = Result,
            {Oid, Type, Value};
        {error, Reason} ->
            error_logger:error_msg("Derp ~p ~n", [Reason])
    end.

prepare(Version, Address, Community) ->
    Options = [
        {engine_id, "simple_snmp_engine"},
        {community, Community},
        {version, Version}
        |
        case Address of
            {Host, Port} ->
                [
                    {address, Host},
                    {port, Port}
                ];
            Host ->
                {ok, HostAddress} = inet:parse_address(Host),
                [{address, HostAddress}]
        end
    ],
    case snmpm:which_agents(simple_snmp_user) of
        [] -> ok;
        _ ->
            snmpm:unregister_agent(simple_snmp_user, "simple_snmp_user")
    end,
    snmpm:register_agent(simple_snmp_user, "simple_snmp_user", Options).
