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
    get/3,
    get_next/2,
    get_next/3
]).

start(normal, _Args) ->
    snmpm:start(),
    snmpm:register_user(simple_snmp_user, simple_snmp_user, undefined),
    {ok, self()}.

stop(_State) ->
    ok.

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

format_result([], State) ->
    lists:reverse(State);
format_result([{_, Oid, Type, Value, _}|T], State) ->
    format_result(T, [{Oid, Type, Value}|State]).

get(Address, Oids) ->
    get(v2, Address, "public", Oids).

get(Address, Community, Oids) ->
    get(v2, Address, Community, Oids).

get(Version, Address, Community, Oids) ->
    ok = prepare(Version, Address, Community),

    case snmpm:sync_get(simple_snmp_user, "simple_snmp_user", Oids) of
        {ok, {_, _, Result}, _Remaining} ->
            format_result(Result, []);
        {error, Reason} ->
            error_logger:error_msg("Derp ~p ~n", [Reason])
    end.

get_next(Address, Oids) ->
    get_next(v2, Address, "public", Oids).

get_next(Address, Community, Oids) ->
    get_next(v2, Address, Community, Oids).

get_next(Version, Address, Community, Oids) ->
    ok = prepare(Version, Address, Community),

    case snmpm:sync_get_next(simple_snmp_user, "simple_snmp_user", [Oids]) of
        {ok, {_, _, Result}, _Remaining} ->
            format_result(Result, []);
        {error, Reason} ->
            error_logger:error_msg("Derp ~p ~n", [Reason]),
            {error, Reason}
    end.
