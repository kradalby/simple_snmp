simple_snmp
=====

Simple wrapper around Erlang's builtin SNMP manager. The goal is to provide easier access to SNMP functionality such as get and get_next.

The initial code is based on the implementation [https://github.com/ates/snmpcl](https://github.com/ates/snmpcl)

Build
-----

    $ rebar3 compile


## Usage
Get:

    1> application:start(simple_snmp).
    ok
    2> simple_snmp:get("195.218.195.228", [[1,3,6,1,2,1,1,6,0]]).
    [{[1,3,6,1,2,1,1,6,0],'OCTET STRING',"Moscow, Russia"}]

Get with multiple OIDs:

    1> simple_snmp:get("195.218.195.228", [[1,3,6,1,2,1,1,5,0],[1,3,6,1,2,1,1,7,0]]).
    [{[1,3,6,1,2,1,1,5,0],'OCTET STRING',"zeus.snmplabs.com"},
     {[1,3,6,1,2,1,1,7,0],'INTEGER',72}]

Get next:

    1> application:start(simple_snmp).
    ok
    2> simple_snmp:get_next("195.218.195.228", [[1,3,6,1,2,1,1,7,0]]).
    [{[1,3,6,1,2,1,1,8,0],'TimeTicks',212615850}]
