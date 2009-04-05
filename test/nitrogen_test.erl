-module(nitrogen_test).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).


label_test() ->
    {E,_} = xmerl_scan:file("./priv/label.xml"),
    {ok,S} = xmerl_xsd:process_schema("priv/nitrogen.xsd",
                                      [{xsdbase,"."}]),
    ?assertMatch({#xmlElement{},_}, xmerl_xsd:validate(E,S)).

wire_test() ->
    {E,_} = xmerl_scan:file("./priv/wire.xml"),
    {ok,S} = xmerl_xsd:process_schema("priv/nitrogen.xsd",
                                      [{xsdbase,"."}]),
    ?assertMatch({#xmlElement{},_}, xmerl_xsd:validate(E,S)).


