-module(nitrogen_test).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-import(seethrough_test,[stringify/1]).


%%% --------------------------------------------------------------------
%%% Nitrogen generation tests
%%% --------------------------------------------------------------------

label() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <n:label text=\"Name\"/>"
         "  </body>"
         "</html>"
         },
        [{title, "Label Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).


textbox() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <n:textbox id=\"nameTextBox\" next=\"passwordTextBox\"/>"
         "  </body>"
         "</html>"
         },
        [{title, "Textbox Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).


password() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <n:password id=\"passwordTextBox\"  next=\"continueButton\"/>"
         "  </body>"
         "</html>"
         },
        [{title, "Password Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).


button() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <n:button id=\"continueButton\" text=\"Continue\" postback=\"continue\"/>"
         "  </body>"
         "</html>"
         },
        [{title, "Button Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).

wire_validate() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <n:wire to=\"continueButton\" what=\"nameTextBox\">"
         "      <n:validate>"
         "        <n:is_required text=\"Required\"/>"
         "      </n:validate>"
         "    </n:wire>"
         "  </body>"
         "</html>"
         },
        [{title, "Textbox Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).
    



all() ->
    stringify(
      seethrough:apply_template(
        {string, 
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
         "      xmlns:e=\"http://dev.hyperstruct.net/seethrough\" "
         "      xmlns:n=\"http://dev.tornkvist.org/seethrough/nitrogen\">"
         "  <head>"
         "    <title e:content=\"title\"/>"
         "  </head>"
         "  <body>"
         "    <p />"
         "    <n:label text=\"Name\"/>"
         "    <n:textbox id=\"nameTextBox\" next=\"passwordTextBox\"/>"
         "    <p />"
         "    <n:label text=\"Password\"/>"
         "    <n:password id=\"passwordTextBox\"  next=\"continueButton\"/>"
         "    <p />"
         "    <n:button id=\"continueButton\" text=\"Continue\" postback=\"continue\"/>"
         "    <n:wire to=\"continueButton\" what=\"nameTextBox\">"
         "      <n:validate>"
         "        <n:is_required text=\"Name is Required\"/>"
         "      </n:validate>"
         "    </n:wire>"
         "    <n:wire to=\"continueButton\" what=\"passwordTextBox\">"
         "      <n:validate>"
         "        <n:is_required text=\"Password is Required\"/>"
         "      </n:validate>"
         "    </n:wire>"
         "  </body>"
         "</html>"
         },
        [{title, "Textbox Test"}],
        [{'http://dev.tornkvist.org/seethrough/nitrogen', seethrough_nitrogen}])).



%%% --------------------------------------------------------------------
%%% Validating tests for the XML Schema
%%% --------------------------------------------------------------------

xlabel_test() ->
    {E,_} = xmerl_scan:file("./priv/label.xml"),
    {ok,S} = xmerl_xsd:process_schema("priv/nitrogen.xsd",
                                      [{xsdbase,"."}]),
    ?assertMatch({#xmlElement{},_}, xmerl_xsd:validate(E,S)).

xwire_test() ->
    {E,_} = xmerl_scan:file("./priv/wire.xml"),
    {ok,S} = xmerl_xsd:process_schema("priv/nitrogen.xsd",
                                      [{xsdbase,"."}]),
    ?assertMatch({#xmlElement{},_}, xmerl_xsd:validate(E,S)).


