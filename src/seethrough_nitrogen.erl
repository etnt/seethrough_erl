%%%-------------------------------------------------------------------
%%% Created :  3 Apr 2009 by Torbjorn Tornkvist <tobbe@kreditor.se>
%%% Desc.   : 
%%%-------------------------------------------------------------------
-module(seethrough_nitrogen).

-export([compile/2]).

%%-import(seethrough, [get_attr_value/2]).

-include_lib("xmerl/include/xmerl.hrl").


-define(xdbg(Message, Args), 
        io:format("~p(~p): ~s~n", [?MODULE,?LINE,io_lib:format(Message, Args)])).
%%-define(xdbg(Message, Args), no_op).

-define(namespace, 'http://dev.tornkvist.org/seethrough/nitrogen').


%%% --------------------------------------------------------------------
%%% L A B E L
%%% --------------------------------------------------------------------

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[{N,?namespace}|_]},
                    nsinfo = {N,"label"},
                    attributes = _Attributes} = Node, 
        Attributes) ->

    case get_value(text, Attributes) of
        undefined ->
            fun(_Env) ->
                    ?xdbg("ERROR Expanding ~s:label", [N]),
                    Node
            end;        
        Value ->
            fun(_Env) ->
                    ?xdbg("Expanding ~s:label", [N]),

                    %% FIXME get a proper TempID !!

                    Html = "<span id='temp273949' class='label'>"++Value++"</span>",
                    {#xmlElement{} = Tree, _} = xmerl_scan:string(Html),
                    Tree

            end
    end;

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[_|Ns]} = Xns,
                    nsinfo = {_,"label"}} = Node, 
        _Attributes) ->
    compile(Node#xmlElement{namespace=Xns#xmlNamespace{nodes=Ns}}, _Attributes);

%%% --------------------------------------------------------------------
%%% T E X T B O X
%%% --------------------------------------------------------------------

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[{N,?namespace}|_]},
                    nsinfo = {N,"textbox"},
                    attributes = _Attributes} = Node, 
        Attributes) ->

    case get_value(id, Attributes) of
        undefined ->
            fun(_Env) ->
                    ?xdbg("ERROR Expanding ~s:textbox~n", [N]),
                    Node
            end;        
        Id ->
            fun(_Env) ->
                    ?xdbg("Expanding ~s:textbox~n", [N]),

                    %% FIXME get a proper TempID !!

                    Next = next(Attributes),
                    Html = "<input id='page__"++Id++"' name='page__"++Id++"' type='text' class='textbox'/>",
                    Script = "<script type=\"text/javascript\">Nitrogen.$observe_event(obj('page."++Id++"'), 'keypress', function anonymous(event) {if (Nitrogen.$is_enter_key(event)) {  Nitrogen.$current_id='page';Nitrogen.$current_path='page."++Id++"';"++Next++"</script>",
                    {#xmlElement{} = Tree, _} = xmerl_scan:string(Html),
                    {#xmlElement{} = Tree2, _} = xmerl_scan:string(Script),
                    ?xdbg("Expanding ~s:textbox to: ~p~n", [N,[Tree,Tree2]]),
                    [Tree,Tree2]

            end
    end;

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[_|Ns]} = Xns,
                    nsinfo = {_,"textbox"}} = Node, 
        _Attributes) ->
    compile(Node#xmlElement{namespace=Xns#xmlNamespace{nodes=Ns}}, _Attributes);

%%% --------------------------------------------------------------------
%%% P A S S W O R D
%%% --------------------------------------------------------------------

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[{N,?namespace}|_]},
                    nsinfo = {N,"password"},
                    attributes = _Attributes} = Node, 
        Attributes) ->

    case get_value(id, Attributes) of
        undefined ->
            fun(_Env) ->
                    ?xdbg("ERROR Expanding ~s:password~n", [N]),
                    Node
            end;        
        Id ->
            fun(_Env) ->
                    ?xdbg("Expanding ~s:password~n", [N]),

                    %% FIXME get a proper TempID !!

                    Next = next(Attributes),
                    Html = "<input id='page__"++Id++"' name='page__"++Id++"' type='password' class='password'/>",
                    Script = "<script type=\"text/javascript\">Nitrogen.$observe_event(obj('page."++Id++"'), 'keypress', function anonymous(event) {if (Nitrogen.$is_enter_key(event)) {  Nitrogen.$current_id='page';Nitrogen.$current_path='page."++Id++"';"++Next++"</script>",
                    {#xmlElement{} = Tree, _} = xmerl_scan:string(Html),
                    {#xmlElement{} = Tree2, _} = xmerl_scan:string(Script),
                    [Tree,Tree2]

            end
    end;

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[_|Ns]} = Xns,
                    nsinfo = {_,"password"}} = Node, 
        _Attributes) ->
    compile(Node#xmlElement{namespace=Xns#xmlNamespace{nodes=Ns}}, _Attributes);

%%% --------------------------------------------------------------------
%%% B U T T O N
%%% --------------------------------------------------------------------

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[{N,?namespace}|_]},
                    nsinfo = {N,"button"},
                    attributes = _Attributes} = Node, 
        Attributes) ->

    case {get_value(id, Attributes),
          get_value(text, Attributes)} of

          {X,Y} when X==undefined orelse Y==undefined ->
            fun(_Env) ->
                    ?xdbg("ERROR Expanding ~s:button", [N]),
                    Node
            end;        
        {Id,Value} ->
            fun(_Env) ->
                    ?xdbg("Expanding ~s:button", [N]),

                    %% FIXME get a proper TempID !!

                    Html = "<input id='page__"++Id++"' name='page__"++Id++"' "
                        " type='button' class='button' value='"++Value++"'/>",
                    {#xmlElement{} = Tree, _} = xmerl_scan:string(Html),
                    Tree

            end
    end;

compile(#xmlElement{namespace=
                    #xmlNamespace{nodes=[_|Ns]} = Xns,
                    nsinfo = {_,"button"}} = Node, 
        _Attributes) ->
    compile(Node#xmlElement{namespace=Xns#xmlNamespace{nodes=Ns}}, _Attributes);





compile(Node, Attributes) -> 
    ?xdbg("compile: Node=~p , Attributes=~p~n",[Node,Attributes]),
    fun(_Env) ->
            Node#xmlElement{attributes = Attributes}
    end.


%%% The next attribute is optional
next(Attributes) ->
    case get_value(next, Attributes) of
        undefined -> "";
        Next ->
            "Nitrogen.$go_next('"++Next++"');;; return false; }});"
    end.

get_value(Name, Attributes) ->
    try 
        [Value] = [V || #xmlAttribute{name=N,value=V} <- Attributes,
                        N == Name],
        Value
    catch
        _:_ -> undefined
    end.
    
