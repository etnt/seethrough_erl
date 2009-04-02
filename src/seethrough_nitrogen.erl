%%%-------------------------------------------------------------------
%%% Created :  3 Apr 2009 by Torbjorn Tornkvist <tobbe@kreditor.se>
%%% Desc.   : 
%%%-------------------------------------------------------------------
-module(seethrough_nitrogen).

-export([compile/2]).

-include_lib("xmerl/include/xmerl.hrl").


-define(xdbg(Message, Args), 
        io:format("~p(~p): ~s~n", [?MODULE,?LINE,io_lib:format(Message, Args)])).
%%-define(xdbg(Message, Args), no_op).

-define(namespace, 'http://dev.tornkvist.org/seethrough/nitrogen').



compile(Node, Attributes) -> 
    ?xdbg("compile: Node=~p , Attributes=~p~n",[Node,Attributes]),
    fun(_Env) ->
            Node#xmlElement{attributes = Attributes}
    end.



    
