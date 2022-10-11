%%%-------------------------------------------------------------------
%%% @author dhanush
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 6:49 PM
%%%-------------------------------------------------------------------
-module(main).
-author("dhanush").

%% API
-export([start/3]).

start(N,Method,Network)->
%%  io:fwrite("Mega Sec is: ~p, Sec is: ~p, MicroSec is: ~p\n",[1665, 457264, 107491]),
%%  io:fwrite("ok\n"),
%%  io:fwrite("Node Converged\n"),
%%  io:fwrite("Mega Sec is: ~p, Sec is: ~p, MicroSec is: ~p\n",[1665, 457266, 934491]).

  case Method of
    "gossip" -> gossipAlgo:start(N,[],Network);
    "pushsum"-> pushsumalgo:start(N,[],Network)
  end.

