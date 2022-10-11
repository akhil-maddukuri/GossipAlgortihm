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
  case Method of
    "gossip" -> gossipAlgo:start(N,[],Network)
  end.

