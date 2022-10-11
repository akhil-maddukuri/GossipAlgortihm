%%%-------------------------------------------------------------------
%%% @author dhanush
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 5:31 PM
%%%-------------------------------------------------------------------

-module(gossipAlgo).

-export([start/3,processHandler/2,createActors/3,full_network/2,line_network/2,network_2d/2,network_3d/2]).

createActors(Pids,I,N)->
  if
    I > length(Pids)->
      done;
    true->
      Pid = lists:nth(I, Pids),
      %io:fwrite("Actor created: ~p",[Pid}),
      Pid ! {start,Pids},
      createActors(Pids, I+1, N)
  end.

start(0,Pids,Network)->
  createActors(Pids, 1, length(Pids)),
  io:fwrite("Starting Time Stamp: ~p ~n",[get_time()]),
  case Network of
    "Full_Network" -> full_network(Pids,1);
    "Line_Network" -> line_network(Pids, 1);
    "2D_Network" -> network_2d(Pids, 1);
    "3D_Network"-> network_3d(Pids, 1)
  end;

start(Count,Pids,Network)->
  Pid = spawn(gossipAlgo,processHandler,[0,[]]),
  start(Count-1, lists:append([Pids,[Pid]]), Network).

processHandler(10,Pids)->
  io:fwrite("Node Converged ~n"),
  io:fwrite("Ending Time Stamp : ~p ~n",[get_time()]),
  stopActors(Pids, 1,self()),
  done;
processHandler(Count,Pids)->
  receive
    {full_network,Index}->
      %io:fwrite("Receoved gossip tp ~p \n",[self()]),
      full_network(Pids,Index),
      processHandler(Count+1, Pids);

    {line_network,Index}->
      %io:fwrite("Receoved gossip tp ~p \n",[self()]),
      line_network(Pids,Index),
      processHandler(Count+1, Pids);

    {grid_network,Index}->
      %io:fwrite("Receoved gossip tp ~p \n",[self()]),
      network_2d(Pids,Index),
      processHandler(Count+1, Pids);

    {threeD_grid_network,Index}->
      %io:fwrite("Receoved gossip tp ~p \n",[self()]),
      network_3d(Pids,Index),
      processHandler(Count+1, Pids);

    {start,Ids}->
      % io:fwrite("Initiating the Actors ~p ~n",[self()]),
      processHandler(Count,Ids);

    {done}->
      erlang:exit("exiting")
  end.

get_rand(N,Index)->
  Rand = rand:uniform(N),
  if
    Index == Rand-> get_rand(N,Index);
    true->
      Rand
  end.

rand_2d(I,J,N)->
  A = [1,0,-1],
  _RIndex1 = I + lists:nth(rand:uniform(3), A),
  _RIndex2 = J + lists:nth(rand:uniform(3), A),
  if
    _RIndex1 == I andalso _RIndex2 == J->
      rand_2d(I, J, N);
    _RIndex1 >= 0 andalso _RIndex1 < N andalso _RIndex2 >= 0 andalso _RIndex2 < N ->
      [_RIndex1,_RIndex2];
    true->
      rand_2d(I, J, N)
  end.

full_network(Pids,_Index)->
  %io:fwrite("sending gossip from ~p \n",[self()]),
  N = length(Pids),
  _RIndex =  get_rand(N, _Index),
  Pid = lists:nth(_RIndex, Pids),
  Pid ! {full_network,_RIndex},
  ok.

line_network(Pids,Index)->
  %io:fwrite("sending gossip from ~p \n",[self()]),
  _Length = length(Pids),
  if
    Index == 1->
      _RIndx = Index+1;
    Index == _Length->
      _RIndx = Index - 1;
    true->
      A = [-1,1],
      _RIndx = Index + lists:nth(rand:uniform(2), A)
  end,
  Pid = lists:nth(_RIndx, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n",[self(),Pid]),
  Pid ! {line_network,_RIndx},
  ok.

network_2d(Pids,_Index)->
  %io:fwrite("sending gossip from ~p \n",[self()]),
  %io:fwrite("In 2d network\n"),
  N = round(math:sqrt(length(Pids))),
  Indices = [round(_Index/4), _Index rem 4],
  _i = lists:nth(1, Indices),
  _j = lists:nth(2, Indices),
  _RNodes = rand_2d(_i,_j,N),
  _RIndex  = round(lists:nth(1, _RNodes)*N + lists:nth(2, _RNodes))+1,
  Pid = lists:nth(_RIndex, Pids),
  Pid ! {grid_network,_RIndex}.

network_3d(Pids,Index)->
  %io:fwrite("sending gossip from ~p \n",[self()]),
  N = round(math:sqrt(length(Pids))),
  _Nodes = [round(Index/4), Index rem 4],
  _I = lists:nth(1, _Nodes),
  _J = lists:nth(2, _Nodes),
  _Rnodes = rand_2d(_I,_J,N),
  _GRidx = round(lists:nth(1, _Rnodes)*N + lists:nth(2, _Rnodes)),
  _RIndx = get_rand(length(Pids), Index),
%%  io:fwrite("randoms: ~p, ~p",[_GRidx,_RIndx]),
  Random_Pid = lists:nth(_RIndx, Pids),
  Pid = lists:nth(_GRidx, Pids),
  Pid ! {threeD_grid_network,_GRidx},
  Random_Pid !{threeD_grid_network,_RIndx},
  done.

stopActors(Pids, Idx,Cur) ->
  if Idx > length(Pids) ->
    ok;
    true ->
      Pid = lists:nth(Idx, Pids),
      if
        Pid /= Cur->
          Pid ! {done};
        true->
          ok
      end,
      stopActors(Pids, Idx + 1,Cur)
  end.

get_time()->
  {MegaSecs, Sec, MicroSecs} = os:timestamp(),
  io:fwrite("Mega Sec is: ~p, Sec is: ~p, MicroSec is: ~p\n",[MegaSecs, Sec, MicroSecs]),
  (MegaSecs*1000000 + Sec)*1000 + (MicroSecs/1000).