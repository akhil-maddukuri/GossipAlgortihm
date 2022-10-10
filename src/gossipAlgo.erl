%%%-------------------------------------------------------------------
%%% @author dhanush
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 5:31 PM
%%%-------------------------------------------------------------------

-module(gossipAlgo).

-export([start/3,processHandler/2]).

get_time()->
  {MegaSecs, Sec, MicroSecs} = os:timestamp(),
  %io:fwrite("Mega Sec is: ~p, Sec is: ~p, MicroSec is: ~p\n",[MegaSecs, Sec, MicroSecs]),
  (MegaSecs*1000000 + Sec)*1000 + (MicroSecs/1000).

createActors(Pids,I,N)->
  if
    I > length(Pids)->
      done;
    true->
      Pid = lists:nth(I, Pids),
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
  io:fwrite("Converged ~n"),
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
      line_network(Pids,Index),
      processHandler(Count+1, Pids);

    {grid_network,Index}->
      network_2d(Pids,Index),
      processHandler(Count+1, Pids);

    {threeD_grid_network,Index}->
      network_3d(Pids,Index),
      processHandler(Count+1, Pids);

    {start,Ids}->
      % io:fwrite("Initiating the Actors ~p ~n",[self()]),
      processHandler(Count,Ids);

    {done}->
      erlang:exit("exiting")
  end.


rand_2d_idx(I,J,N)->
  A = [1,0,-1],
  Random_Index_1 = I + lists:nth(rand:uniform(3), A),
  Random_Index_2 = J + lists:nth(rand:uniform(3), A),
  if
    Random_Index_1 == I andalso Random_Index_2 == J->
      rand_2d_idx(I, J, N);
    Random_Index_1 >= 0 andalso Random_Index_1 < N andalso Random_Index_2 >= 0 andalso Random_Index_2 < N ->
      [Random_Index_1,Random_Index_2];
    true->
      rand_2d_idx(I, J, N)
  end.

get_random_index(N,Index)->
  J = rand:uniform(N),
  if
    Index /= J->
      J;
    true->
      get_random_index(N, Index)
  end.
full_network(Pids,Index)->
  %io:fwrite("sending gossip from ~p \n",[self()]),
  N = length(Pids),
  Random_Index =  get_random_index(N, Index),
  Pid = lists:nth(Random_Index, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n",[self(),Pid]),
  Pid ! {full_network,Random_Index},
  ok.

line_network(Pids,Index)->
  N = length(Pids),
  if
    Index == 1->
      Random_Index = Index+1;
    Index == N->
      Random_Index = Index - 1;
    true->
      A = [-1,1],
      Random_Index = Index + lists:nth(rand:uniform(2), A)
  end,
  Pid = lists:nth(Random_Index, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n",[self(),Pid]),
  Pid ! {line_network,Random_Index},
  ok.

network_2d(Pids,_Indx)->
  N = round(math:sqrt(length(Pids))),
  Indices = [round(_Indx/4), _Indx rem 4],
  %Indices = get_2d_index(_Indx, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  _RNodes = rand_2d_idx(I,J,N),
  _RIndx  = round(lists:nth(1, _RNodes)*N + lists:nth(2, _RNodes)),
  %Random_Index = get_1d_index(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N)+1,
  Pid = lists:nth(_RIndx, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n",[self(),Pid]),
  Pid ! {grid_network,_RIndx},
  ok.

network_3d(Pids,Index)->
  N = round(math:sqrt(length(Pids))),
  % io:fwrite("~p ~n",[N]),
  Indices = [round(Index/4), Index rem 4],
  %Indices = get_2d_index(Index, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  Random_Indices = rand_2d_idx(I,J,N),
  _GRidx = round(lists:nth(1, Random_Indices)*N + lists:nth(2, Random_Indices)),
  %Grid_Random_Index = get_1d_index(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N)+1,
  Random_Index = get_random_index(length(Pids), Index),
  Random_Pid = lists:nth(Random_Index, Pids),
  Pid = lists:nth(_GRidx, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n",[self(),Pid]),
  Pid ! {threeD_grid_network,_GRidx},
  Random_Pid !{threeD_grid_network,Random_Index},
  ok.

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