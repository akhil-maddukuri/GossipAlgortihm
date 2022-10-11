%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2022 5:31 PM
%%%-------------------------------------------------------------------
-module(sum).
-author("akhil").

%% API
-export([start/3,pushsum/4]).


start_actors(1,Pids) ->
  _pid = lists:nth(1,Pids),
  _pid ! {start, 0,Pids, 1,1 };


start_actors(N, Pids) ->
  _pid = lists:nth(N, Pids),
  _pid ! {start, 0, Pids, N, 1},
  start_actors(N-1, Pids).


terminate_actors( Pids, N , _pid) ->
  if N > length(Pids) ->
    ended;
    true ->
      _pid_remove = lists:nth(N, Pids),
      if
        _pid_remove /= _pid->
          _pid_remove ! {done};
        true->
          ok
      end,
      terminate_actors(Pids, N + 1,_pid)
  end.





start(0, Pids, Network) ->
  start_actors(length(Pids)-1,Pids),
  _pid = lists:nth(length(Pids)-1, Pids),

  if Network == "Full_Network"  ->
    _pid ! {sum, full_network, 0, 0, 1};
    Network == "2D_Grid" -> _pid ! {sum, twodgrid, 0, 0, 1};
    Network == "Line" -> _pid ! {sum, line, 0, 0, 1};
    Network == "Imperfect_2D" -> _pid ! {sum, impft2d, 0, 0, 1};
    true ->
      throw("invalid topology")
  end;


start(N, _pids, Network) ->
  {_,_} = statistics(runtime),
  {_,_} = statistics(wall_clock),
  _pid = spawn(sum, pushsum, [0, [], N, 1]),
  start(N - 1, lists:append([_pids, [_pid]]), Network).


pushsum(3, Pids,_,_) ->
  io:fwrite("Converged ~n"),
  %io:fwrite("~p ~n",[get_time_stamp()]),
  terminate_actors(Pids, 1,self()),
  done;

pushsum(Count, Pids,_curr_s,_curr_w)->
  receive
    {sum, full_network, S, W, _index} ->
      _updated_s = _curr_s + S,
      _updated_w = _curr_w + W,
      send_full_network(Pids, _updated_s / 2, _updated_w / 2, _index),

      if abs((_curr_s/_curr_w) - (_updated_s/_updated_w)) =< 0.0000000001 ->
        pushsum(Count + 1, Pids, _updated_s / 2, _updated_w / 2);
        true ->
          pushsum(0, Pids, _updated_s / 2, _updated_w / 2)
      end;

    {sum, twodgrid, S, W, _index} ->
      _updated_s = _curr_s + S,
      _updated_w = _curr_w + W,
      send_2d_network(Pids, _updated_s / 2, _updated_w / 2, _index),

      if abs((_curr_s/_curr_w) - (_updated_s/_updated_w)) =< 0.0000000001 ->
        pushsum(Count + 1, Pids, _updated_s / 2, _updated_w / 2);
        true ->
          pushsum(0, Pids, _updated_s / 2, _updated_w / 2)
      end;



    {sum, line, S, W, _index} ->
      _updated_s = _curr_s + S,
      _updated_w = _curr_w + W,
      send_line_network(Pids, _updated_s / 2, _updated_w / 2, _index),

      if abs((_curr_s/_curr_w) - (_updated_s/_updated_w)) =< 0.0000000001 ->
        pushsum(Count + 1, Pids, _updated_s / 2, _updated_w / 2);
        true ->
          pushsum(0, Pids, _updated_s / 2, _updated_w / 2)
      end;


    {sum, impft2d, S, W, _index} ->
      _updated_s = _curr_s + S,
      _updated_w = _curr_w + W,
      send_3d_network(Pids, _updated_s / 2, _updated_w / 2, _index),

      if abs((_curr_s/_curr_w) - (_updated_s/_updated_w)) =< 0.0000000001 ->
        pushsum(Count + 1, Pids, _updated_s / 2, _updated_w / 2);
        true ->
          pushsum(0, Pids, _updated_s / 2, _updated_w / 2)
      end;

    {start, C, Ids, S, W} ->
      % io:fwrite("The initial S,W are ~p ~p ~n", [S, W]),
      pushsum(C, Ids, S, W);

    {done}->
      exit("")
  end.

get_2d_index(I, N) ->
  [round(I / N), I rem N].

get_1d_index(I, J, N) ->
  round(I * N + J).

get_random_index_2d(I, J, N) ->
  A = [1, 0, -1],
  Random_Index_1 =
    I
      + lists:nth(
      rand:uniform(3), A),
  Random_Index_2 =
    J
      + lists:nth(
      rand:uniform(3), A),
  if Random_Index_1 == I andalso Random_Index_2 == J ->
    get_random_index_2d(I, J, N);
    Random_Index_1 >= 0
      andalso Random_Index_1 < N
      andalso Random_Index_2 >= 0
      andalso Random_Index_2 < N ->
      [Random_Index_1, Random_Index_2];
    true ->
      get_random_index_2d(I, J, N)
  end.

get_random_index(N, Index) ->
  io:fwrite("Entred random index fundion\n"),
  J = rand:uniform(N),
  io:fwrite("Exiting\n"),
  if Index /= J ->
      J;
    true ->
      get_random_index(N, Index)
  end.



send_full_network(Pids, S, W, Index) ->
  N = length(Pids),
  io:fwrite("Value of N is : ~p and Index is ~p \n",[N,Index]),
  Random_Index = get_random_index(N, Index),
  Pid = lists:nth(Random_Index, Pids),
  Pid ! {sum, full_network, S, W, Random_Index}.

send_line_network(Pids, S, W, Index) ->
  N = length(Pids),
  if Index == 1 ->
    Random_Index = Index + 1;
    Index == N ->
      Random_Index = Index - 1;
    true ->
      A = [-1, 1],
      Random_Index =
        Index
          + lists:nth(
          rand:uniform(2), A)
  end,
  Pid = lists:nth(Random_Index, Pids),
  io:fwrite("Sending gossip from ~p to ~p ~n", [self(), Pid]),
  Pid ! {sum, line_network, S, W, Random_Index}.

send_2d_network(Pids, S, W, Index) ->
  N = round(math:sqrt(length(Pids))),
  % io:fwrite("~p ~n",[N]),
  Indices = get_2d_index(Index, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  Random_Indices = get_random_index_2d(I, J, N),
  Random_Index =
    get_1d_index(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N) + 1,
  % io:fwrite("~p ~n",[Random_Indices]),
  Pid = lists:nth(Random_Index, Pids),
  io:fwrite("Sending gossip from ~p to ~p ~n", [self(), Pid]),
  Pid ! {sum, grid_network, S, W, Random_Index}.

send_3d_network(Pids, S, W, Index) ->
  N = round(math:sqrt(length(Pids))),
  % io:fwrite("~p ~n",[N]),
  Indices = get_2d_index(Index, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  Random_Indices = get_random_index_2d(I, J, N),
  Grid_Random_Index =
    get_1d_index(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N) + 1,
  Random_Index = get_random_index(length(Pids), Index),
  Random_Pid = lists:nth(Random_Index, Pids),
  Pid = lists:nth(Grid_Random_Index, Pids),
  % io:fwrite("Sending gossip from ~p to ~p ~n", [self(), Pid]),
  Pid ! {sum, threeD_grid_network, S, W, Grid_Random_Index},
  Random_Pid ! {sum, threeD_grid_network, S, W, Random_Index}.











