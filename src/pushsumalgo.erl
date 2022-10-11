%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 4:21 PM
%%%-------------------------------------------------------------------
-module(pushsumalgo).
-author("akhil").

%% API
-export([start/3, server/4]).

get_time_stamp()->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + (Micro/1000).

end_actors(Pids, Index,Curent_Pid) ->
  if Index > length(Pids) ->
    started;
    true ->
      Pid = lists:nth(Index, Pids),
      if
        Pid /= Curent_Pid->
          Pid ! {done};

        true->
          ok
      end,
      end_actors(Pids, Index + 1,Curent_Pid)
  end.

start_actors(Pids, Index) ->
  if Index > length(Pids) ->
    started;
    true ->
      Pid = lists:nth(Index, Pids),
      Pid ! {start, 0, Pids, Index, 1},
      start_actors(Pids, Index + 1)
  end.

start(0, Pids, Network) ->

  start_actors(Pids, 1),
  _pid = lists:nth(1, Pids),
  io:fwrite("~p ~n",[get_time_stamp()]),

  if Network == "Full_Network"  ->
  _pid ! {pushsumalgo, full_network, 0, 0, 1};
  Network == "2D_Grid" -> _pid ! {pushsumalgo, twodgrid, 0, 0, 1};
  Network == "Line" -> _pid ! {pushsumalgo, line, 0, 0, 1};
  Network == "Imperfect_2D" -> _pid ! {pushsumalgo, impft2d, 0, 0, 1};
  true ->
  throw("invalid topology")
  end;

start(N, Pids, Network) ->
  Pid = spawn(pushsumalgo,server, [0, [], 1, 1]),
  start(N - 1, lists:append([Pids, [Pid]]), Network).

server(3, Pids, _, _) ->
  io:fwrite("Converged ~n"),
  io:fwrite("~p ~n",[get_time_stamp()]),
  end_actors(Pids, 1,self()),
  done;
server(Count, Pids, Current_S, Current_W) ->
  receive
    {pushsumalgo, full_network, S, W, Index} ->
      New_S = Current_S + S,
      New_W = Current_W + W,
      send_full_network(Pids, New_S / 2, New_W / 2, Index),

      if abs(Current_S / Current_W - New_S / New_W) =< 0.0000000001 ->
        server(Count + 1, Pids, New_S / 2, New_W / 2);
        true ->
          server(0, Pids, New_S / 2, New_W / 2)
      end;

    {pushsumalgo, line, S, W, Index} ->
      New_S = Current_S + S,
      New_W = Current_W + W,
      send_line_network(Pids, New_S / 2, New_W / 2, Index),

      if abs(Current_S / Current_W - New_S / New_W) =< 0.0000000001 ->
        server(Count + 1, Pids, New_S / 2, New_W / 2);
        true ->
          server(0, Pids, New_S / 2, New_W / 2)
      end;

    {pushsumalgo, twodgrid, S, W, Index} ->
      New_S = Current_S + S,
      New_W = Current_W + W,

      send_2d_network(Pids, New_S / 2, New_W / 2, Index),

      if abs(Current_S / Current_W - New_S / New_W) =< 0.0000000001 ->
        server(Count + 1, Pids, New_S / 2, New_W / 2);
        true ->
          server(0, Pids, New_S / 2, New_W / 2)
      end;

    {pushsumalgo, impft2d, S, W, Index} ->
      New_S = Current_S + S,
      New_W = Current_W + W,

      send_3d_network(Pids, New_S / 2, New_W / 2, Index),

      if abs(Current_S / Current_W - New_S / New_W) =< 0.0000000001 ->
        server(Count + 1, Pids, New_S / 2, New_W / 2);
        true ->
          server(0, Pids, New_S / 2, New_W / 2)
      end;

    {start, C, Ids, S, W} ->
      % io:fwrite("The initial S,W are ~p ~p ~n", [S, W]),
      server(C, Ids, S, W);

    {done}->
      io:fwrite("procss ~p ended\n",[self()]),
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
  J = rand:uniform(N),
  if Index /= J ->
    J;
    true ->
      get_random_index(N, Index)
  end.

send_full_network(Pids, S, W, Index) ->
  N = length(Pids),
  Random_Index = get_random_index(N, Index),
  Pid = lists:nth(Random_Index, Pids),
  Pid ! {pushsumalgo, full_network, S, W, Random_Index}.

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

  Pid ! {pushsumalgo, line, S, W, Random_Index}.

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

  Pid ! {pushsumalgo, twodgrid, S, W, Random_Index}.

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

  Pid ! {pushsumalgo, impft2d, S, W, Grid_Random_Index},
  Random_Pid ! {pushsumalgo, impft2d, S, W, Random_Index}.

