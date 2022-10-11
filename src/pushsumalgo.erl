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
-export([start/3, server/5]).


start_actors(Pids, Index) ->
  if Index > length(Pids) ->
    started;
    true ->
      Pid = lists:nth(Index, Pids),
      Pid ! {start, 0, Pids, Index, 1},
      start_actors(Pids, Index + 1)
  end.


system_time()->
  {M, S, Mi} = os:timestamp(),
  (M*1000000 + S)*1000 + (Mi/1000).



start(0, Pids, Network) ->

  start_actors(Pids, 1),
  _pid = lists:nth(1, Pids),
  io:fwrite("~p ~n",[system_time()]),

  if Network == "Full_Network"  ->
  _pid ! {pushsumalgo, full_network, 0, 0, 1};
  Network == "2D_Grid" -> _pid ! {pushsumalgo, twodgrid, 0, 0, 1};
  Network == "Line" -> _pid ! {pushsumalgo, line, 0, 0, 1};
  Network == "Imperfect_2D" -> _pid ! {pushsumalgo, impft2d, 0, 0, 1};
  true ->
  throw("invalid topology")
  end;

start(N, Pids, Network) ->
  Pid = spawn(pushsumalgo,server, [0, [], 1, 1,""]),
  start(N - 1, lists:append([Pids, [Pid]]), Network).

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



server(3, Pids, _, _, _) ->
  io:fwrite("Converged ~n"),
  io:fwrite("~p ~n",[system_time()]),
  end_actors(Pids, 1,self()),
  done;
server(Sequences, Pids, Old_S, Old_W,_) ->
  receive
    {pushsumalgo, full_network, S, W, Index} ->
      Updated_S = Old_S + S,
      Updated_W = Old_W + W,
      full_network(Pids, Updated_S / 2, Updated_W / 2, Index),

      if abs(Old_S / Old_W - Updated_S / Updated_W) =< 0.0000000001 ->
        server(Sequences + 1, Pids, Updated_S / 2, Updated_W / 2,"");
        true ->
          server(0, Pids, Updated_S / 2, Updated_W / 2, "")

      end;

    {pushsumalgo, line, S, W, Index} ->
      Updated_S = Old_S + S,
      New_W = Old_W + W,
      line_network(Pids, Updated_S / 2, New_W / 2, Index),

      if abs(Old_S / Old_W - Updated_S / New_W) =< 0.0000000001 ->
        server(Sequences + 1, Pids, Updated_S / 2, New_W / 2, "");
        true ->
          server(0, Pids, Updated_S / 2, New_W / 2, "")
      end;

    {pushsumalgo, twodgrid, S, W, Index} ->
      Updated_S = Old_S + S,
      New_W = Old_W + W,

      twod_network(Pids, Updated_S / 2, New_W / 2, Index),

      if abs(Old_S / Old_W - Updated_S / New_W) =< 0.0000000001 ->
        server(Sequences + 1, Pids, Updated_S / 2, New_W / 2, "");
        true ->
          server(0, Pids, Updated_S / 2, New_W / 2, "")
      end;

    {pushsumalgo, impft2d, S, W, Index} ->
      Updated_S = Old_S + S,
      New_W = Old_W + W,

      threed_network(Pids, Updated_S / 2, New_W / 2, Index),

      if abs(Old_S / Old_W - Updated_S / New_W) =< 0.0000000001 ->
        server(Sequences + 1, Pids, Updated_S / 2, New_W / 2, "");
        true ->
          server(0, Pids, Updated_S / 2, New_W / 2,"")
      end;

    {start, C, Ids, S, W} ->

      server(C, Ids, S, W,"");

    {done}->

      exit("")
  end.

twod(I, N) ->
  [round(I / N), I rem N].

oned(I, J, N) ->
  round(I * N + J).

randtwod(I, J, N) ->
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
    randtwod(I, J, N);
    Random_Index_1 >= 0
      andalso Random_Index_1 < N
      andalso Random_Index_2 >= 0
      andalso Random_Index_2 < N ->
      [Random_Index_1, Random_Index_2];
    true ->
      randtwod(I, J, N)
  end.

random_ind(N, Index) ->
  J = rand:uniform(N),
  if Index /= J ->
    J;
    true ->
      random_ind(N, Index)
  end.

full_network(Pids, S, W, Index) ->
  N = length(Pids),
  Random_Index = random_ind(N, Index),
  Pid = lists:nth(Random_Index, Pids),
  Pid ! {pushsumalgo, full_network, S, W, Random_Index}.

line_network(Pids, S, W, Index) ->
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

twod_network(Pids, S, W, Index) ->
  N = round(math:sqrt(length(Pids))),

  Indices = twod(Index, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  Random_Indices = randtwod(I, J, N),
  Random_Index =
    oned(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N) + 1,

  Pid = lists:nth(Random_Index, Pids),

  Pid ! {pushsumalgo, twodgrid, S, W, Random_Index}.

threed_network(Pids, S, W, Index) ->
  N = round(math:sqrt(length(Pids))),

  Indices = twod(Index, 4),
  I = lists:nth(1, Indices),
  J = lists:nth(2, Indices),
  Random_Indices = randtwod(I, J, N),
  Grid_Random_Index =
    oned(lists:nth(1, Random_Indices), lists:nth(2, Random_Indices), N) + 1,
  Random_Index = random_ind(length(Pids), Index),
  Random_Pid = lists:nth(Random_Index, Pids),
  Pid = lists:nth(Grid_Random_Index, Pids),

  Pid ! {pushsumalgo, impft2d, S, W, Grid_Random_Index},
  Random_Pid ! {pushsumalgo, impft2d, S, W, Random_Index}.

