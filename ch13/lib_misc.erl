-module(lib_misc).
-compile([export_all]).

%% Watch the process Pid and evaluates Fun(Why) if the process exits with the reason Why
on_exit01(Pid, Func) ->
    spawn(fun () ->
                  Start = erlang:system_time(),
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          End = erlang:system_time(),
                          Func(Pid, Start, End, Why)
                  end
          end).


%% on_exit with spawn_monitor and timeout
on_exit_aux(Mod, Func, Arity, Time) ->
    {Pid, Ref} = spawn_monitor(Mod, Func, Arity),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited, because of ~p~n", [Pid, Why]),
            on_exit_aux(Mod, Func, Arity, Time)
    after Time ->
            io:format("timeout after ~p seconds~n", [Time/1000]),
            exit(Pid, kill)
    end.
on_exit(Mod, Func, Arity, Time) ->
    spawn(fun () -> on_exit_aux(Mod, Func, Arity, Time) end).


on_exit_aux(Mod, Func, Arity) ->
    {Pid, Ref} = spawn_monitor(Mod, Func, Arity),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited, because of ~p~n", [Pid, Why]),
            on_exit_aux(Mod, Func, Arity);
        stop ->
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop
    end.
on_exit(Mod, Func, Arity) ->
    spawn(fun () -> on_exit_aux(Mod, Func, Arity) end).


on_exit_aux(F) ->
    {Pid, Ref} = spawn_monitor(F),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        stop -> % this pattern makes us could stop workers by their monitoring process
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop; % stop worker process normally
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("~p exited, because of normal~n", [Pid]);
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited abnormally, because of ~p~n", [Pid, Why]),
            on_exit_aux(F)
    end.
on_exit(F) ->
    spawn(fun () -> on_exit_aux(F) end).


on_exit_aux(F, Time) ->
    {Pid, Ref} = spawn_monitor(F),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        stop -> % this pattern makes us could stop workers by their monitoring process
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop; % stop worker process normally
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("~p exited, because of normal~n", [Pid]);
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited abnormally, because of ~p~n", [Pid, Why]),
            on_exit_aux(F, Time)
    after
        Time ->
            io:format("timeout after ~p seconds~n", [Time/1000]),
            exit(Pid, kill)
    end.
on_exit(F, Time) ->
    spawn(fun () -> on_exit_aux(F, Time) end).


%% L is a list of functions as worker
%% It returns a list of Pids which each of them monitor a worker and keep it a live.
cluster01(L) ->
    lists:reverse(cluster01(L, [])).
cluster01([], Acc)->
    Acc;
cluster01([H|T], Acc) ->
    cluster01(T, [on_exit(H)|Acc]).

stop_cluster01([]) ->
    void;
stop_cluster01([H|T]) ->
    spawn(fun () ->
                  H ! stop, % send stop to Pid, Pid could be monitor process or worker process
                  stop_cluster01(T)
          end).

%% For question 06:
%% if one of worker process is exiting with abnormally.
%% just stop cluster which stop both monitoring and monitored process
%% After make sure all monitoring process have been exit, restart cluster

create_workers(Num) ->
    Workers = [
               %% {{Pid, Ref}, I}
               spawn_monitor(worker, worker, []) || _ <- lists:seq(1, Num)
              ],
    monitor_workers(Workers).

monitor_workers(Workers) ->
    io:format("~p monitoring workers ~p~n", [self(), Workers]),
    Pids = lists:map(fun ({Pid, Ref}) -> Pid end, Workers),

    receive
        stop -> % stop all workers
            io:format("stop all workers~n"),
            stop_workers(Pids);
        restart ->
            io:format("restart all workers~n"),
            restart_workers(Pids, nil);
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("~p exited, because of normal~n", [Pid]),
            monitor_workers(Pids);
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited abnormally, because of ~p~n", [Pid, Why]),
            io:format("restart all workers~n"),
            restart_workers(Pids, Pid);
        _ ->
            monitor_workers(Pids)
    end.


restart_workers(Pids, Pid) ->
    N = length(Pids),
    StoppingWorkers = lists:filter(fun (X) -> X =/= Pid end, Pids),
    stop_workers(StoppingWorkers),
    io:format("~p have been stopped~n", [StoppingWorkers]),
    spawn(fun () ->
                  create_workers(N)
          end).

stop_workers(Pids) ->
    [Pid ! stop || Pid <- Pids].