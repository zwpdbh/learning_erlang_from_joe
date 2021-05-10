-module(lib_misc).
-compile([export_all]).

%% Watch the process Pid and evaluates Fun(Why) if the process exits with the reason Why
on_exit(Pid, Func) ->
    spawn(fun () ->
                  Start = erlang:system_time(),
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          End = erlang:system_time(),
                          Func(Pid, Start, End, Why)
                  end
          end).


on_timeout_aux(Mod, Func, Arity, Time) ->
    {Pid, Ref} = spawn_monitor(Mod, Func, Arity),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited, because of ~p~n", [Pid, Why]),
            on_timeout_aux(Mod, Func, Arity, Time)
    after Time ->
            io:format("timeout after ~p seconds~n", [Time/1000]),
            exit(Pid, kill)
    end.
on_timeout(Mod, Func, Arity, Time) ->
    spawn(fun () -> on_timeout_aux(Mod, Func, Arity, Time) end).


keep_alive_aux(Mod, Func, Arity) ->
    {Pid, Ref} = spawn_monitor(Mod, Func, Arity),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited, because of ~p~n", [Pid, Why]),
            keep_alive_aux(Mod, Func, Arity);
        stop ->
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop
    end.
keep_alive(Mod, Func, Arity) ->
    spawn(fun () -> keep_alive_aux(Mod, Func, Arity) end).


keep_alive_aux(F) ->
    {Pid, Ref} = spawn_monitor(F),
    io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
    receive
        stop ->
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop;
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("~p exited, because of normal", [Pid]);
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p exited abnormally, because of ~p~n", [Pid, Why]),
            keep_alive_aux(F)
    end.
keep_alive(F) ->
    spawn(fun () -> keep_alive_aux(F) end).


%% L is a list of functions as worker
%% It returns a list of Pids which each of them monitor a worker and keep it a live.
cluster01(L) ->
    lists:reverse(cluster01(L, [])).
cluster01([], Acc)->
    Acc;
cluster01([H|T], Acc) ->
    cluster01(T, [keep_alive(H)|Acc]).

stop_cluster01([]) ->
    void;
stop_cluster01([H|T]) ->
    spawn(fun () ->
                  H ! stop,
                  stop_cluster01(T)
          end).
