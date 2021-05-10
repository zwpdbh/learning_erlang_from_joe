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
        stop -> % this pattern makes us could stop workers by their monitoring process
            io:format("stop monitored process ~p~n", [Pid]),
            Pid ! stop; % stop worker process normally
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("~p exited, because of normal~n", [Pid]);
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
                  H ! stop, % send stop to Pid, Pid could be monitor process or worker process
                  stop_cluster01(T)
          end).



%% %% the only differences between it with above keep_alive is that it will restart all workers
%% keep_all_aux(F) ->
%%     {Pid, Ref} = spawn_monitor(F),
%%     io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
%%     receive
%%         stop ->
%%             io:format("stop monitored process ~p~n", [Pid]),
%%             Pid ! stop;
%%         {'DOWN', Ref, process, Pid, normal} ->
%%             io:format("~p exited, because of normal", [Pid]);
%%         {'DOWN', Ref, process, Pid, Why} ->
%%             io:format("~p exited abnormally, because of ~p~n", [Pid, Why]),
%%             stop_cluster
%%     end.
%% keep_all(F) ->
%%     spawn(fun () -> keep_all_aux(F) end).



%% cluster02(L) ->
%%     lists:reverse(cluster02(L, [])).
%% cluster02([], Acc) ->
%%     Acc;
%% cluster02([H|T], Acc) ->
%%     cluster02(T, [spawn(fun H)|Acc]).

%% monitor_cluster02(L) ->
%%     monitor_cluster02(L, []).
%% monitor_cluster02([], Acc) ->
%%     Acc;
%% monitor_cluster02([Pid|T], Acc) ->

%%     kill_all_alive