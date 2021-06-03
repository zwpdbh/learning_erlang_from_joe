-module(ex13).
-compile([export_all]).


bad_f() ->
    receive
        X ->
            list_to_atom(X)
    end.


debug(Pid, Start, End, Why)->
    io:format("~p lived for ~p, died because of ~p~n", [Pid, End - Start, Why]).


my_on_exit(F, Handler)  ->
    spawn(fun () ->
                  {Pid, Ref} = spawn_monitor(F),
                  io:format("waiting message from{~p, ~p}~n", [Pid, Ref]),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          io:format("~p exited, because of ~p~n", [Pid, Why]),
                          Handler()
                  end
          end).



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

%% For ex2. Spawn a process and when that process died, print out how long it have been lived and why it is died.
my_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    on_exit(Pid, fun debug/4),
    Pid.

%% For ex3. Spawn a process and kill it automatically after Time period
my_spawn(Mod, Func, Args, Time) ->
    Pid = spawn(Mod, Func, Args),
    spawn(fun () ->
                  receive
                  after Time ->
                          io:format("You lived too long, you have to die.~n"),
                          exit(Pid, kill)
                  end
          end).


%% For ex4: a function that create a registered process and write out message every five seconds.
start_tick()->
    register(clock, Pid = spawn(fun () ->  tick() end)),
    Pid.


stop_tick() ->
    clock ! stop.
tick() ->
    receive
        stop ->
            void
    after 5000 ->
            io:format("I(~p) am still alive~n", [self()]),
            tick()
    end.

monitor_func4() ->
    spawn(fun () ->
                  Pid = whereis(clock),
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, normal} ->
                          io:format("~p exit normally~n", [Pid]),
                          void;
                      {'DOWN', Ref, process, Pid, Why} ->
                          io:format("Restart func since it exit because of ~p.~n", [Why]),
                          start_tick()
                  end
          end).

%% For question5: Write a function that starts and monitor several worker processes.
%% If any of the worker process die abnormally, restart it.
monitor_func(F)  ->
    {Pid, Ref} = spawn_monitor(F),
    spawn(fun () ->
                  receive
                      {'DOWN', Ref, process, Pid, stop} ->
                          io:format("~p stop normally~n", [Pid]),
                          void;
                      {'DOWN', Ref, process, Pid, normal} ->
                          io:format("~p exit normally~n", [Pid]),
                          void;
                      {'DOWN', Ref, process, Pid, Why} ->
                          io:format("Restart func since it exit because of ~p.~n", [Why]),
                          start_func(F)
                  end

          end).



start_func(F) ->
    spawn(fun () -> F() end).

%% L is a list of function references
master(L) ->
    lists:reverse(master(L, [])).
master([], Acc) ->
    Acc;
master([H|T], Acc) ->
    monitor_func(H),
    master(T, [H|Acc]).

gen_workers(Num) ->
    [fun worker/0 || _ <- lists:seq(1,Num)].

%% start_worker(Mark) ->
%%     spawn(fun ()-> worker(Mark) end).
worker() ->
    io:format("I am worker ~p~n", [self()]),
    receive
        Message ->
            io:format("receive: ~p~n", [Message]),
            worker()
    after 10000 ->
            worker()
    end.


%% For question6: start and monitor several worker processes. If any of them dies abnormally
%% Kill all the workers processes and restart them all.