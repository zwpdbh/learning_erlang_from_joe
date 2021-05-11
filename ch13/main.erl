-module(main).
-compile([export_all]).

log(Pid, Start, End, Why) ->
    io:format("~p lived for ~p, died because of ~p~n", [Pid, End - Start, Why]).

%% start a function and print out how long it has been lived
q02() ->
    Pid = spawn(worker, worker, []),
    lib_misc:on_exit01(Pid, fun log/4).

%% start a function and kill its execution after a certain time.
q03() ->
    lib_misc:on_exit(worker, worker, [], 10000).

%% monitor a function and restart it if it dies
q04() ->
    lib_misc:on_exit(worker, worker, []).

%% starts and monitor several workers. If a worker failed, restart it.
%% test with Num work, all use the worker function as work.
q05(Num) ->
    lib_misc:cluster01([fun worker:worker/0 || _ <- lists:seq(1, Num)]).


%% starts and monitor several worker process. If any of the worker process die abnormally,
%% Kill all the worker processes and restart them all.
q06(Time) ->
    lib_misc:on_exit(fun () ->
                             lib_misc:on_exit(fun () ->
                                                      worker:worker()
                                              end, Time)
                     end).