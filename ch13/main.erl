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
    observer:start(),
    lib_misc:cluster01([fun worker:worker/0 || _ <- lists:seq(1, Num)]).


%% starts and monitor several worker process. If any of the worker process die abnormally,
%% Kill all the worker processes and restart them all.
q06(Num) ->
    observer:start(),
    spawn(fun () ->  lib_misc:create_workers(Num) end).

%% spawn(fun () ->  lib_misc:monitor_workers(Pids) end).


test() ->
    L = [1, 2, 3, 4, 5],
    lists:filter(fun (X) -> X =/= nil end, L).
