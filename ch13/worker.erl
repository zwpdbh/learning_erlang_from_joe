-module(worker).
-compile([export_all]).

%% Run a worker
worker() ->
    io:format("I am worker ~p~n", [self()]),
    receive
        Message ->
            io:format("receive: ~p~n", [Message]),
            worker()
    after 10000 ->
            worker()
    end.

%% Start to run a worker on a process
start_worker() ->
    spawn(fun () -> worker() end).