-module(worker).
-compile([export_all]).

%% Run a worker
worker() ->
    io:format("I am worker ~p~n", [self()]),
    receive
        stop ->
            io:format("received stop, so stop work now~n");
        Message ->
            io:format("receive: ~p~n", [Message]),
            worker()
    after 2000 ->
            worker()
    end.