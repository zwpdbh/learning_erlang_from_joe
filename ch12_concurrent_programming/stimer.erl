-module(stimer).
-export([start/2, cancel/1]).

%% Like setTimeout in javascript with extra ability to cancel it.
start(Time, Fun) ->
    spawn(fun () -> timer(Time, Fun) end).

cancel(Pid) ->
    Pid ! cancel.

timer(Time, Fun) ->
    receive
        cancel ->
            void
    after Time ->
            Fun()
    end.