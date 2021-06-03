-module(multiproc).
-compile([export_all]).

sleep(T) ->
    receive
    after
        T -> ok
    end.

flush() ->
    receive
        _ ->
            flush()
    after
        0 -> ok
    end.
