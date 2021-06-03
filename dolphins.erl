-module(dolphins).
-export([dolphin1/0]).

dolphin1() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?~n";
        {From, fish} ->
            From ! "so long and thanks for all the fish!~n";
        _ ->
            io:format("Heh, we're smarter than you humans.~n"),
            dolphin1()
    end.