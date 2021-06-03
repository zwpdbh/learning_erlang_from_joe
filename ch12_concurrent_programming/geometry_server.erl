-module(geometry_server).
-export([loop/0, start_server/0]).

loop() ->
    receive
        {Client, {square, S} = Tuple} ->
            io:format("Server: Area of square of Side ~p is ~p and Client was ~p~n", [S, S*S, Client]),
            Client ! {self(), Tuple,  S*S},
            loop()
    end.

start_server() ->    spawn(geometry_server, loop, []).