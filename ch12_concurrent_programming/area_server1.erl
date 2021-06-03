-module(area_server1).
-export([loop/0, rpc/2]).

%% Encapsulate sending a request to a server and waiting for a response
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive 
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width * Ht},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, {circle, R}} ->
            From ! {self(), 3.1415926 * R * R},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.