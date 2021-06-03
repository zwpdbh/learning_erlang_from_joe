-module(area_server_final).
-export([loop/0, area/2, start/0, rpc/2]).

start() ->
    %% we also need to export loop to spawn
    spawn(area_server_final, loop, []).

area(Pid, What) ->
    rpc(Pid, What).

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