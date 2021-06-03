-module(geometry_client).
-export([client/2]).

client(Pid_server, Geom_tuple) ->
    Server = whereis(Pid_server),
    Server ! {self(), Geom_tuple},
    receive
        {Server, Geom_tuple, Area} -> io:format("Client: Area of ~p is ~p and  server was ~p~n", [Geom_tuple, Area, Server])

    after 1000 ->
            io:format("~p~n",["received nothing from server"] )
    end.
