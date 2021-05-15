-module(ports).
-export([start/1, stop/0, init/1]).
-export([send/1]).

start(ExtPrg) ->
    spawn_link(?MODULE, init, [ExtPrg]).
stop() ->
    port ! stop.

send(Y) -> call_port({msg, Y}).
call_port({msg, Msg}) ->
    port ! {call, self(), Msg},
    receive
        {port, Result} ->
            Result
    end.

init(ExtPrg) ->
    register(port, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, []),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, Msg++[10]}},
            Data = receive_all(Port, 100),
            Caller ! {port, Data},
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason})
    end.

receive_all(Port, Timeout) -> receive_all(Port, Timeout, []).
receive_all(Port, Timeout, Buffer) ->
    receive
        {Port, {data, Data}} ->
            receive_all(Port, Timeout, [Data | Buffer])
    after Timeout ->
            lists:flatten(lists:reverse(Buffer))
    end.