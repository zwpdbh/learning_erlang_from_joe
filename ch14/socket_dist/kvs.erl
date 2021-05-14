-module(kvs).
-export([start/0, store/2, lookup/1]).

%% -spec kvs:start() -> true.
%% -spec kvs:store(Key, Value) -> true.
%% -spec kvs:lookup(Key) -> {ok, Value} | undefined.

start() ->
    %% spawn the server process since it keeps waiting on messages.
    register(kvs, spawn(fun () -> loop() end)).

%% Encapsulate sending a request to a server and waiting for a response
store(Key, Value) ->
    rpc({store, Key, Value}).

%% Encapsulate sending a request to a server and waiting for a response
lookup(Key) ->
    rpc({lookup, Key}).

rpc(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs, Reply} ->
            Reply
    end.

%% A server is just a function keep waiting messages.
loop() ->
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs, get(Key)},
            loop()
    end.
