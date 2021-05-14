-module(mod_name_server).
-export([start_me_up/3]).

start_me_up(MM, _ArgsC, _ArgsS) ->
    loop(MM).

loop(MM) ->
    receive
        {chan, MM, {store, K, V}} ->
            kvs:store(K, V),
            loop(MM);
        {chan, MM, {lookup, K}} ->
            MM ! {second, kvs:lookup(K)},
            loop(MM);
        {chan_closed, MM} ->
            true
    end.
