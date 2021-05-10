-module(q02).
-compile([export_all]).
-import(worker, [start_worker/0]).

start() ->
    start_worker().