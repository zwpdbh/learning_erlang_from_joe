-module(example).
-compile([export_all]).


worker(Id) ->
    timer:sleep(1000 * rand:uniform(5)),
    io:format("Worker~w: I'm still alive~n", [Id]),
    worker(Id).

create_workers(N) ->
    Workers = [  % { {Pid, Ref}, Id }
                 { spawn_monitor(?MODULE, worker, [Id]), Id }
                 || Id <- lists:seq(1, N)
              ],
    monitor_workers(Workers).

monitor_workers(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            Worker = {Pid, Ref},
            case is_my_worker(Worker, Workers) of
                true  ->
                    NewWorkers = replace_worker(Worker, Workers, Why),
                    io:format("Old Workers:~n~p~n", [Workers]),
                    io:format("New Workers:~n~p~n", [NewWorkers]),
                    monitor_workers(NewWorkers);
                false ->
                    monitor_workers(Workers)
            end;
        _Other ->
            monitor_workers(Workers)
    end.

is_my_worker(Worker, Workers) ->
    lists:keymember(Worker, 1, Workers).

replace_worker(Worker, Workers, Why) ->
    {{Pid, _}, Id} = lists:keyfind(Worker, 1, Workers),
    io:format("Worker~w (~w) went down: ~s~n", [Id, Pid, Why]),
    NewWorkers = lists:keydelete(Worker, 1, Workers),
    NewWorker = spawn_monitor(?MODULE, worker, [Id]),
    [{NewWorker, Id}|NewWorkers].

start() ->
    observer:start(),  %%In the Processes tab, you can right click on a worker and kill it.
    create_workers(4).