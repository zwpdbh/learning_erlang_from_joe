-module(error_and_process).
-compile([export_all]).

myproc() ->
    timer:sleep(5000),
    exit(reason). %  this kind of error message can not be caught with try ... catch

%% links die together
chain(0) ->
    receive
        _ ->
            ok
    after
        2000 -> exit("chain dies here")
    end;
chain(N) ->
    %% Pid = spawn(fun () -> chain(N-1) end), % same as spawn(?MODULE, chain, [N-1])
    %% link(Pid),
    spawn_link(?MODULE, chain, [N-1]),
    receive
        _ ->
            ok
    end.
