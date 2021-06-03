-module(note13).
-compile([export_all]).


%% Watch the process Pid and evaluates Fun(Why) if the process exits with the reason Why
on_exit(Pid, Fun) ->
    spawn(fun () ->
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          Fun(Pid, Why)
                  end
          end).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun (_Why) -> keep_alive(Name, Fun) end).

print(Pid, Why) ->
    io:format(">> ~p died with: ~p~n", [Pid, Why]).


bad_f() ->
    spawn(fun () ->
                  receive
                      X ->
                          list_to_atom(X)
                  end
          end).
