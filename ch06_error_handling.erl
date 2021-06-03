-module(ch06_error_handling).
-export([demo1/0, sqrt/1]).

generate_exception(1) ->
    a;
generate_exception(2) ->
    throw(a);
generate_exception(3) ->
    exit(a);
generate_exception(4) ->
    {'Exit', a};
generate_exception(5) ->
    error(a);
generate_exception(6) ->
    error(a).


%% shows use try ... catch to trap and distinguish all the forms of exceptions that a function can raise.
demo1()->
    [catcher(I) || I <- [1,2,3,4,5,6]].

catcher(N) ->
    try generate_exception(N) of
        Val ->
             {N, normal, Val}
    catch
        throw:X ->
            {N, caught, thrown, X};
        exit:X -> 
            {N, caught, exited, X};
        error:X ->
            {N, caught, error, X};
        error:X ->
            {X, erlang:get_stacktrace()}
    end.    

sqrt(X) when X < 0 ->
    error({sqrtRootDetectNegativeArg, X});
sqrt(X) ->
    math:sqrt(X).