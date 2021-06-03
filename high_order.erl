-module(high_order).
-export([fold/3, filter/2, filter/3, map/2]).

map(_, []) ->
    [];
map(F, [H|T]) ->
    [F(H) | map(F, T)].
%% high_order:map(fun(X) -> X * X end, lists:seq(1, 4)).

fold(_, Start, []) ->
    Start;
fold(F, Start, [H|T]) ->
    fold(F, F(H, Start), T).
%% high_order:fold(fun(X, Y) -> X + Y end, 0, lists:seq(1, 10)).

filter(Pred, L) ->
    lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) ->
    Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        false ->
            filter(Pred, T, Acc);
        true ->
            filter(Pred, T, [H|Acc])
    end.
%% high_order:filter(fun(X) -> X  rem 2 == 0 end, lists:seq(1,8)).