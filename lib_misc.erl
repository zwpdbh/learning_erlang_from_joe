-module(lib_misc).
-export([for/3, qsort/1, perms/1, filter/2, odds_and_evens2/1]).

for(Max, Max, F) ->
    [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

qsort([]) ->
    [];
qsort([Pivot|T]) ->
    qsort([X || X <-T, X < Pivot])
        ++ [Pivot] ++
        qsort([X || X <-T, X >= Pivot]).

perms([])->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L -- [H])].


filter(P, [H|T]) ->
    filter1(P(H), H, P, T);
filter(_, []) -> [].

filter1(true, H, P, T) ->
    [H|filter(P, T)];
filter1(false, _, P, T) ->
    filter(P, T).


odds_and_evens2(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 ->
            odds_and_evens_acc(T, [H|Odds], Evens);
        0 ->
            odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) -> {lists:reverse(Odds), lists:reverse(Evens)}.

%% from ch12
flush_buffer() ->
    receive
        _Any ->
            flush_buffer()
    after 0 ->
            true
    end.
