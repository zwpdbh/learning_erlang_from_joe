-module(shop).
-export([cost/1, total/1, test/0]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

total([{What, N}|T]) ->
    cost(What) * N + total(T);
total([]) -> 0.

test() ->
    Buy = [{oranges,4}, {newspaper,1}, {apples,10}, {pears,6}, {milk,3}],
    total(Buy).