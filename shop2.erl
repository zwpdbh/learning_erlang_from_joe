-module(shop2).
-export([total/1, test/0]).
-import(lists, [map/2, sum/1]).

total(L) ->
    sum(map(fun ({What, N}) -> shop:cost(What) * N end, L)).

test() ->
    Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}],
    123 = total(Buy),
    worked.    