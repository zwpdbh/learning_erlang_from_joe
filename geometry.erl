-module(geometry).
-export([area/1, test/0]).

area({rectange, Width, Height}) ->
    Width * Height;

area({square, Side}) -> Side * Side;

area({circle, Radius}) ->
    3.1415926 * Radius * Radius.


test() ->
    12 = area({rectange, 3, 4}),
    144 = area({square, 12}),
    tests_worked.
