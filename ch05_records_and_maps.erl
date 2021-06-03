-module(ch05_records_and_maps).
-export([count_characters/1]).

%% function count the number of characters in a string
count_characters(Str) ->
    count_characters(Str, #{}).

count_characters([H|T], Counts) ->
    Count = maps:get(H, Counts, 0),
    count_characters(T, maps:put(H, Count + 1, Counts));
count_characters([], Counts) ->
    Counts.
