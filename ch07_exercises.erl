-module(ch07_exercises).
-export([reverse_binary/2, reverse_bits/2]).

%% Acc is <<>>
reverse_binary(<<>>, Acc) ->
    Acc;
reverse_binary(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse_binary(Rest, <<H/binary, Acc/binary>>).

reverse_bits(<<>>, Acc) ->
    Acc;
reverse_bits(<<H:1/bits, Rest/bits>>, Acc) ->
    reverse_bits(Rest, <<H/bits, Acc/bits>>).
