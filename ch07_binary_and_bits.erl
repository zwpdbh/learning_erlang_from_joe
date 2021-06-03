-module(ch07_binary_and_bits).

%% MPEG audio data = a number of frames.
%% Each frame = header + audio information
%% Frame header = 11-bits frame sync  + 2-bits Audio version + 2-bits layer desc + 1 bit, protection bit.

%% look for three consecutive headers
find_sync(Bin, N) ->
    case is_header(N, Bin) of 
        {ok, Len1, _} ->
            case is_header(N + Len1, Bin) of 
                {ok, Len2, _} ->
                    case is_header(N + Len1 + Len2, Bin) of
                        {ok, _, _} ->
                            {ok, N};
                        error -> 
                            find_sync(Bin, N+1)
                    end;
                error ->
                    find_sync(Bin, N+1)
            end;
        error ->
            find_sync(Bin, N+1)
    end.

is_header(N, Bin) ->
    unpack_header(get_word(N, Bin)).

%% We extract 32 bits of data
get_word(N, Bin) ->
    % remember binary is size 8, so C is 4 * 8 = 32 bits 
    {_, <<C:4/binary, _/binary>>} = split_binary(Bin, N),
    C.

unpack_header(X) ->
    try 
        decode_header(X)
    catch
        _:_ ->
            error
    end.

%% Use pattern matching is so common during function definition.
%% Here, we define the function logic for correct inputs
decode_header(<<2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, Bits:9>>) ->
    Vsn = case B of
              0 -> {2, 5};
              1 -> exit(badVsn);
              2 -> 2;
              3 -> 1
          end,
    Layer = case C of
                0 -> exit(badLayer);
                1 -> 3;
                2 -> 2;
                3 -> 1
            end,
    
    BitRate = bitrate(Vsn, Layer, E) * 1000,
    SampleRate = samplerate(Vsn, F),
    Padding = G,
    FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
    if 
        FrameLength < 21 ->
            exit(frameSize);
        true ->
            {ok, FrameLength, {Layer, BitRate, SampleRate, Vsn, Bits}}
    end;

%% Means for everything else, it will return badHeader
decode_header(_) ->
    exit(badHeader).    
