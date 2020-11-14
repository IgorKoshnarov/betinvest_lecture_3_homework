-module(bs03).
-export([split/2]).

split(Bin, Splitter) ->
    reverse_and_clean(split(Bin, list_to_bin(Splitter), <<>>, <<>>, [])).

split(Bin, <<>>, Splitter, Word, List) ->
    split(Bin, Splitter, <<>>, <<>>, [Word | List]);
split(<<A/utf8, Rest/binary>>, <<A/utf8, Rest1/binary>>, Bin, Word, List) ->
    split(Rest, Rest1, <<Bin/binary, A/utf8>>, Word, List);
split(<<A/utf8, Rest/binary>>, Rest1, Bin, Word, List) ->
    split(Rest, <<Bin/binary, Rest1/binary>>, <<>>, <<Word/binary, Bin/binary, A/utf8>>, List);
split(<<>>, _, Bin, Word, List) ->
    [<<Word/binary, Bin/binary>> | List].

list_to_bin(L) ->
    list_to_bin(L, <<>>).

list_to_bin([H | T], Acc) ->
    list_to_bin(T, <<H/utf8, Acc/binary>>);
list_to_bin([], Acc) ->
    Acc.


reverse_and_clean(L) ->
    reverse_and_clean(L, []).

reverse_and_clean([<<>> | T], L) ->
    reverse_and_clean(T, L);
reverse_and_clean([H | T], L) ->
    reverse_and_clean(T, [H | L]);
reverse_and_clean([], L) ->
    L.