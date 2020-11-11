-module(bs02).
-export([words/1]).

words(Bin) ->
    reverse(words(Bin, <<>>, [])).

words(<<" ", Rest/binary>>, Word, List) ->
    words(Rest, <<>>, [Word | List]);
words(<<A/utf8, Rest/binary>>, Word, List) ->
    words(Rest, <<Word/binary, A/utf8>>, List);
words(<<>>, Word, List) ->
    [Word | List].

reverse(L) ->
    reverse(L, []).

reverse([H | T], L) ->
    reverse(T, [H | L]);
reverse([], L) ->
    L.