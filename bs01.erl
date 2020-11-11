-module(bs01).
-export([first_word/1]).

first_word(Bin) ->
    first_word(Bin, <<>>).

first_word(<<" ", Rest/binary>>, <<>>) ->
    first_word(Rest, <<>>);
first_word(<<" ", _Rest/binary>>, Acc) ->
    Acc;
first_word(<<A, Rest/binary>>, Acc) ->
    first_word(Rest, <<Acc/binary, A>>);
first_word(<<>>, Acc) ->
    Acc.