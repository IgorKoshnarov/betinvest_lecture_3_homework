-module(bs04).
-export([decode/2]).
-export([test/0, test/1]).

%%%%%%%%%%%%%%%%%%
%%  JSON decoder.
%%  The json text is parsed character by character,
%%  some characters are just accumulated in the <Acc> and 
%%  some characters also change the decoder's state.
%%  The decoder's state is tracked in a list called <Stack>.
%%  Valid states of the decoder are:
%%  arr, elem, obj, key, val, str, esc, num, float. 
%%  arr - array found
%%  elem - element of an array found
%%  obj - object found
%%  key - key of the object found
%%  val - value of the object's key found
%%  str - elem, key or value is a string
%%  esc - special escaped symbol found
%%  num - numeric value found
%%  float - float number value found
%%%%%%%%%%%%%%%%%%

decode(Json, proplist) ->
    decode(Json, []);
decode(<<"[", Rest/binary>>, Acc) ->
    decode(Rest, [arr], Acc);
decode(<<"{", Rest/binary>>, Acc) ->
    decode(Rest, [obj], Acc);
decode(Json, map) ->
    list_to_map(decode(Json, proplist)).

%% double quote opens string context
decode(<<"\"", Rest/binary>>, [arr | _] = Stack, Acc) ->
    decode(Rest, [str, elem | Stack], [<<>> | Acc]);
decode(<<"\"", Rest/binary>>, [obj | _] = Stack, Acc) ->
    decode(Rest, [str, key | Stack], [<<>> | Acc]);
decode(<<"\"", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, [str | Stack], [<<>> | Acc]);
decode(<<"\"", Rest/binary>>, [key | _] = Stack, Acc) ->
    decode(Rest, [str | Stack], [<<>> | Acc]);
decode(<<"\"", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, [str | Stack], [<<>> | Acc]);
%% double quote closes string context
decode(<<"\"", Rest/binary>>, [str | St], Acc) ->
    decode(Rest, St, Acc);
%% inside string context - special characters escaped
decode(<<"\\", Rest/binary>>, [str | _] = Stack, Acc) ->
    decode(Rest, [esc | Stack], Acc);
decode(<<"\\", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\\">> | T]);
decode(<<"\/", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\/">> | T]);
decode(<<"n", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\n">> | T]);
decode(<<"t", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\t">> | T]);
decode(<<"b", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\b">> | T]);
decode(<<"r", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\r">> | T]);
decode(<<"f", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\f">> | T]);
decode(<<"\"", Rest/binary>>, [esc | St], [H | T]) ->
    decode(Rest, St, [<<H/binary, "\"">> | T]);
decode(<<"u", A0, A1, A2, A3, Rest/binary>>, [esc | St], [H | T]) ->
    S = unicode_seq(A0, A1, A2, A3),
    decode(Rest, St, [<<H/binary, S/utf16>> | T]);
%% inside string context - general characters
decode(<<A/utf8, Rest/binary>>, [str | _] = Stack, [H | T]) ->
    decode(Rest, Stack, [<<H/binary, A/utf8>> | T]);
%% numeric values start from "-" or digit and can be found in arr, elem, val contexts
decode(<<"-", Rest/binary>>, [arr | _] = Stack, Acc) ->
    decode(Rest, [num, elem | Stack], [<<"-">> | Acc]);
decode(<<"-", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, [num, elem | Stack], [<<"-">> | Acc]);
decode(<<"-", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, [num | Stack], [<<"-">> | Acc]);
decode(<<A, Rest/binary>>, [arr | _] = Stack, Acc) when (A >= 48) andalso (A =< 57) ->
    decode(Rest, [num, elem | Stack], [<<A>> | Acc]);
decode(<<A, Rest/binary>>, [elem | _] = Stack, Acc) when (A >= 48) andalso (A =< 57) ->
    decode(Rest, [num | Stack], [<<A>> | Acc]);
decode(<<A, Rest/binary>>, [val | _] = Stack, Acc) when (A >= 48) andalso (A =< 57) ->
    decode(Rest, [num | Stack], [<<A>> | Acc]);
decode(<<A, Rest/binary>>, [num | _] = Stack, [H | T]) when (A >= 48) andalso (A =< 57) ->
    decode(Rest, Stack, [<<H/binary, A>> | T]);
%% numeric values containing "." are floats, they can contain mantissa
decode(<<".", Rest/binary>>, [num | St], [H | T]) ->
    decode(Rest, [float | St], [<<H/binary, ".">> | T]);
decode(<<"e", Rest/binary>>, [float | _] = Stack, [H | T]) ->
    decode(Rest, Stack, [<<H/binary, "e">> | T]);
decode(<<"E", Rest/binary>>, [float | _] = Stack, [H | T]) ->
    decode(Rest, Stack, [<<H/binary, "e">> | T]);
decode(<<"-", Rest/binary>>, [float | _] = Stack, [H | T]) ->
    decode(Rest, Stack, [<<H/binary, "-">> | T]);
decode(<<"+", Rest/binary>>, [float | _] = Stack, [H | T]) ->
    decode(Rest, Stack, [<<H/binary, "+">> | T]);
decode(<<A, Rest/binary>>, [float | _] = Stack, [H | T]) when (A >= 48) andalso (A =< 57) ->
    decode(Rest, Stack, [<<H/binary, A>> | T]);
decode(<<",", Rest/binary>>, [num, elem | St], [H | T]) ->
    decode(Rest, [elem, elem | St], [element(1, string:to_integer(H)) | T]);
decode(<<",", Rest/binary>>, [float, elem | St], [H | T]) ->
    decode(Rest, [elem, elem | St], [element(1, string:to_float(H)) | T]);   
decode(<<",", Rest/binary>>, [num, val | St], [H | T]) ->
    decode(Rest, [key, val | St], [element(1, string:to_integer(H)) | T]);
decode(<<",", Rest/binary>>, [float, val | St], [H | T]) ->
    decode(Rest, [key, val | St], [element(1, string:to_float(H)) | T]); 
%% coma after array element or key value pair creates new context
decode(<<",", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, [elem | Stack], Acc); 
decode(<<",", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, [key | Stack], Acc); 
%% json values in context arr, elem, val, can be true, false or null
decode(<<"true", Rest/binary>>, [arr | _] = Stack, Acc) ->
    decode(Rest, [elem | Stack], [true | Acc]);
decode(<<"false", Rest/binary>>, [arr | _] = Stack, Acc) ->
    decode(Rest, [elem | Stack], [false | Acc]);
decode(<<"null", Rest/binary>>, [arr | _] = Stack, Acc) ->
    decode(Rest, [elem | Stack], [null | Acc]);
decode(<<"true", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, Stack, [true | Acc]);
decode(<<"false", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, Stack, [false | Acc]);
decode(<<"null", Rest/binary>>, [elem | _] = Stack, Acc) ->
    decode(Rest, Stack, [null | Acc]);
decode(<<"true", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, Stack, [true | Acc]);
decode(<<"false", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, Stack, [false | Acc]);
decode(<<"null", Rest/binary>>, [val | _] = Stack, Acc) ->
    decode(Rest, Stack, [null | Acc]);
%% colon after key creates context val
decode(<<":", Rest/binary>>, [key | _] = Stack, Acc) ->
    decode(Rest, [val | Stack], Acc);
%% spaces, \n, \r, \t are ignored everywhere except str context
decode(<<" ", Rest/binary>>, Stack, Acc) ->
    decode(Rest, Stack, Acc);
decode(<<"\n", Rest/binary>>, Stack, Acc) ->
    decode(Rest, Stack, Acc);
decode(<<"\t", Rest/binary>>, Stack, Acc) ->
    decode(Rest, Stack, Acc);
decode(<<"\r", Rest/binary>>, Stack, Acc) ->
    decode(Rest, Stack, Acc);
%% nested arrays and object go into context
decode(<<"[", Rest/binary>>, [arr |St], Acc) ->
    decode(Rest, [arr, elem, arr | St], Acc);
decode(<<"[", Rest/binary>>, Stack, Acc) ->
    decode(Rest, [arr | Stack], Acc);
decode(<<"{", Rest/binary>>, [arr | St], Acc) ->
    decode(Rest, [obj, elem, arr | St], Acc);
decode(<<"{", Rest/binary>>, Stack, Acc) ->
    decode(Rest, [obj | Stack], Acc);
%% nested arrays and objects go out of context
decode(<<"]", Rest/binary>>, [num, elem | St], [H | T]) ->
    decode(Rest, [element(1, string:to_integer(H))], St, T);
decode(<<"]", Rest/binary>>, [float, elem | St], [H | T]) ->
    decode(Rest, [element(1, string:to_float(H))], St, T);
decode(<<"]", Rest/binary>>, [elem | St], [H | T]) ->
    decode(Rest, [H], St, T);
decode(<<"]", Rest/binary>>, [arr, val | St], Acc) ->
    decode(Rest, [val | St], [[] | Acc]);
decode(<<"]", Rest/binary>>, [arr, elem | St], Acc) ->
    decode(Rest, [elem | St], [[] | Acc]);
decode(<<"]", Rest/binary>>, [arr, arr | St], Acc) ->
    decode(Rest, [elem, arr | St], [[] | Acc]);
decode(<<"]", Rest/binary>>, [arr], Acc) ->
    decode(Rest, [], Acc);
decode(<<"}", Rest/binary>>, [num, val, key | St], [V, K | T]) ->
    decode(Rest, [{K, element(1, string:to_integer(V))}], St, T);
decode(<<"}", Rest/binary>>, [float, val, key | St], [V, K | T]) ->
    decode(Rest, [{K, element(1, string:to_float(V))}], St, T);
decode(<<"}", Rest/binary>>, [val, key | St], [V, K | T]) ->
    decode(Rest, [{K, V}], St, T);
decode(<<"}", Rest/binary>>, [obj, val | St], Acc) ->
    decode(Rest, [val | St], [[] | Acc]);
decode(<<"}", Rest/binary>>, [obj, elem | St], Acc) ->
    decode(Rest, [elem | St], [[] | Acc]);
decode(<<"}", Rest/binary>>, [obj], Acc) ->
    decode(Rest, [], Acc);
decode(<<>>, [elem | _] = Stack, Acc) ->
    decode(<<>>, [], Stack, Acc);
decode(<<>>, [arr], []) ->
    [];
decode(<<>>, [obj], []) ->
    [];
decode(<<>>, [], []) ->
    [];
decode(<<>>, [val, key | _] = Stack, Acc) ->
    decode(<<>>, [], Stack, Acc).

%% handle nested arrays and objects
decode(<<>>, Acc1, [elem | St], [H | T]) ->
    decode(<<>>, [H | Acc1], St, T);
decode(<<>>, Acc1, [val, key | St], [V, K | T]) ->
    decode(<<>>, [{K, V} | Acc1], St, T);
decode(<<>>, Acc1, [arr], _Acc) ->
    Acc1;
decode(<<>>, Acc1, [obj], _Acc) ->
    Acc1;
decode(<<>>, Acc1, [], []) ->
    Acc1;
decode(Bin, Acc1, [elem | St], [H | T]) ->
    decode(Bin, [H | Acc1], St, T);
decode(Bin, Acc1, [arr, elem | St], Acc) ->
    decode(Bin, [elem | St], [Acc1 | Acc]);
decode(Bin, Acc1, [arr, val | St], Acc) ->
    decode(Bin, [val | St], [Acc1 | Acc]);
decode(Bin, Acc1, [arr, arr | St], [H, T]) ->
    decode(Bin, [arr | St], [[H | Acc1] | T]);
decode(Bin, Acc1, [val, key | St], [V, K | T]) ->
    decode(Bin, [{K, V} | Acc1], St, T);
decode(Bin, Acc1, [obj, arr | St], [H | T]) ->
    decode(Bin, [arr | St], [[H | Acc1] | T]);
decode(Bin, Acc1, [obj, elem | St], Acc) ->
    decode(Bin, [elem | St], [Acc1 | Acc]);
decode(Bin, Acc1, [obj, val | St], Acc) ->
    decode(Bin, [val | St], [Acc1 | Acc]).

unicode_seq(A0, A1, A2, A3) ->
    hex_to_int(A3) bor
    (hex_to_int(A2) bsl 4) bor
    (hex_to_int(A1) bsl 8) bor
    (hex_to_int(A0) bsl 12).

hex_to_int(A) when A >= $0, A =< $9 -> 
    A - $0;
hex_to_int(A) when A >= $a, A =< $f -> 
    A - $a + 10;
hex_to_int(A) when A >= $A, A =< $F -> 
    A - $A + 10.

list_to_map([]) -> 
    [];
list_to_map([{K, V} | T]) ->
    list_to_map(T, #{K => list_to_map(V)});
list_to_map([H | T]) ->
    reverse(list_to_map(T, [list_to_map(H)]));
list_to_map(V) -> V.

list_to_map([{K, V} | T], Acc) ->
    list_to_map(T, maps:put(K, list_to_map(V), Acc));
list_to_map([H | T], Acc) ->
    list_to_map(T, [list_to_map(H) | Acc]);
list_to_map([], Acc) ->
    Acc.


reverse(L) ->
    reverse(L, []).

reverse([H | T], L) ->
    reverse(T, [H | L]);
reverse([], L) ->
    L.

test() ->
[] = bs04:decode(<<"[]">>, proplist),
[] = bs04:decode(<<"[ \n \t] \n\t ">>, proplist),
[] = bs04:decode(<<"{}">>, proplist),
[] = bs04:decode(<<"{ \n \t} \n\t ">>, proplist),
[[]] = bs04:decode(<<"[{}]">>, proplist),
[[]] = bs04:decode(<<"[[]]">>, proplist),
[[], 2, 3] = bs04:decode(<<"[{}, 2, 3]">>, proplist),
[1, [], 3] = bs04:decode(<<"[1, {}, 3]">>, proplist),
[1, 2, []] = bs04:decode(<<"[1, 2, {}]">>, proplist),
[1, 2, 3] = bs04:decode(<<"[1, 2, 3]">>, proplist),
[true, false, null] = bs04:decode(<<"[true, false, null]">>, proplist),
[<<"String value">>] = bs04:decode(<<"[\"String value\"]">>, proplist),
[<<"⃲"/utf16>>] = bs04:decode(<<"[\"\\u20f2\"]">>, proplist),
[<<"\n\r\t\b\f\\/\"">>] = bs04:decode(<<"[\"\\n\\r\\t\\b\\f\\\\\\/\\\"\"]">>, proplist),
[{<<"key">>, []}] = bs04:decode(<<"{\"key\": []}">>, proplist),
[{<<"key">>, []}] = bs04:decode(<<"{\"key\": []}">>, proplist),
[{<<"key">>, []}] = bs04:decode(<<"{\"key\": {}}">>, proplist),
[[{<<"key">>, []}]] = bs04:decode(<<"[{\"key\": {}}]">>, proplist),
[[{<<"key">>, []}]] = bs04:decode(<<"[{\"key\": []}]">>, proplist),
[{<<"key">>, <<"value">>}] = bs04:decode(<<"{\"key\": \"value\"}">>, proplist),
[{<<"key">>, [<<"value">>]}] = bs04:decode(<<"{\"key\": [\"value\"]}">>, proplist),
[{<<"key">>, [{<<"inner key">>, <<"value">>}]}] = bs04:decode(<<"{\"key\": {\"inner key\": \"value\"}}">>, proplist),
[[{<<"key">>, <<"value">>}]] = bs04:decode(<<"[{\"key\": \"value\"}]">>, proplist),
[1, false, <<"element">>, [<<"nested">>, <<"array">>], [{<<"key1">>, <<"value1">>}, {<<"key2">>, [<<"inside">>, <<"object">>]}]] =
bs04:decode(<<"[1, false, \"element\", [\"nested\", \"array\"], {\"key1\": \"value1\", \"key2\": [\"inside\", \"object\"]}]">>, proplist).

test(proplist) ->
    [test(proplist, Filename) || Filename <- ["test1.json", "test2.json"]];
test(map) ->
    [test(map, Filename) || Filename <- ["test1.json", "test2.json"]].

test(Type, Filename) ->
    {ok, Json} = file:read_file(Filename),
    decode(Json, Type).