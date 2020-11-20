-module(bs04).
-export([decode/2]).

-export([test/0, test/1]).

-define(Is_number_start(C), (C == $-) or ((C >= $0) and (C =< $9))).

decode(Json, Type) ->
    {Acc, <<>>} = do_decode(string:trim(Json), Type),
    Acc.

do_decode(<<"[", _Rest/binary>> = Bin, Type) ->
    decode_val(Bin, Type);
do_decode(<<"{", _Rest/binary>> = Bin, Type) ->
    decode_val(Bin, Type).

decode_val(<<"[", Rest/binary>>, Type) ->
    decode_arr(string:trim(Rest, leading), Type);
decode_val(<<"{", Rest/binary>>, Type) ->
    decode_obj(string:trim(Rest, leading), Type);
decode_val(<<"\"", Rest/binary>>, _Type) ->
    decode_str(Rest, <<>>);
decode_val(<<C, Rest/binary>>, _Type) when ?Is_number_start(C) ->
    decode_num(Rest, {<<C>>, int});
decode_val(<<"true", Rest/binary>>, _Type) ->
    {true, Rest};
decode_val(<<"false", Rest/binary>>, _Type) ->
    {false, Rest};
decode_val(<<"null", Rest/binary>>, _Type) ->
    {null, Rest}.

decode_arr(Bin, Type) ->
    decode_arr(Bin, [], Type).

decode_arr(<<"]", Rest/binary>>, Acc, _Type) ->
    {lists:reverse(Acc), Rest};
decode_arr(Bin, Acc, Type) ->
    {Val, Rest} = decode_val(string:trim(Bin, leading), Type),
    Rest1 = string:trim(Rest, leading),
    case Rest1 of
        <<",", Rest2/binary>> -> decode_arr(string:trim(Rest2), [Val | Acc], Type);
        <<"]", Rest2/binary>> -> {lists:reverse([Val |Acc]), Rest2}
    end.

decode_obj(Bin, map) ->
    decode_obj(Bin, #{}, map);
decode_obj(Bin, proplist) ->
    decode_obj(Bin, [], proplist).

decode_obj(<<"}", Rest/binary>>, Acc, map) ->
    {Acc, Rest};
decode_obj(<<"}", Rest/binary>>, Acc, proplist) ->
    {lists:reverse(Acc), Rest};
decode_obj(Bin, Acc, Type) ->
    {Key, Rest} = decode_key(string:trim(Bin, leading)),
    Rest1 = string:trim(Rest, leading),
    <<":", Rest2/binary>> = Rest1,
    {Val, Rest3} = decode_val(string:trim(Rest2, leading), Type),
    Acc1 = put_kv(Key, Val, Acc, Type),
    Rest4 = string:trim(Rest3, leading),
    case Rest4 of
        <<",", Rest5/binary>> -> decode_obj(string:trim(Rest5, leading), Acc1, Type);
        <<"}", _Rest5/binary>> -> decode_obj(Rest4, Acc1, Type)
    end.

decode_key(<<"\"", Rest/binary>>) ->
    decode_str(Rest, <<>>).

decode_str(<<"\\\\", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\\">>);
decode_str(<<"\\\"", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\"">>);
decode_str(<<"\\/", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "/">>);
decode_str(<<"\\n", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\n">>);
decode_str(<<"\\r", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\r">>);
decode_str(<<"\\t", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\t">>);
decode_str(<<"\\b", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\b">>);
decode_str(<<"\\f", Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, "\f">>);
decode_str(<<"\\u", A0, A1, A2, A3, Rest/binary>>, Acc) ->
    U = unicode_seq(A0, A1, A2, A3),
    decode_str(Rest, <<Acc/binary, U/utf16>>);
decode_str(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
decode_str(<<C/utf8, Rest/binary>>, Acc) ->
    decode_str(Rest, <<Acc/binary, C/utf8>>).

decode_num(<<C, Rest/binary>>, {Num, T}) when C >= $0, C=< $9 ->
    decode_num(Rest, {<<Num/binary, C>>, T});
decode_num(<<".", Rest/binary>>, {Num, int}) ->
    decode_num(Rest, {<<Num/binary, ".">>, int});
decode_num(<<"-", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "-">>, float});
decode_num(<<"e", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "e">>, float});
decode_num(<<"E", Rest/binary>>, {Num, float}) ->
    decode_num(Rest, {<<Num/binary, "e">>, float});
decode_num(Bin, {Num, int}) ->
    {binary_to_integer(Num), Bin};
decode_num(Bin, {Num, float}) ->
    {binary_to_float(Num), Bin}.

put_kv(K, V, Obj, map) ->
    maps:put(K, V, Obj);
put_kv(K, V, Obj, proplist) ->
    [{K, V} | Obj].

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

test() ->
[] = bs04:decode(<<"[]">>, proplist),
[] = bs04:decode(<<"[]">>, map),
[] = bs04:decode(<<"\r\n \t[ \n\r \t] \n\t\r ">>, proplist),
[] = bs04:decode(<<"\r\n \t[ \n\r \t] \n\t\r ">>, map),
[] = bs04:decode(<<"{}">>, proplist),
#{} = bs04:decode(<<"{}">>, map),
[] = bs04:decode(<<"\r\n \t{ \n\r \t} \n\t\r ">>, proplist),
#{} = bs04:decode(<<"\r\n \t{ \n\r \t} \n\t\r ">>, map),
[[]] = bs04:decode(<<"[{}]">>, proplist),
[#{}] = bs04:decode(<<"[{}]">>, map),
[[]] = bs04:decode(<<"[[]]">>, proplist),
[[]] = bs04:decode(<<"[[]]">>, map),
[[], 2, 3] = bs04:decode(<<"[{}, 2, 3]">>, proplist),
[#{}, 2, 3] = bs04:decode(<<"[{}, 2, 3]">>, map),
[1, [], 3] = bs04:decode(<<"[1, {}, 3]">>, proplist),
[1, #{}, 3] = bs04:decode(<<"[1, {}, 3]">>, map),
[1, 2, []] = bs04:decode(<<"[1, 2, {}]">>, proplist),
[1, 2, #{}] = bs04:decode(<<"[1, 2, {}]">>, map),
[1, 2, 3] = bs04:decode(<<"[1, 2, 3]">>, proplist),
[1, 2, 3] = bs04:decode(<<"[1, 2, 3]">>, map),
[true, false, null] = bs04:decode(<<"[true, false, null]">>, proplist),
[true, false, null] = bs04:decode(<<"[true, false, null]">>, map),
[<<"String value">>] = bs04:decode(<<"[\"String value\"]">>, proplist),
[<<"String value">>] = bs04:decode(<<"[\"String value\"]">>, map),
[<<"⃲"/utf16>>] = bs04:decode(<<"[\"\\u20f2\"]">>, proplist),
[<<"⃲"/utf16>>] = bs04:decode(<<"[\"\\u20f2\"]">>, map),
[<<"\n\r\t\b\f\\/\"">>] = bs04:decode(<<"[\"\\n\\r\\t\\b\\f\\\\\\/\\\"\"]">>, proplist),
[<<"\n\r\t\b\f\\/\"">>] = bs04:decode(<<"[\"\\n\\r\\t\\b\\f\\\\\\/\\\"\"]">>, map),
[{<<"key">>, []}] = bs04:decode(<<"{\"key\": []}">>, proplist),
% #{<<"key">> => []} = bs04:decode(<<"{\"key\": []}">>, map),
[{<<"key">>, []}] = bs04:decode(<<"{\"key\": {}}">>, proplist),
% bs04:decode(<<"{\"key\": {}}">>, map) = #{<<"key">> => #{}},
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