-module(tnetstrings).
-author('mathieu@garambrogne.net').

-export([
    encode/1,
    encode/2,
    decode/1,
    decode/2
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(decoder, {label=binary, float=false}).

encode(Z)->
    encode(Z, []).

encode(Z, _Option) ->
    reverse(encodel(Z)).

reverse(L) when is_list(L) -> list_to_binary(lists:reverse(L));
reverse(L) -> L.

encodel(B) when B == true -> <<"4:true!">>;
encodel(false) -> <<"5:false!">>;
encodel(null) -> <<"0:~">>;
encodel(F) when is_float(F) ->
    [A] = io_lib:format("~w", [F]),
    [$^ | with_size(A)];
encodel(N) when is_integer(N) ->
    [$# | with_size(integer_to_list(N))];
encodel(S) when is_binary(S) ->
    [$, | with_size(binary_to_list(S))];
encodel(A) when is_atom(A) ->
    [$, | with_size(atom_to_list(A))];
encodel([{_K, _V}| _Remains] = Props) ->
    [$} | with_size(lists:reverse(lists:foldl(
        fun({K, V}, Acc) ->
            %FIXME assert K is string
            [reverse(encodel(V)), reverse(encodel(K)) | Acc]
    end, [], Props)))];
encodel(L) when is_list(L) ->
    LL = lists:foldl(
        fun(I, Acc) ->
                [reverse(encodel(I)) | Acc]
    end, [], L),
    [$\] | with_size(lists:reverse(LL))];
encodel({struct, Props}) when is_list(Props) ->
    encodel(Props).

decode(T) ->
    decode(T, []).

decode(T, Option) ->
    {Value, _} = parse(T, parse_decoder(Option)),
    Value.

parse_decoder(Option) ->
    #decoder{
        label=proplists:get_value(label, Option, binary),
        float=proplists:get_bool(float, Option)}.

to_float(F) ->
    {Float, _} = string:to_float(binary_to_list(F)),
    Float.
to_int(I) ->
    {Int, _} = string:to_integer(binary_to_list(I)),
    Int.

parse_int(I, Float) when Float -> to_int(I) * 1.0;
parse_int(I, _) -> to_int(I).

parse(T, Option) ->
    {Payload, Type, Remain} = payload_parse(T),
    Value = case Type of
        <<"#">> -> parse_int(Payload, Option#decoder.float);
        <<"^">> -> to_float(Payload);
        <<"~">> -> null;
        <<",">> -> Payload;
        <<"!">> ->
            case Payload of
                <<"true">>  -> true;
                <<"false">> -> false;
                _Bad -> exit("Bad binary")
            end;
        <<"]">> -> parse_list(Payload, [], Option);
        <<"}">> -> parse_struct(Payload, [], Option);
        _Bad -> exit("Bad type")
    end,
    {Value, Remain}.

% private
with_size(A) ->
    lists:reverse([integer_to_list(iolist_size(A)), $: | A]).

payload_parse(T) ->
    [L, E] = binary:split(T, <<$:>>),
    Length = list_to_integer(binary_to_list(L)),
    Data   = binary:part(E, 0, Length),
    Type   = binary:part(E, Length, 1),
    Remain = case iolist_size(E) -1 of
         Length -> <<>>;
         _      -> binary:part(E, Length+1, iolist_size(E)-Length-1)
    end,
    {Data, Type, Remain}.

parse_list(L, Acc, Option) ->
    {Value, Remain} = parse(L, Option),
    List = [Value | Acc],
    case Remain of
        <<>> -> lists:reverse(List);
        _  -> parse_list(Remain, List, Option)
    end.

parse_key(K, Option) when Option == existing_atom ->
    binary_to_existing_atom(K, utf8);
parse_key(K, Option) when Option == atom ->
    binary_to_atom(K, utf8);
parse_key(K, _) ->
    K.

parse_struct(S, Acc, Option) ->
    {K, R1} = parse(S, Option),
    {V, R2} = parse(R1, Option),
    io:format("~p~n", [Option]),
    Struct = [{parse_key(K, Option#decoder.label), V} | Acc ],
    case R2 of
        <<>> -> {struct, lists:reverse(Struct)};
        _  -> parse_struct(R2, Struct, Option)
    end.
