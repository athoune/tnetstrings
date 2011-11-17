-module(tnetstrings).
-author('mathieu@garambrogne.net').

-export([encode/1, decode/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

encode(Z)->
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
    {Value, _} = parse(T),
    Value.

parse(T) ->
    {Payload, Type, Remain} = payload_parse(T),
    Value = case Type of
        <<"#">> ->
            {Int, _} = string:to_integer(binary_to_list(Payload)),
            Int;
        <<"^">> ->
            {Float, _} = string:to_float(binary_to_list(Payload)),
            Float;
        <<"~">> -> null;
        <<",">> -> Payload;
        <<"!">> ->
            case Payload of
                <<"true">>  -> true;
                <<"false">> -> false;
                _Bad -> exit("Bad binary")
            end;
        <<"]">> -> parse_list(Payload, []);
        <<"}">> -> parse_struct(Payload, []);
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

parse_list(L, Acc) ->
    {Value, Remain} = parse(L),
    List = [Value | Acc],
    case Remain of
        <<>> -> lists:reverse(List);
        _  -> parse_list(Remain, List)
    end.

parse_struct(S, Acc) ->
    {K, R1} = parse(S),
    {V, R2} = parse(R1),
    Struct = [{binary_to_atom(K, utf8), V} | Acc ],
    case R2 of
        <<>> -> {struct, lists:reverse(Struct)};
        _  -> parse_struct(R2, Struct)
    end.
