-module(tnetstrings).
-author('mathieu@garambrogne.net').

-export([encode/1, decode/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

encode(B) when B == true -> "4:true!";
encode(false) -> "5:false!";
encode(null) -> "0:~";
encode(F) when is_float(F) ->
    [A] = io_lib:format("~w", [F]),
    with_size(A) ++ "^";
encode(N) when is_integer(N) ->
    with_size(integer_to_list(N)) ++ "#";
encode(S) when is_binary(S) ->
    with_size(binary_to_list(S)) ++ ",";
encode(A) when is_atom(A) ->
    with_size(atom_to_list(A)) ++ ",";
encode([{_K, _V}| _Remains] = Props) ->
    with_size(lists:foldl(
        fun({K, V}, Acc) ->
            %FIXME assert K is string
            Acc ++ encode(K) ++ encode(V)
    end, [], Props)) ++ "}";
encode(L) when is_list(L) ->
    with_size(lists:foldl(
        fun(I, Acc) ->
            Acc ++ encode(I)
        end, [], L)) ++ "]";
encode({struct, Props}) when is_list(Props) ->
    with_size(lists:foldl(
        fun({K, V}, Acc) ->
                %FIXME assert K is string
            Acc ++ encode(K) ++ encode(V)
    end, [], Props)) ++ "}".

decode(T) ->
    {Value, _} = parse(T),
    Value.

parse(T) ->
    {Payload, Type, Remain} = payload_parse(T),
    Value = case Type of
        $# ->
            {Int, _} = string:to_integer(Payload),
            Int;
        $^ ->
            {Float, _} = string:to_float(Payload),
            Float;
        $~ -> null;
        $, -> list_to_binary(Payload);
        $! ->
            case Payload of
                "true"  -> true;
                "false" -> false
                % FIXME  _ -> fail
            end;
        $] -> parse_list(Payload, []);
        $} -> parse_struct(Payload, [])

    end,
    {Value, Remain}.

% private
with_size(A) ->
    integer_to_list(iolist_size(A)) ++ ":" ++ A.

payload_parse(T) ->
    {Length, Extra} = payload_size(T, ""),
    Data = string:substr(Extra, 1, Length),
    Type = lists:nth(Length + 1, Extra),
    Remain = string:substr(Extra, Length + 2),
    {Data, Type, Remain}.

payload_size("", _) ->
    ko; %FIXME nice error message
payload_size([Head | Tail], Acc) ->
    case Head of
         $: ->
             {Int, _Rest} = string:to_integer(Acc),
             {Int, Tail};
          N -> payload_size(Tail, Acc ++ [N])
    end.

parse_list(L, Acc) ->
    {Value, Remain} = parse(L),
    List = Acc ++ [Value],
    case Remain of
        [] -> List;
        _  -> parse_list(Remain, List)
    end.

parse_struct(S, Acc) ->
    {K, R1} = parse(S),
    {V, R2} = parse(R1),
    Struct = Acc ++ [{binary_to_atom(K, utf8), V}],
    case R2 of
        [] -> {struct, Struct};
        _  -> parse_struct(R2, Struct)
    end.
