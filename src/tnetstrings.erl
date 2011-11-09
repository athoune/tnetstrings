-module(tnetstrings).

-export([encode/1, decode/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

my_func() ->
    ok.

encode(B) when B == true -> "4:true!";
encode(false) -> "5:false!";
encode(null) -> "0:~";
encode(F) when is_float(F) ->
    [A] = io_lib:format("~w", [F]),
    with_size(A) ++ "^";
encode(N) when is_number(N) ->
    [A] = io_lib:format("~w", [N]),
    with_size(A) ++ "#";
encode(S) when is_binary(S) ->
    with_size(binary_to_list(S)) ++ ",";
encode(A) when is_atom(A) ->
    with_size(atom_to_list(A)) ++ ",";
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
    {Payload, Type, _Remain} = payload_parse(T),
    case Type of
        $# ->
            {Int, _} = string:to_integer(Payload),
            Int;
        $^ ->
            {Float, _} = string:to_float(Payload),
            Float;
        $~ -> null;
        $, -> list_to_binary(Payload)
    end.

% private
with_size(A) ->
    [S] = io_lib:format("~w", [iolist_size(A)]),
    S ++ ":" ++ A.

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

