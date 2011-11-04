-module(tnetstrings).

-export([encode/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

my_func() ->
    ok.

encode(B) when B == true -> "4:true!";
encode(false) -> "5:false!";
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
        end, [], L)) ++ "]".

with_size(A) ->
    [S] = io_lib:format("~w", [iolist_size(A)]),
    S ++ ":" ++ A.

