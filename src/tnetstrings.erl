-module(tnetstrings).

-export([encode/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

my_func() ->
    ok.

encode(N) when is_number(N) ->
    [A] = io_lib:format("~w", [N]),
    with_size(A) ++ "#".

with_size(A) ->
    [S] = io_lib:format("~w", [iolist_size(A)]),
    S ++ ":" ++ A.

