-module(tnetstrings_tests).
-include_lib("eunit/include/eunit.hrl").

int_test() ->
    ?assertEqual("2:42#", tnetstrings:encode(42)).
