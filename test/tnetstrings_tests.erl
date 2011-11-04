-module(tnetstrings_tests).
-include_lib("eunit/include/eunit.hrl").

number_test() ->
    ?assertEqual("2:42#", tnetstrings:encode(42)),
    ?assertEqual("8:3.141509^", tnetstrings:encode(3.141509)).

string_test() ->
    ?assertEqual("5:Beuha,", tnetstrings:encode('Beuha')),
    ?assertEqual("5:Beuha,", tnetstrings:encode(<<"Beuha">>)).

boolean_test() ->
    ?assertEqual("4:true!", tnetstrings:encode(true)).

list_test() ->
    ?assertEqual("15:1:1#1:a,4:true!]", tnetstrings:encode([1, <<"a">>, true])).
