-module(tnetstrings_tests).
-include_lib("eunit/include/eunit.hrl").

number_test() ->
    ?assertEqual("2:42#", tnetstrings:encode(42)),
    ?assertEqual("8:3.141509^", tnetstrings:encode(3.141509)).

string_test() ->
    ?assertEqual("5:Beuha,", tnetstrings:encode('Beuha')),
    ?assertEqual("5:Beuha,", tnetstrings:encode(<<"Beuha">>)).

boolean_test() ->
    ?assertEqual("4:true!", tnetstrings:encode(true)),
    ?assertEqual("5:false!", tnetstrings:encode(false)).

null_test() ->
    ?assertEqual("0:~", tnetstrings:encode(null)).

list_test() ->
    ?assertEqual("15:1:1#1:a,4:true!]", tnetstrings:encode([1, <<"a">>, true])).

struct_test() ->
    ?assertEqual("27:3:age,2:42#4:name,6:Robert,}", tnetstrings:encode({struct, [
                    {age, 42},
                    {name, <<"Robert">>}
                ]})).
