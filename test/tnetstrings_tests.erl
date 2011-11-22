-module(tnetstrings_tests).
-author('mathieu@garambrogne.net').

-include_lib("eunit/include/eunit.hrl").

number_test() ->
    ?assertEqual(<<"2:42#">>, tnetstrings:encode(42)),
    ?assertEqual(42, tnetstrings:decode(<<"2:42#">>)),
    ?assertEqual(<<"8:3.141509^">>, tnetstrings:encode(3.141509)),
    ?assertEqual(3.141509, tnetstrings:decode(<<"8:3.141509^">>)),
    ?assertEqual(42.0, tnetstrings:decode(<<"2:42#">>, [float])).

string_test() ->
    ?assertEqual(<<"5:Beuha,">>, tnetstrings:encode('Beuha')),
    ?assertEqual(<<"5:Beuha,">>, tnetstrings:encode(<<"Beuha">>)),
    ?assertEqual(<<"Beuha">>, tnetstrings:decode(<<"5:Beuha,">>)).

boolean_test() ->
    ?assertEqual(<<"4:true!">>, tnetstrings:encode(true)),
    ?assertEqual(true, tnetstrings:decode(<<"4:true!">>)),
    ?assertEqual(<<"5:false!">>, tnetstrings:encode(false)),
    ?assertEqual(false, tnetstrings:decode(<<"5:false!">>)).

null_test() ->
    ?assertEqual(<<"0:~">>, tnetstrings:encode(null)),
    ?assertEqual(null, tnetstrings:decode(<<"0:~">>)).

list_test() ->
    ?assertEqual(<<"15:1:1#1:a,4:true!]">>, tnetstrings:encode([1, <<"a">>, true])),
    ?assertEqual([1, <<"a">>, true], tnetstrings:decode(<<"15:1:1#1:a,4:true!]">>)).

struct_test() ->
    ?assertEqual(<<"27:3:age,2:42#4:name,6:Robert,}">>, tnetstrings:encode([
                    {age, 42},
                    {name, <<"Robert">>}
                ])),
    ?assertEqual(<<"27:3:age,2:42#4:name,6:Robert,}">>, tnetstrings:encode({struct, [
                    {age, 42},
                    {name, <<"Robert">>}
                ]})),
    ?assertEqual({struct, [
                    {age, 42},
                    {name, <<"Robert">>}
                ]}, tnetstrings:decode(<<"27:3:age,2:42#4:name,6:Robert,}">>, [{label, atom}])).

payload_test() ->
    T = <<"5:false!">>,
    ?assertEqual({<<"false">>, <<"!">>, <<>>}, tnetstrings:payload_parse(T)).

binary_test() ->
    T = <<"27:3:age,2:42#4:name,6:Robert,}">>,
    {Data, Type, Remain} = tnetstrings:payload_parse(T),
    ?assertEqual(<<"}">>, Type),
    ?assertEqual(<<>>, Remain),
    ?assertEqual(<<"3:age,2:42#4:name,6:Robert,">>, Data).
