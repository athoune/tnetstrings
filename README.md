Tnetstrings for Erlang
======================

[Tnetstrings](http://tnetstrings.org/) is a JSON like serialisation.

Install
-------

You can put *tnetstrings.erl* in your project or add the project as a dependency if you are using *rebar*.

Usage
-----

```erlang
Example = {struct, [
    {age, 42},
    {name, <<"Robert">>}
]},
T = tnetstrings:encode(Example),
Example = tnetstrings:decode(T).

```

State
-----

Early release, but each types are handled.

Todo
----

 * Following Erlang recomandation for a JSON API
 * Explain errors

Licence
-------

MIT
