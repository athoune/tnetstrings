Tnetstrings for Erlang
======================

[Tnetstrings](http://tnetstrings.org/) is a JSON like serialisation.

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

BSD
