erlang-simplicitydb
===================

SimplicityDB, simple file based key-value storage in pure Erlang

```
$ ./rebar compile
$ vim test.config
$ erl -pa ./apps/simplicitydb/ebin -config test.config
1> application:ensure_all_started(simplicitydb). % will take a few seconds
{ok,[simplicitydb]}
2> simplicitydb:write([some,key], [ {k1, v1}, {k2, v2} ]).
ok
3> simplicitydb:read([ some, key ]).
{ok,[{k1,v1},{k2,v2}]}
4> simplicitydb:delete([ some, key ]).
ok
5>
```

For more details see http://eax.me/erlang-simplicitydb/
