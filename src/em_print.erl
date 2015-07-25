-module(em_print).

-export([print/1, print/2]).

print(Res) -> io:format("~p~n", [Res]).
print(Fmt, Res) -> io:format(Fmt, Res).
