-module(em_print).

-export([print/2, format_object/2]).

print(Res, Env) ->
  Fm = format_object(Res, Env),
  io:format("~ts~n", [Fm]).

format_object(O, Env) ->
  F = format_object1(O, Env),
  iolist_to_binary(F).

format_object1([H|T], Env) when is_list(T) ->
  Fh = format_object1(H, Env),
  ["(", Fh, [[" ", format_object1(O, Env)] || O <- T], ")"];
format_object1([H|T], Env) ->
  Fh = format_object(H, Env),
  Ft = format_object(T, Env),
  ["(", Fh, " . ", Ft, ")"];
format_object1([], _Env) -> "nil";
format_object1({symbol, S}, _Env) -> S;
format_object1({string, S}, _Env) -> S;
format_object1(I, _Env) when is_integer(I) -> integer_to_list(I);
format_object1(F, _Env) when is_float(F) -> float_to_list(F);
format_object1(O, _Env) -> io_lib:format("#<~p>", [O]).
