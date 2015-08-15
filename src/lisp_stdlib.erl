-module(lisp_stdlib).

-compile([export_all]).

-define(T, lisp_package:get_symbol("lisp:t")).
-define(NIL, []).

init() ->
  [lisp_package:symbol_value_internal(
     lisp_package:get_symbol(Name), Val) || {Name, Val} <- std_vals()],
  [lisp_package:symbol_function_internal(
     lisp_package:get_symbol(Name), Val) || {Name, Val} <- std_funs()].

id(X) -> X.
car([[X|_]]) -> X.
cdr([[_|X]]) -> X.
quote([X]) -> X.
list(X) -> X.
cons([X, Y]) -> [X|Y].

atom([[]]) -> ?T;
atom([X]) when is_list(X) -> ?NIL;
atom([_]) -> ?T.

consp([X]) when is_list(X) -> ?T;
consp([_]) -> ?NIL.

null([[]]) -> ?T;
null([_]) -> ?NIL.

eq([A, A]) -> ?T;
eq([_, _]) -> ?NIL.

error_(Args) -> throw({lisp, Args}).

exit([]) -> throw(lisp_exit).

std_funs() ->
  [{"lisp:car", fun car/1},
   {"lisp:cdr", fun cdr/1},
   {"lisp:quote", fun quote/1},
   {"lisp:list", fun list/1},
   {"lisp:cons", fun cons/1},
   {"lisp:atom", fun atom/1},
   {"lisp:consp", fun consp/1},
   {"lisp:not", fun null/1},
   {"lisp:null", fun null/1},
   {"lisp:eq", fun eq/1},
   {"lisp:error", fun error_/1},
   {"lisp:exit", fun exit/1}].

std_vals() ->
  [{"lisp:nil", ?NIL}].
