-module(em_core_forms).

-compile([export_all]).

-include_lib("emlisp/include/emlisp.hrl").

%% AWKLisp is a Lisp interpreter written in awk, available by anonymous
%% ftp from ftp.cs.cmu.edu:/user/ai/lang/lisp/impl/awk/. It has thirteen
%% built-in functions: CAR, CDR, CONS, EQ, ATOM, SET, EVAL, ERROR, QUOTE,
%% COND, AND, OR, LIST.

error_(Tag, Data, _Env) -> throw({emlisp, make_symbol(Tag), Data}).

make_symbol({symbol, _}=S) -> S;
make_symbol(A) when is_binary(A) -> {symbol, A};
make_symbol(A) when is_list(A) -> {symbol, iolist_to_binary(A)};
make_symbol(A) when is_atom(A) -> {symbol, atom_to_binary(A, utf8)}.

is_symbol({symbol, _}) -> true;
is_symbol(_) -> false.

make_t() -> {symbol, <<"t">>}.
make_nil() -> [].

is_nil([]) -> true;
is_nil({symbol, <<"nil">>}) -> true;
is_nil(_) -> false.

is_true([]) -> false;
is_true(_) -> true.

%% Env & Eval
std_symbols() ->
  [{<<"eval">>, eval}, {<<"car">>, car}, {<<"cdr">>, cdr},
   {<<"cons">>, cons}, {<<"eq">>, eq}, {<<"atom">>, atom},
   {<<"defmacro">>, defmacro}, {<<"progn">>, progn}, {<<"set">>, set},
   {<<"error">>, error}, {<<"quote">>, quote}, {<<"list">>, list},
   {<<"exit">>, exit}, {<<"macroexpand-1">>, 'macroexpand-1'},
   {<<"nil">>, nil}, {<<"t">>, t},
   {<<"cond">>, cond_}, {<<"and">>, and_}, {<<"or">>, or_},
   {<<"let">>, let_}, {<<"let*">>, let_star}, {<<"funcall">>, funcall}].

fill_std_env() ->
  em_env:env_add_many(
    [{make_symbol(N), Op} || {N, Op} <- std_symbols()],
    em_env:global_env()).

is_std_symbol({symbol, S}) ->
  lists:keyfind(S, 1, std_symbols()) /= false.

%% EVAL

-spec eval(ast()) -> res().
eval(Ast) -> eval(Ast, em_env:global_env()).

-spec eval(ast(), em_env:env()) -> res().
eval([{erlmf, Mod, Fun} | Args], Env) ->
  M = erlang:binary_to_existing_atom(Mod, utf8),
  F = erlang:binary_to_existing_atom(Fun, utf8),
  EvaledArgs = eval_collect(Args, Env),
  erlang:apply(M, F, EvaledArgs);

eval({erlmfa, Mod, Fun, Arity}, _Env) ->
  M = erlang:binary_to_existing_atom(Mod, utf8),
  F = erlang:binary_to_existing_atom(Fun, utf8),
  fun M:F/Arity;

eval([{symbol, <<"lambda">>}, Args | Body], Env) ->
  make_lambda(Args, Body, Env);

eval([{symbol, _}=S | Args], Env) ->
  SymFunc = em_env:env_get(S, Env),
  case func_type(SymFunc) of
    lambda ->
      EvaledArgs = eval_collect(Args, Env),
      exec_lambda(SymFunc, EvaledArgs);
    function ->
      EvaledArgs = eval_collect(Args, Env),
      exec_func(SymFunc, EvaledArgs, Env);
    macro -> exec_macro(SymFunc, Args, Env);
    builtin -> exec_builtin(SymFunc, Args, Env)
  end;

eval([[{symbol, <<"lambda">>}, Names | Body] | Args], Env) ->
  {EvaledArgs, Env2} = eval_collect(Args, Env),
  exec_func(make_func(Names, Body), EvaledArgs, Env2);

eval([{lambda, Args, Body} | Args], Env) ->
  {EvaledArgs, Env2} = eval_collect(Args, Env),
  exec_func(make_func(Args, Body), EvaledArgs, Env2);

eval([], _Env) -> [];
eval(I, _Env) when is_integer(I) -> I;
eval({string, _}=X, _Env) -> X;
eval({boolean_, _}=X, _Env) -> X;
eval({symbol, _}=S, Env) -> em_env:env_get(S, Env);

eval({space}, _Env) -> make_nil();

%% eval(Exprs, Env) when is_list(Exprs) ->
%%   lists:foldl(fun(NextExpr, {_Res, AccEnv}) ->
%%                   eval(NextExpr, AccEnv)
%%               end, {{}, Env}, Exprs);

%% eval({quote, Exprs}, Env) ->
%%   {Exprs, Env};

eval(Data, Env) when is_list(Data) ->
  error_(illegal_function_call, Data, Env);
eval(Token, Env) ->
  error_(bad_token, Token, Env).

-spec eval_sequence([ast()], em_env:env()) -> res().
eval_sequence([Expr], Env) ->
  eval(Expr, Env);
eval_sequence([E|Rest], Env) ->
  eval(E, Env),
  eval_sequence(Rest, Env).

-spec eval_collect([ast()], em_env:env()) -> {[res()], em_env:env()}.
eval_collect(Exprs, Env) -> [eval(E, Env) || E <- Exprs].

exec_lambda(L, EvaledArgs) ->
  exec_func(make_func(lambda_args(L), lambda_body(L)),
            EvaledArgs, lambda_env(L)).

exec_func(Func, EvaledArgs, Env) ->
  ArgNames = func_args(Func),
  case length(EvaledArgs) == length(ArgNames) of
    true ->
      Env2 = em_env:env_add_frame(Env),
      em_env:env_add_many(lists:zip(ArgNames, EvaledArgs), Env2),
      Body = func_body(Func),
      Res = eval_sequence(Body, Env2),
      em_env:env_free_frame(Env2),
      Res;
    false ->
      error_(fun_arity_error, {Func, ArgNames, EvaledArgs}, Env)
  end.

expand_macro(Macro, Args) ->
  ArgNames = macro_args(Macro),
  Env = macro_env(Macro),
  case length(Args) == length(ArgNames) of
    true ->
      Env2 = em_env:env_add_frame(Env),
      em_env:env_add_many(lists:zip(ArgNames, Args), Env2),
      Body = macro_body(Macro),
      Res = eval_sequence(Body, Env2),
      em_env:env_free_frame(Env2),
      Res;
    false ->
      error_(macro_arity_error, {Macro, ArgNames, Args}, Env)
  end.

exec_macro(Macro, Args, Env) ->
  Expand = expand_macro(Macro, Args),
  eval(Expand, Env).

make_func(Args, Exprs) -> {func, Args, Exprs}.

func_type({func, _, _}) -> function;
func_type({macro, _, _, _}) -> macro;
func_type({lambda, _, _, _}) -> lambda;
func_type(Atom) when is_atom(Atom) -> builtin.

func_body({func, _, Exprs}) -> Exprs.
func_args({func, Args, _}) -> Args.

make_macro(Args, Exprs, Env) -> {macro, Args, Exprs, Env}.

macro_args({macro, Args, _, _}) -> Args.
macro_body({macro, _, Exprs, _}) -> Exprs.
macro_env({macro, _, _, Env}) -> Env.

make_lambda(Args, Body, Env) ->
  em_env:env_keep(Env),
  {lambda, Args, Body, Env}.

lambda_args({lambda, Args, _, _}) -> Args.
lambda_body({lambda, _, Body, _}) -> Body.
lambda_env({lambda, _, _, Env}) -> Env.

exec_builtin(eval, [Arg], Env) ->
  eval(Arg, Env);
exec_builtin(car, [Arg], Env) ->
  case eval(Arg, Env) of
    [] -> make_nil();
    {symbol, <<"nil">>} -> make_nil();
    [H|_] -> H;
    Other -> error_(cant_car, Other, Env)
  end;
exec_builtin(cdr, [Arg], Env) ->
  case eval(Arg, Env) of
    [] -> make_nil();
    {symbol, <<"nil">>} -> make_nil();
    [_|T] -> T;
    Other -> error_(cant_cdr, Other, Env)
  end;
exec_builtin(cons, [Arg1, Arg2], Env) ->
  Evaled1 = eval(Arg1, Env),
  Evaled2 = eval(Arg2, Env),
  case is_nil(Evaled2) of
    true -> [Evaled1];
    false -> [Evaled1 | Evaled2]
  end;
exec_builtin(eq, [Arg1, Arg2], Env) ->
  Evaled1 = eval(Arg1, Env),
  Evaled2 = eval(Arg2, Env),
  case {Evaled1, Evaled2} of
    {A, A} -> make_t();
    {[], {symbol, <<"nil">>}} -> make_t();
    {{symbol, <<"nil">>}, []} -> make_t();
    _ -> make_nil()
  end;
exec_builtin(atom, [Arg], Env) ->
  case eval(Arg, Env) of
    [] -> make_t();
    L when is_list(L) -> make_nil();
    _ -> make_t()
  end;
exec_builtin(set, Args, Env) ->
  exec_set(Args, Env);
exec_builtin(error, Args, Env) ->
  case eval_collect(Args, Env) of
    [H|T] -> error_(H, T, Env);
    Evaled -> error_(Evaled, [], Env)
  end;
exec_builtin(quote, [Arg], _Env) -> Arg;
exec_builtin(cond_, Args, Env) -> exec_cond(Args, Env);
exec_builtin(and_, Args, Env) -> exec_and(Args, Env);
exec_builtin(or_, Args, Env) -> exec_or(Args, Env);
exec_builtin(list, Args, Env) -> eval_collect(Args, Env);
exec_builtin(let_, [Binds|Forms], Env) -> exec_let(Binds, Forms, Env);
exec_builtin(let_star, [Binds|Forms], Env) ->
  exec_let_star(Binds, Forms, Env);
exec_builtin(exit, _, _Env) -> throw(emlisp_exit);
exec_builtin(progn, Forms, Env) -> eval_sequence(Forms, Env);
exec_builtin(funcall, [F|Forms], Env) ->
  Fun = eval(F, Env),
  Args = eval_collect(Forms, Env),
  exec_lambda(Fun, Args);
exec_builtin(defmacro, [Name, Args, Body], Env) ->
  case is_symbol(Name) of
    true ->
      L = make_macro(Args, [Body], Env),
      em_env:env_set(em_env:global_env(), Name, L),
      Name;
    false ->
      error_(cant_defmacro, Name, Env)
  end;
exec_builtin('macroexpand-1', [[_Quote, [MacroName | Args]]], Env) ->
  Macro = em_env:env_get(MacroName, Env),
  Evaled = expand_macro(Macro, Args),
  Evaled;
exec_builtin(B, Args, Env) ->
  error_(undefined_builtin, {B, Args}, Env).

exec_and(Args, Env) -> exec_and(Args, Env, true).
exec_and([], _Env, Last) -> Last;
exec_and([H|T], Env, _Last) ->
  {Evaled, Env2} = eval(H, Env),
  case is_true(Evaled) of
    false -> make_nil();
    true -> exec_and(T, Env2, Evaled)
  end.

exec_or([], _Env) -> make_nil();
exec_or([H|T], Env) ->
  Evaled = eval(H, Env),
  case is_true(Evaled) of
    true -> Evaled;
    false -> exec_or(T, Env)
  end.

exec_cond([], _Env) -> make_nil();
exec_cond([[C, B]|T], Env) ->
  Evaled = eval(C, Env),
  case is_true(Evaled) of
    true -> eval(B, Env);
    false -> exec_cond(T, Env)
  end.

exec_set(Args, Env) -> exec_set(Args, Env, make_nil()).
exec_set([], _Env, Last) -> Last;
exec_set([K, V|T], Env, _Last) ->
  EvaledK = eval(K, Env),
  case is_symbol(EvaledK) of
    true ->
      case is_std_symbol(EvaledK) of
        true -> error_(cant_redefine, EvaledK, Env);
        false ->
          EvaledV = eval(V, Env),
          em_env:env_set(EvaledK, EvaledV, Env),
          exec_set(T, Env, EvaledV)
        end;
    false -> error_(cant_set, EvaledK, Env)
  end.

exec_let(Binds, Forms, Env) ->
  Env2 = em_env:env_add_frame(Env),
  [case Form of
     [Var] -> em_env:env_add(Var, make_nil(), Env2);
     [Var, Expr] -> em_env:env_add(Var, eval(Expr, Env), Env2)
   end || Form <- Binds],
  Res = eval_sequence(Forms, Env2),
  em_env:env_free_frame(Env2),
  Res.

exec_let_star(Binds, Forms, Env) ->
  Env2 = em_env:env_add_frame(Env),
  [case Form of
     [Var] -> em_env:env_add(Var, make_nil(), Env2);
     [Var, Expr] -> em_env:env_add(Var, eval(Expr, Env2), Env2)
   end || Form <- Binds],
  Res = eval_sequence(Forms, Env2),
  em_env:env_free_frame(Env2),
  Res.

exec_store_set([Space, Key, Val], _Env) ->
  em_store:set_value(Space, Key, Val).

exec_store_get([Space, Key], _Env) ->
  case em_store:get_value(Space, Key) of
    {ok, S} -> S;
    {error, not_found} -> error_(not_found, Space, Key)
  end.
