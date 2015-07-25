-module(em_core_forms).

-compile([export_all]).

%% AWKLisp is a Lisp interpreter written in awk, available by anonymous
%% ftp from ftp.cs.cmu.edu:/user/ai/lang/lisp/impl/awk/. It has thirteen
%% built-in functions: CAR, CDR, CONS, EQ, ATOM, SET, EVAL, ERROR, QUOTE,
%% COND, AND, OR, LIST.

-type ast() :: any().
-type env() :: any().
-type res() :: any().

error_(Tag, Data, _Env) -> throw({emlisp, make_symbol(Tag), Data}).

make_symbol({symbol, _}=S) -> S;
make_symbol(A) when is_binary(A) -> {symbol, A};
make_symbol(A) when is_list(A) -> {symbol, iolist_to_binary(A)};
make_symbol(A) when is_atom(A) -> {symbol, atom_to_binary(A, utf8)}.

is_symbol({symbol, _}) -> true;
is_symbol(_) -> false.

make_t() -> {symbol, <<"t">>}.
make_nil() -> [].

is_true([]) -> false;
is_true(_) -> true.

%% Env & Eval

-spec null_env() -> env().
null_env() -> env_add_frame([]).

-spec env_add_frame(env()) -> env().
env_add_frame(Env) -> [dict:new()|Env].

-spec env_set(any(), any(), env()) -> env().
env_set(K, V, [Env|Rest]) -> [dict:store(K, V, Env)|Rest].

env_set_many(KVs, [Env|Rest]) ->
  Frame2 = lists:foldl(fun({K, V}, Frame) ->
                           dict:store(K, V, Frame)
                       end, Env, KVs),
  [Frame2|Rest].

-spec env_get(any(), env()) -> any().
env_get(K, Envs) -> env_get1(K, Envs, Envs).

env_get1(K, [], Envs) -> error_(env_not_found, K, Envs);
env_get1(K, [Env|Rest], Envs) ->
  case dict:find(K, Env) of
    {ok, Val} -> Val;
    error -> env_get1(K, Rest, Envs)
  end.

std_env() ->
  env_set_many(
    [{make_symbol(<<"nil">>), make_nil()},
     {make_symbol(<<"t">>), make_t()},
     {make_symbol(<<"cond">>), cond_},
     {make_symbol(<<"and">>), and_},
     {make_symbol(<<"or">>), or_}
     | [{make_symbol(N), N} || N <- [car, cdr, cons, eq,
                                     atom, set, error, quote, list, exit]]],
    null_env()).

%% EVAL

-spec eval(ast()) -> {res(), env()}.
eval(Ast) -> eval(Ast, std_env()).

-spec eval(ast(), env()) -> {res(), env()}.
eval([{erlsym, Mod, Fun} | Args], Env) ->
  M = erlang:binary_to_existing_atom(Mod, utf8),
  F = erlang:binary_to_existing_atom(Fun, utf8),
  {EvaledArgs, Env2} = eval_collect(Args, Env),
  {erlang:apply(M, F, EvaledArgs), Env2};

eval([{symbol, _}=S | Args], Env) ->
  SymFunc = env_get(S, Env),
  case func_type(SymFunc) of
    function -> exec_func(SymFunc, Args, Env);
    macro -> exec_macro(SymFunc, Args, Env);
    builtin -> exec_builtin(SymFunc, Args, Env)
  end;

eval([], Env) -> {[], Env};
eval(I, Env) when is_integer(I) -> {I, Env};
eval({string, S}, Env) -> {{string, S}, Env};
eval({boolean_, B}, Env) -> {{boolean_, B}, Env};
eval({symbol, _}=S, Env) ->
  V = env_get(S, Env),
  {V, Env};

%% eval(Exprs, Env) when is_list(Exprs) ->
%%   lists:foldl(fun(NextExpr, {_Res, AccEnv}) ->
%%                   eval(NextExpr, AccEnv)
%%               end, {{}, Env}, Exprs);

%% eval({quote, Exprs}, Env) ->
%%   {Exprs, Env};

eval(Data, Env) ->
  error_(illegal_function_call, Data, Env).

-spec eval_collect([ast()], env()) -> {[res()], env()}.
eval_collect(Exprs, Env) ->
  {NewEnv, Res} = lists:foldl(fun(NextExpr, {NextEnv, Acc}) ->
                                  {Res, Env2} = eval(NextExpr, NextEnv),
                                  {Env2, [Res | Acc]}
                              end, {Env, []}, Exprs),
  {lists:reverse(Res), NewEnv}.

exec_func(Func, Args, Env) ->
  {EvaledArgs, Env2} = eval_collect(Args, Env),
  ArgNames = func_args(Func),
  case length(EvaledArgs) == length(ArgNames) of
    true ->
      Env2 = env_add_frame(Env),
      Env3 = env_set_many(lists:zip(ArgNames, EvaledArgs), Env2),
      Body = func_body(Func),
      lists:foldl(fun(NextExpr, {_Res, AccEnv}) ->
                      eval(NextExpr, AccEnv)
                  end, {{}, Env3}, Body);
    false ->
      error_(fun_arity_error, {Func, ArgNames, EvaledArgs}, Env)
  end.

exec_macro(Macro, Args, Env) ->
  ArgNames = macro_args(Macro),
  case length(Args) == length(ArgNames) of
    true ->
      Env2 = env_add_frame(Env),
      Env3 = env_set_many(lists:zip(ArgNames, Args), Env2),
      Body = macro_body(Macro),
      lists:foldl(fun(NextExpr, {_Res, AccEnv}) ->
                      eval(NextExpr, AccEnv)
                  end, {{}, Env3}, Body);
    false ->
      error_(macro_arity_error, {Macro, ArgNames, Args}, Env)
  end.

make_func(Args, Exprs) -> {func, Args, Exprs}.

func_type({func, _, _}) -> function;
func_type({macro, _, _}) -> macro;
func_type(Atom) when is_atom(Atom) -> builtin.

func_body({func, _, Exprs}) -> Exprs.
func_args({func, Args, _}) -> Args.

make_macro(Args, Exprs) -> {macro, Args, Exprs}.

macro_args({macro, Args, _}) -> Args.
macro_body({macro, _, Exprs}) -> Exprs.

exec_builtin(car, [Arg], Env) ->
  {Evaled, Env2} = eval(Arg, Env),
  case Evaled of
    [] -> {make_nil(), Env2};
    {symbol, <<"nil">>} -> {make_nil(), Env2};
    [H|_] -> {H, Env2};
    Other -> error_(cant_car, Other, Env2)
  end;
exec_builtin(cdr, [Arg], Env) ->
  {Evaled, Env2} = eval(Arg, Env),
  case Evaled of
    [] -> {make_nil(), Env2};
    {symbol, <<"nil">>} -> {make_nil(), Env2};
    [_|T] -> {T, Env2};
    Other -> error_(cant_cdr, Other, Env2)
  end;
exec_builtin(cons, [Arg1, Arg2], Env) ->
  {Evaled1, Env2} = eval(Arg1, Env),
  {Evaled2, Env3} = eval(Arg2, Env2),
  case Evaled2 of
    {symbol, <<"nil">>} -> {[Evaled1], Env};
    _ -> {[Evaled1 | Evaled2], Env3}
  end;
exec_builtin(eq, [Arg1, Arg2], Env) ->
  {Evaled1, Env2} = eval(Arg1, Env),
  {Evaled2, Env3} = eval(Arg2, Env2),
  case {Evaled1, Evaled2} of
    {A, A} -> {make_t(), Env3};
    {[], {symbol, <<"nil">>}} -> {make_t(), Env3};
    {{symbol, <<"nil">>}, []} -> {make_t(), Env3};
    _ -> {make_nil(), Env3}
  end;
exec_builtin(atom, [Arg], Env) ->
  {Evaled, Env2} = eval(Arg, Env),
  case Evaled of
    [] -> {make_t(), Env2};
    _ when is_list(Evaled) -> {make_nil(), Env2};
    _ -> {make_t(), Env2}
  end;
exec_builtin(set, Args, Env) ->
  exec_set(Args, Env);
exec_builtin(error, Args, Env) ->
  {Evaled, Env2} = eval_collect(Args, Env),
  case Evaled of
    [H|T] -> error_(H, T, Env2);
    _ -> error_(Evaled, [], Env2)
  end;
exec_builtin(quote, [Arg], Env) ->
  {Arg, Env};
exec_builtin(cond_, Args, Env) ->
  exec_cond(Args, Env);
exec_builtin(and_, Args, Env) ->
  exec_and(Args, Env);
exec_builtin(or_, Args, Env) ->
  exec_or(Args, Env);
exec_builtin(list, Args, Env) ->
  eval_collect(Args, Env);
exec_builtin(exit, _, _Env) ->
  throw(emlisp_exit);
exec_builtin(B, Args, Env) ->
  error_(undefined_builtin, {B, Args}, Env).

exec_and(Args, Env) -> exec_and(Args, Env, true).
exec_and([], Env, Last) -> {Last, Env};
exec_and([H|T], Env, _Last) ->
  {Evaled, Env2} = eval(H, Env),
  case is_true(Evaled) of
    false -> {make_nil(), Env2};
    true -> exec_and(T, Env2, Evaled)
  end.

exec_or([], Env) -> {make_nil(), Env};
exec_or([H|T], Env) ->
  {Evaled, Env2} = eval(H, Env),
  case is_true(Evaled) of
    true -> {Evaled, Env2};
    false -> exec_or(T, Env2)
  end.

exec_cond([], Env) -> {make_nil(), Env};
exec_cond([[C|B]|T], Env) ->
  {Evaled, Env2} = eval(C, Env),
  case is_true(Evaled) of
    true -> eval(B, Env2);
    false -> exec_cond(T, Env2)
  end.

exec_set(Args, Env) -> exec_set(Args, Env, make_nil()).
exec_set([], Env, Last) -> {Last, Env};
exec_set([K, V|T], Env, _Last) ->
  {EvaledK, Env2} = eval(K, Env),
  case is_symbol(EvaledK) of
    true ->
      {EvaledV, Env3} = eval(V, Env2),
      Env4 = env_set(EvaledK, EvaledV, Env3),
      exec_set(T, Env4, EvaledV);
    false -> error_(cant_set, EvaledK, Env2)
  end.
