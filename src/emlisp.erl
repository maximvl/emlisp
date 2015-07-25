-module(emlisp).

-export([re/1, re/2, repl/0, repl/1]).

re(String) ->
  {Res, _Env} = re(String, em_core_forms:std_env()),
  Res.

re(String, Env) ->
  em_core_forms:eval(em_grammar:parse(String), Env).

repl() ->
  repl(em_core_forms:std_env()).

repl(Env) ->
  Line = io:get_line("emlisp> "),
  NewEnv = try
             {Res, Env2} = em_core_forms:eval(em_grammar:parse(Line), Env),
             em_print:print(Res),
             Env2
           catch throw:{emlisp, Tag, Data} ->
               em_print:print("error ~p : ~p~n", [Tag, Data]),
               Env
           end,
  repl(NewEnv).
