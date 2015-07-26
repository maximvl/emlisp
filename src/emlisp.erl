-module(emlisp).

-export([start/0, stop/0, re/1, re/2, repl/0, repl/1]).

start() -> application:start(emlisp).
stop() -> application:stop(emlisp).

re(String) ->
  {Res, _Env} = re(String, em_env:null_env()),
  Res.

re(String, Env) ->
  em_core_forms:eval(em_grammar:parse(String), Env).

repl() ->
  ok = application:ensure_started(emlisp),
  repl(em_env:null_env()).

repl(Env) ->
  Line = io:get_line("emlisp> "),
  NewEnv = try
             {Res, Env2} = em_core_forms:eval(em_grammar:parse(Line), Env),
             em_print:print(Res, Env2),
             Env2
           catch throw:{emlisp, Tag, Data} ->
               FTag = em_print:format_object(Tag, Env),
               FData = em_print:format_object(Data, Env),
               io:format("error ~ts: ~ts~n", [FTag, FData]),
               Env
           end,
  repl(NewEnv).
