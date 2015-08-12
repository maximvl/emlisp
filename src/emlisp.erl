-module(emlisp).

-export([start/0, stop/0, re/1, repl/0, repl/1]).

start() -> application:start(emlisp).
stop() -> application:stop(emlisp).

re(String) ->
  em_core_forms:eval(em_grammar:parse(String)).

repl() ->
  ok = application:ensure_started(emlisp),
  repl(em_env:global_env()).

repl(Env) ->
  Line = io:get_line("emlisp> "),
  try
    Res = em_core_forms:eval(em_grammar:parse(Line), Env),
    em_print:print(Res, Env)
  catch throw:{emlisp, Tag, Data} ->
      FTag = em_print:format_object(Tag, Env),
      FData = em_print:format_object(Data, Env),
      io:format("error ~ts: ~ts~n", [FTag, FData])
  end,
  repl(Env).
