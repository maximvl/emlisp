-module(em_code_walker).

-export([walk/3, macro_expander/2, walk_print/3, add_node/3,
         get_node/2, replace_node/3]).

-include_lib("emlisp/include/emlisp.hrl").

-spec walk(fun(), any(), ast()) -> {ok, any()}.
walk(F, FState, Ast) ->
  case F(term, FState, Ast) of
    {ok, FState2} -> {ok, FState2};
    {inside, FState2} when is_list(Ast) ->
      walk_inside(F, FState2, Ast)
  end.

-spec walk_inside(fun(), any(), list()) -> {ok, any()}.
walk_inside(F, FState, []) -> F(finish, FState, []);
walk_inside(F, FState, [H|T]) ->
  {ok, FState2} = walk(F, FState, H),
  walk_inside(F, FState2, T).

-record(me_state, {ast=[], env, macro=[],
                   list_start=false, level=1}).

macro_expander(Expr, Env) ->
  {ok, #me_state{ast=[Ast]}} = walk(fun macro_expander1/3,
                                    #me_state{ast=[],
                                              env=em_env:get_frame(Env),
                                              macro=[],
                                              list_start=false, level=1},
                                    Expr),
  {ok, Ast}.

macro_expander1(term, #me_state{ast=Ast, level=L}=State,
                [{symbol, <<"quote">>}|_]=A) ->
  {ok, State#me_state{ast=add_node(A, Ast, L)}};
macro_expander1(term, #me_state{level=Level}=State, L) when is_list(L) ->
  {inside, State#me_state{list_start=true, level=Level+1}};
macro_expander1(term, #me_state{list_start=true, env=Env,
                                macro=Macro, ast=Ast, level=L}=State, Atom) ->
  case em_env:env_get1(Atom, Env) of
    {ok, M} ->
      case em_core_forms:is_macro(M) of
        true -> {ok, State#me_state{macro=[M|Macro], list_start=false}};
        false -> {ok, State#me_state{ast=add_node(Atom, Ast, L),
                                     macro=[[]|Macro], list_start=false}}
      end;
    _ -> {ok, State#me_state{ast=add_node(Atom, Ast, L),
                             macro=[[]|Macro], list_start=false}}
  end;
macro_expander1(term, #me_state{ast=Ast, level=L}=State, Atom) ->
  {ok, State#me_state{ast=add_node(Atom, Ast, L)}};
macro_expander1(finish, #me_state{ast=Ast, macro=[], level=L}=State, _) ->
  Node = lists:reverse(get_node(Ast, L-1)),
  NewAst = replace_node(Node, Ast, L-1),
  {ok, State#me_state{ast=NewAst, level=L-1}};
macro_expander1(finish, #me_state{ast=Ast, macro=[[]|M], level=L}=State, _) ->
  Node = lists:reverse(get_node(Ast, L-1)),
  NewAst = replace_node(Node, Ast, L-1),
  {ok, State#me_state{ast=NewAst, macro=M, level=L-1}};
macro_expander1(finish, #me_state{ast=[Args|Ast],
                                  macro=[M|InMacro], level=L}=State, _) ->
  Expanded = em_core_forms:expand_macro(M, lists:reverse(Args)),
  {ok, State#me_state{ast=[Expanded|Ast], macro=InMacro, level=L-1}}.

add_node(X, L, 1) -> [X|L];
add_node(X, [H|R], N) when is_list(H) -> [add_node(X, H, N-1)|R];
add_node(X, L, N) -> add_node(X, [[]|L], N).

get_node([H|_], 1) -> H;
get_node([H|_], N) -> get_node(H, N-1).

replace_node(New, [_|T], 1) -> [New|T];
replace_node(New, [H|T], N) -> [replace_node(New, H, N-1)|T].


walk_print(term, _, L) when is_list(L) ->
  io:format("(", []),
  {inside, {}};
walk_print(term, _, T) ->
  io:format("~p ", [T]),
  {ok, {}};
walk_print(finish, _, _) ->
  io:format(")", []),
  {ok, {}}.
