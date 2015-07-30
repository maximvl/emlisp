-module(em_ast).
-export([transform/3]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(program, [_, Node, _], _Index) ->
  Node;

%% CELLS
transform(cell, [_, [<<"'">>, Cell], _], _Index) ->
  [{symbol, <<"quote">>}, Cell];
transform(cell, [_, Node, _], _Index) ->
  Node;

%% LISTS
transform(list, [<<"(">>, Cells, <<")">>], _Index) -> Cells;

%% DATA
transform(integer, Node, _Index) ->
  binary_to_integer(iolist_to_binary(Node));
transform(float, Node, _Index) ->
  binary_to_float(iolist_to_binary(Node));
transform(symbol, Node, _Index) ->
  S = iolist_to_binary(Node),
  case S of
    <<"nil">> -> [];
    _ -> {symbol, S}
  end;
transform(string, [<<"\"">>, Node, <<"\"">>], _Index) ->
  {string, iolist_to_binary(Node)};
transform(erlmf, [Mod, <<":">>, Fun], _Index) ->
  ErlMod = erlang:iolist_to_binary(Mod),
  ErlFun = erlang:iolist_to_binary(Fun),
  {erlmf, ErlMod, ErlFun};
transform(erlmfa, [{erlmf, Mod, Fun}, <<"/">>, Arity], _Index) ->
  ErlMod = erlang:iolist_to_binary(Mod),
  ErlFun = erlang:iolist_to_binary(Fun),
  {erlmfa, ErlMod, ErlFun, Arity};

transform(space, _, _Index) ->
  {space};
transform(atom, [_, Node, _], _Index) ->
  Node;
transform(Symbol, Node, _Index) when is_atom(Symbol) ->
  {Symbol, Node}.
