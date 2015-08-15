-module(lisp_package).

-compile([export_all]).

-define(PACKAGES, lisp_packages).

-record(package, {name=error(no_name), symbols=error(no_symbols)}).

-record(sym, {name=error(no_name),
              package=error(no_package),
              value=unbound,
              function=unbound,
              plist=[]}).

-type symbol() :: #sym{}.
-type package() :: any().

-spec init() -> ok.
init() ->
  ets:new(?PACKAGES, [public, named_table,
                      {keypos, #package.name}]),
  make_package("lisp"),
  make_package("keyword"),
  make_package("user"),
  ok.

-spec system_package() -> package().
system_package() -> get_package("lisp").

-spec keyword_package() -> package().
keyword_package() -> get_package("keyword").

-spec default_package() -> package().
default_package() -> get_package("user").

-spec make_package(list()) -> package().
make_package(Name) when is_list(Name) ->
  Ets = ets:new(?MODULE, [public, {keypos, #sym.name}]),
  P = #package{name=Name, symbols=Ets},
  ets:insert(?PACKAGES, P),
  P.

-spec delete_package(package()) -> ok.
delete_package(#package{name=Name, symbols=Ets}) ->
  ets:delete(Ets),
  ets:delete(?PACKAGES, Name),
  ok.

-spec get_package(list()) -> package().
get_package(Name) when is_list(Name) ->
  case ets:lookup(?PACKAGES, Name) of
    [P] -> P;
    _ -> throw(package_not_found)
  end.

-spec get_symbol(list()) -> symbol().
get_symbol([$: | Name]) -> get_symbol(keyword_package(), Name);
get_symbol(Name) ->
  case string:tokens(Name, ":") of
    [PName, SName] ->
      get_symbol(get_package(PName), SName);
    _ ->
      get_symbol(default_package(), Name)
  end.

-spec get_symbol(package(), list()) -> symbol().
get_symbol(#package{name=PName, symbols=P}, Name) when is_list(Name) ->
  case ets:lookup(P, Name) of
    [Sym] -> Sym;
    [] ->
      Sym = #sym{name=Name, package=PName},
      ets:insert(P, Sym),
      Sym
  end.

-spec uintern_symbol(symbol()) -> ok.
uintern_symbol(#sym{name=Name, package=P}) ->
  [{_, P}] = ets:lookup(?PACKAGES, P),
  ets:delete(P, Name),
  ok.

symbol_name(#sym{name=Name}) -> Name.
symbol_package(#sym{package=P}) -> get_package(P).

symbol_value(#sym{package="keyword"}=S) -> S;
symbol_value(#sym{package="lisp", name="t"}=S) -> S;
symbol_value(#sym{value=V}) -> V.

symbol_function(#sym{function=F}) -> F.
symbol_plist(#sym{plist=P}) -> P.

symbol_value(#sym{package="keyword"}=S, _) ->
  throw({cant_set, S});
symbol_value(#sym{package="lisp"}=S, _) ->
  throw({cant_set, S});
symbol_value(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{value=V},
  ets:insert(Package, NewS),
  NewS.

symbol_value_internal(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{value=V},
  ets:insert(Package, NewS),
  NewS.

symbol_function(#sym{package="keyword"}=S, _) ->
  throw({cant_set, S});
symbol_function(#sym{package="lisp"}=S, _) ->
  throw({cant_set, S});
symbol_function(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{function=V},
  ets:insert(Package, NewS),
  NewS.

symbol_function_internal(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{function=V},
  ets:insert(Package, NewS),
  NewS.

symbol_plist(#sym{package="keyword"}=S, _) ->
  throw({cant_set, S});
symbol_plist(#sym{package="lisp"}=S, _) ->
  throw({cant_set, S});
symbol_plist(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{plist=V},
  ets:insert(Package, NewS),
  NewS.

symbol_plist_internal(#sym{package=P}=S, V) ->
  Package = get_package(P),
  NewS = S#sym{plist=V},
  ets:insert(Package, NewS),
  NewS.
