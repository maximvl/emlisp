-module(lisp_objects).

-compile([export_all]).

-type symbol() :: lisp_package:symbol().
-type package() :: lisp_package:package().

-type lisp_string() :: {string, list()}.
-type lisp_number() :: number().
-type lisp_list() :: [lisp_object()].
-type lisp_boolean() :: lisp_object().

-type lisp_object() :: lisp_list() | symbol() | package() |
                       lisp_string() | lisp_number() | lisp_boolean().

-spec make_string(list()) -> lisp_string().
make_string(S) when is_list(S) -> {string, S}.

-spec make_list([lisp_object()]) -> lisp_list().
make_list(L) when is_list(L) -> L.

-spec make_number(number()) -> lisp_number().
make_number(N) when is_number(N) -> N.

-spec make_boolean(boolean()) -> lisp_boolean().
make_boolean(true) -> lisp_package:get_symbol("lisp:t");
make_boolean(false) -> [].
