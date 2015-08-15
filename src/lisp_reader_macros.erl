-module(lisp_reader_macros).

-compile([export_all]).

-define(MACROS, ?MODULE).

-type character() :: lisp_stream:character().
-type stream() :: lisp_stream:stream().
-type lisp_object() :: lisp_reader:lisp_object().
-type lisp_list() :: lisp_reader:lisp_list().
-type lisp_string() :: lisp_reader:lisp_string().

-record(reader_macro, {char :: character(),
                       function :: function()}).

-type reader_macro() :: #reader_macro{}.

-spec init() -> ok.
init() ->
  ets:new(?MACROS, [public, named_table,
                    {keypos, #reader_macro.char}]),
  set($(, fun left_paren/2),
  set($', fun single_quote/2),
  set($;, fun semicolon/2),
  set($", fun double_quote/2),
  set($`, fun backquote/2),
  set($,, fun comma/2),
  set($#, fun sharpsign/2),
  ok.

-spec set(character(), function()) -> reader_macro().
set(Char, Macro) when is_function(Macro, 2) ->
  M = #reader_macro{char=Char, function=Macro},
  ets:insert(?MACROS, M),
  M.

-spec unset(character()) -> ok.
unset(Char) ->
  ets:delete(?MACROS, Char),
  ok.

-spec get(character()) -> not_found | reader_macro().
get(Char) ->
  case ets:lookup(?MACROS, Char) of
    [M] -> M;
    [] -> not_found
  end.

-spec eval(reader_macro(), character(), stream()) -> lisp_object().
eval(#reader_macro{function=F}, X, Stream) -> F(X, Stream).

-spec left_paren(character(), stream()) -> lisp_list().
left_paren(_, Stream) -> left_paren1(Stream, []).

-spec left_paren1(stream(), [lisp_object()]) -> lisp_list().
left_paren1(Stream, Acc) ->
  case lisp_stream:peek(Stream) of
    end_of_file -> throw(end_of_file);
    $) ->
      %% consume found paren
      lisp_stream:read(Stream),
      lisp_objects:make_list(lists:reverse(Acc));
    _ ->
      Next = lisp_reader:read(Stream, true, [], true),
      left_paren1(Stream, [Next|Acc])
  end.

-spec single_quote(character(), stream()) -> lisp_list().
single_quote(_, Stream) ->
  Next = lisp_reader:read(Stream, true, [], true),
  [lisp_package:get_symbol("lisp:quote"), Next].

-spec semicolon(character(), stream()) -> nothing.
semicolon(_, Stream) ->
  case lisp_stream:read(Stream) of
    end_of_file -> nothing;
    $\n -> nothing;
    _ -> semicolon({}, Stream)
  end.

-spec double_quote(character(), stream()) -> lisp_string().
double_quote(_, Stream) -> double_quote1(Stream, []).

-spec double_quote1(stream(), string()) -> lisp_string().
double_quote1(Stream, Acc) ->
  case lisp_stream:read(Stream) of
    end_of_file -> throw(end_of_file);
    $" -> lisp_objects:make_string(lists:reverse(Acc));
    Next ->
      case lisp_reader:syntax_type(Next) of
        single_escape ->
          case lisp_stream:read(Stream) of
            end_of_file -> throw(end_of_file);
            Next2 -> double_quote1(Stream, [Next2|Acc])
          end;
        _ -> double_quote1(Stream, [Next|Acc])
      end
  end.

-spec backquote(character(), stream()) -> lisp_object().
backquote(_, _Stream) -> throw(not_implemented).

-spec comma(character(), stream()) -> nothing.
comma(_, _) -> throw(comma_not_inside_backquote).

-spec sharpsign(character(), stream()) -> lisp_object().
sharpsign(_, _Stream) -> throw(not_implemented).
