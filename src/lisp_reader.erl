-module(lisp_reader).

-compile([export_all]).

-define(LINE_FEED, 13).
-define(RUBOUT, 127).

-type syntax_type() :: constituent | whitespace |
                       terminating_macro | non_terminating_macro |
                       single_escape | multiple_escape | invalid.

-type stream() :: lisp_stream:stream().
-type character() :: lisp_stream:character().
-type lisp_object() :: any().

%% -record(reader_state, {stream=error(no_stream),
%%                        eof_error_p=false,
%%                        eof_value=nil,
%%                        recursive_p=false}).

-spec read(stream()) -> lisp_object() | nil.
read(S) -> read(S, true, nil).

-spec read(stream(), boolean(), any()) -> lisp_object() | any().
read(S, EofErrorP, EofValue) -> read(S, EofErrorP, EofValue, false).

-spec read(stream(), boolean(), any(), boolean()) -> lisp_object() | any().
read(S, EofErrorP, EofValue, RecursiveP) ->
  case lisp_stream:read(S) of
    end_of_file ->
      case {EofErrorP, RecursiveP} of
        {false, false} -> EofValue;
        _ -> throw(end_of_file)
      end;
    X -> dispatch(syntax_type(X), X, S, EofErrorP, EofValue)
  end.

-spec dispatch(syntax_type(), character(), stream(), boolean(), any()) -> ok.
dispatch(invalid, _X, _S, _EofE, _Eof) -> throw(invalid_character);
dispatch(whitespace, _X, S, EofE, Eof) -> read(S, EofE, Eof);
dispatch(terminating_macro, X, S, EofE, Eof) ->
  call_reader_macro_fun(X, S, EofE, Eof);
dispatch(non_terminating_macro, X, S, EofE, Eof) ->
  call_reader_macro_fun(X, S, EofE, Eof);
dispatch(single_escape, _X, S, EofE, Eof) ->
  case lisp_stream:read(S) of
    end_of_file -> throw(end_of_file);
    Next -> dispatch(constituent, Next, S, EofE, Eof)
  end;
dispatch(multiple_escape, _X, S, _EofE, _Eof) ->
  read_escaped_token([], S);
dispatch(constituent, X, S, _EofE, _Eof) ->
  case read_token([X], S) of
    invalid -> throw(invalid_character);
    T -> T
  end.

-spec read_token(character(), stream()) -> invalid | lisp_object().
read_token(Acc, S) ->
  case lisp_stream:read(S) of
    end_of_file -> token_to_object(lists:reverse(Acc));
    Y ->
      case syntax_type(Y) of
        T when T == constituent orelse T == non_terminating_macro ->
          read_token([Y|Acc], S);
        single_escape ->
          case lisp_stream:read(S) of
            end_of_file -> throw(end_of_file);
            Z -> read_token([Z|Acc], S)
          end;
        multiple_escape -> read_escaped_token(Acc, S);
        invalid -> throw(invalid_character);
        terminating_macro ->
          lisp_stream:unread(Y, S),
          token_to_object(lists:reverse(Acc));
        whitespace ->
          %% If y is a whitespace[2] character, then it terminates the token
          %% First the character y is unread if appropriate
          %% (see read-preserving-whitespace), and then step 10 is entered.
          %% lisp_stream:unread(Y, S),
          token_to_object(lists:reverse(Acc))
      end
  end.

read_escaped_token(Acc, S) ->
  case lisp_stream:read(S) of
    end_of_file -> throw(end_of_file);
    Y ->
      case syntax_type(Y) of
        T when T == constituent orelse T == terminating_macro orelse
               T == non_terminating_macro orelse T == whitespace ->
          read_escaped_token([Y|Acc], S);
        single_escape ->
          case lisp_stream:read(S) of
            end_of_file -> throw(end_of_file);
            Z -> read_escaped_token([Z|Acc], S)
          end;
        multiple_escape -> read_token(Acc, S);
        invalid -> throw(invalid_character)
      end
  end.

-spec token_to_object([character()]) -> invalid | lisp_object().
token_to_object(T) ->
  case token_to_number(T) of
    {ok, Num} -> lisp_objects:make_number(Num);
    _ ->
      case lists:all(fun(E) -> E == $. end, T) of
        true -> invalid;
        false -> token_to_symbol(T)
      end
  end.

token_to_number(T) ->
  try {ok, erlang:list_to_integer(T)}
  catch _:badarg ->
      try {ok, erlang:list_to_float(T)}
      catch _:badarg ->
          error
      end
  end.

token_to_symbol(T) -> lisp_package:get_symbol(T).

call_reader_macro_fun(X, S, EofE, Eof) ->
  case lisp_reader_macros:get(X) of
    not_found -> throw({reader_macro_not_found, X});
    F -> case lisp_reader_macros:eval(F, X, S) of
           nothing -> read(S, EofE, Eof, false);
           Object -> Object
         end
  end.


%% character  syntax type                 character  syntax type
%% Backspace  constituent                 0--9       constituent
%% Tab        whitespace[2]               :          constituent
%% Newline    whitespace[2]               ;          terminating macro char
%% Linefeed   whitespace[2]               <          constituent
%% Page       whitespace[2]               =          constituent
%% Return     whitespace[2]               >          constituent
%% Space      whitespace[2]               ?          constituent*
%% !          constituent*                @          constituent
%% "          terminating macro char      A--Z       constituent
%% #          non-terminating macro char  [          constituent*
%% $          constituent                 \          single escape
%% %          constituent                 ]          constituent*
%% &          constituent                 ^          constituent
%% '          terminating macro char      _          constituent
%% (          terminating macro char      `          terminating macro char
%% )          terminating macro char      a--z       constituent
%% *          constituent                 {          constituent*
%% +          constituent                 |          multiple escape
%% ,          terminating macro char      }          constituent*
%% -          constituent                 ~          constituent
%% .          constituent                 Rubout     constituent
%% /          constituent

-spec syntax_type(character()) -> syntax_type().
syntax_type($\b) -> constituent;
syntax_type($\t) -> whitespace;
syntax_type($\n) -> whitespace;
syntax_type(?LINE_FEED) -> whitespace;
syntax_type($ ) -> whitespace;
syntax_type($!) -> constituent;
syntax_type($") -> terminating_macro;
syntax_type($#) -> non_terminating_macro;
syntax_type($$) -> constituent;
syntax_type($%) -> constituent;
syntax_type($&) -> constituent;
syntax_type($') -> terminating_macro;
syntax_type($() -> terminating_macro;
syntax_type($)) -> terminating_macro;
syntax_type($*) -> constituent;
syntax_type($+) -> constituent;
syntax_type($,) -> terminating_macro;
syntax_type($-) -> constituent;
syntax_type($.) -> constituent;
syntax_type($/) -> constituent;

syntax_type(N) when N >= $0 andalso N =< $9 -> constituent;
syntax_type($:) -> constituent;
syntax_type($;) -> terminating_macro;
syntax_type($<) -> constituent;
syntax_type($=) -> constituent;
syntax_type($>) -> constituent;
syntax_type($?) -> constituent;
syntax_type($@) -> constituent;
syntax_type(A) when A >= $A andalso A =< $Z -> constituent;
syntax_type($[) -> constituent;
syntax_type($\\) -> single_escape;
syntax_type($]) -> constituent;
syntax_type($^) -> constituent;
syntax_type($_) -> constituent;
syntax_type($`) -> terminating_macro;
syntax_type(A) when A >= $a andalso A =< $z -> constituent;
syntax_type(${) -> constituent;
syntax_type($|) -> multiple_escape;
syntax_type($}) -> constituent;
syntax_type($~) -> constituent;
syntax_type(?RUBOUT) -> constituent;
syntax_type(_) -> invalid.
