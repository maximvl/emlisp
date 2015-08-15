%%%-------------------------------------------------------------------
%%% @author  <maxvel>
%%% @copyright (C) 2015,
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2015 by  <maxvel>
%%%-------------------------------------------------------------------
-module(lisp_stream).

-behaviour(gen_server).

%% API
-export([make/1, read/1, unread/2, peek/1, close/1, is_open/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {data=error(no_data), unreaded=[]}).

-type stream_spec() :: list() | binary() | {file, list()}.
-type stream() :: pid().
-type character() :: any().

%%%===================================================================
%%% API
%%%===================================================================

-spec make(stream_spec()) -> stream().
make(A) when is_list(A) ->
  {ok, S} = start_link(A),
  S;
make(A) when is_binary(A) -> make(binary_to_list(A));
make({file, A}) ->
  {ok, Content} = file:read_file(A),
  make(Content).

-spec read(stream()) -> end_of_file | character().
read(S) ->
  case is_process_alive(S) of
    true -> gen_server:call(S, read);
    false -> throw(closed_stream)
  end.

-spec unread(character(), stream()) -> ok.
unread(C, S) ->
  case is_process_alive(S) of
    true -> gen_server:call(S, {unread, C});
    false -> throw(closed_stream)
  end.

-spec peek(stream()) -> end_of_file | character().
peek(S) ->
  case is_process_alive(S) of
    true -> gen_server:call(S, peek);
    false -> throw(closed_stream)
  end.

-spec close(stream()) -> ok.
close(S) ->
  case is_process_alive(S) of
    true -> gen_server:call(S, close);
    false -> ok
  end.

is_open(S) -> is_process_alive(S).

-spec start_link(stream_spec()) -> {ok, pid()}.
start_link(Spec) -> gen_server:start_link(?MODULE, [Spec], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Spec]) ->
  {ok, #state{data=Spec}}.

handle_call(read, _From, #state{unreaded=[C|R]}=State) ->
  {reply, C, State#state{unreaded=R}};

handle_call(read, _From, #state{data=[C|T]}=State) ->
  {reply, C, State#state{data=T}};

handle_call(read, _From, #state{data=[], unreaded=[]}=State) ->
  {stop, normal, end_of_file, State};

handle_call({unread, C}, _From, #state{unreaded=U}=State) ->
  {reply, ok, State#state{unreaded=[C|U]}};

handle_call(peek, _From, #state{unreaded=[C|_]}=State) ->
  {reply, C, State};

handle_call(peek, _From, #state{data=[C|_]}=State) ->
  {reply, C, State};

handle_call(peek, _From, #state{data=[], unreaded=[]}=State) ->
  {reply, end_of_file, State};

handle_call(close, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
