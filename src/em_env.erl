%%%-------------------------------------------------------------------
%%% @author  <maxvel>
%%% @copyright (C) 2015,
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2015 by  <maxvel>
%%%-------------------------------------------------------------------
-module(em_env).

-behaviour(gen_server).

%% API
-export([start_link/0, global_env/0, env_add/3,
         env_add_frame/1, env_set/3, env_get/2, env_get1/2,
         env_add_many/2, env_keep/1, env_free_frame/1]).

-export([enclosing_frame/1, get_frame/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("emlisp/include/emlisp.hrl").

-define(SERVER, ?MODULE).
-define(GLOBAL, em_env_global).
-define(FRAMES, em_env_frames).

-type id() :: integer().
-type addr() :: id() | global.
-type bindings() :: dict:dict() | {ets, atom()}.

-record(state, {}).
-record(frame, {addr=error(no_addr)                    :: addr(),
                enclosing_addr=error(no_enclosing_adr) :: addr() | undefined,
                uses=0                                 :: integer(),
                binds=dict:new()                       :: bindings()}).

-type frame() :: #frame{}.
-type env() :: frame().

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec global_env() -> addr().
global_env() -> global.

-spec env_add_frame(addr()) -> addr().
env_add_frame(Addr) ->
  NewAddr = make_id(),
  Frame = #frame{addr=NewAddr, enclosing_addr=Addr, uses=1},
  ets:insert(?FRAMES, Frame),
  %% maybe update uses for addr?
  NewAddr.

-spec env_add(any(), any(), addr()) -> ok.
env_add(K, V, A) ->
  env_add1(K, V, get_frame(A)).

-spec env_add1(any(), any(), env()) -> ok.
env_add1(K, V, #frame{binds={ets, Tab}}) ->
  ets:insert(Tab, {K, V}),
  ok;
env_add1(K, V, #frame{addr=A, binds=Binds}) ->
  Binds2 = dict:store(K, V, Binds),
  ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
  ok.

-spec env_add_many([{any(), any()}], addr()) -> ok.
env_add_many(KVs, Addr) ->
  env_add_many1(KVs, get_frame(Addr)).

-spec env_add_many1([{any(), any()}], env()) -> ok.
env_add_many1(KVs, #frame{binds={ets, Tab}}) ->
  ets:insert(Tab, KVs),
  ok;
env_add_many1(KVs, #frame{addr=A, binds=Binds}) ->
  Binds2 = lists:foldl(fun({K, V}, IBinds) ->
                           dict:store(K, V, IBinds)
                       end, Binds, KVs),
  ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
  ok.

-spec env_set(any(), any(), addr()) -> ok.
env_set(K, V, Addr) ->
  case env_set1(K, V, get_frame(Addr)) of
    {error, _E} ->
      %% em_core_forms:error_(E, K, Env);
      env_add(K, V, global_env());
    _ -> ok
  end.

-spec env_set1(any(), any(), env() | undefined) ->
                  ok | {error, env_not_found}.
env_set1(_K, _V, undefined) -> {error, env_not_found};
env_set1(K, V, #frame{binds={ets, Tab}}=F) ->
  case ets:lookup(Tab, K) of
    [{_, _}] ->
      ets:update_element(Tab, K, {2, V}), ok;
    [] ->
      env_set1(K, V, enclosing_frame(F))
  end;
env_set1(K, V, #frame{addr=A, binds=Binds}=F) ->
  case dict:find(K, Binds) of
    {ok, _} ->
      Binds2 = dict:store(K, V, Binds),
      ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
      ok;
    error ->
      env_set1(K, V, enclosing_frame(F))
  end.

-spec env_get(any(), addr()) -> any().
env_get(K, Env) ->
  case env_get1(K, get_frame(Env)) of
    {ok, Val} -> Val;
    {error, E} -> em_core_forms:error_(E, K, Env)
  end.

-spec env_get1(any(), env() | undefined) ->
                  {ok, any()} | {error, env_not_found}.
env_get1(_K, undefined) -> {error, env_not_found};
env_get1(K, #frame{binds={ets, Tab}}=F) ->
  case ets:lookup(Tab, K) of
    [{_, Val}] -> {ok, Val};
    [] -> env_get1(K, enclosing_frame(F))
  end;
env_get1(K, #frame{binds=Binds}=F) ->
  case dict:find(K, Binds) of
    {ok, Val} -> {ok, Val};
    error -> env_get1(K, enclosing_frame(F))
  end.

-spec env_keep(addr()) -> ok.
env_keep(A) ->
  ets:update_counter(?FRAMES, A, {#frame.uses, 1}),
  ok.

-spec env_free_frame(addr()) -> ok.
env_free_frame(Addr) ->
  env_free_frame1(get_frame(Addr)).

env_free_frame1(#frame{binds={ets, ?GLOBAL}}) -> ok;
env_free_frame1(#frame{addr=A, binds=Binds}) ->
  case ets:update_counter(?FRAMES, A, {#frame.uses, -1}) of
    0 ->
      ets:delete(?FRAMES, A),
      case Binds of
        {ets, Tab} -> ets:delete(Tab), ok;
        _ -> ok
      end;
    _ -> ok
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?FRAMES, [public, named_table, {keypos, #frame.addr}]),
  ets:new(?GLOBAL, [public, named_table]),
  GlobalFrame = #frame{addr=global, enclosing_addr=undefined,
                       binds={ets, ?GLOBAL}},
  ets:insert(?FRAMES, GlobalFrame),
  io:format("DBG: ~p~n", [ets:lookup(?FRAMES, global_env())]),
  em_core_forms:fill_std_env(),
  {ok, #state{}, hibernate}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
-spec make_id() -> id().
make_id() ->
  {X, Y, Z} = erlang:now(),
  M = 1000 * 1000,
  X * M * M + Y * M + Z.

-spec enclosing_frame(frame() | addr()) -> frame() | undefined.
enclosing_frame(#frame{enclosing_addr=undefined}) -> undefined;
enclosing_frame(#frame{enclosing_addr=A}) ->
  [Frame] = ets:lookup(?FRAMES, A),
  Frame.

-spec get_frame(addr()) -> env().
get_frame(A) ->
  [Frame] = ets:lookup(?FRAMES, A),
  Frame.
