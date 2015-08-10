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
         env_add_many/2, env_keep/1, env_unkeep/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("emlisp/include/emlisp.hrl").

-define(SERVER, ?MODULE).
-define(GLOBAL, ?MODULE).
-define(FRAMES, ?MODULE).

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

-spec global_env() -> env().
global_env() -> #frame{
                   addr=global,
                   enclosing_addr=undefined,
                   binds={ets, ?GLOBAL}}.

-spec env_add_frame(env()) -> env().
env_add_frame(#frame{addr=Addr}) ->
  Frame = #frame{addr=make_id(), enclosing_addr=Addr, uses=1},
  ets:insert(?FRAMES, Frame),
  Frame.

-spec env_add(any(), any(), env()) -> env().
env_add(K, V, #frame{binds={ets, Tab}}=F) ->
  ets:insert(Tab, {K, V}),
  F;
env_add(K, V, #frame{addr=A, binds=Binds}=F) ->
  Binds2 = dict:store(K, V, Binds),
  ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
  F#frame{binds=Binds2}.

-spec env_add_many([{any(), any()}], env()) -> env().
env_add_many(KVs, #frame{binds={ets, Tab}}=F) ->
  ets:insert(Tab, KVs),
  F;
env_add_many(KVs, #frame{addr=A, binds=Binds}=F) ->
  Binds2 = lists:foldl(fun({K, V}, IBinds) ->
                           dict:store(K, V, IBinds)
                       end, Binds, KVs),
  ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
  F#frame{binds=Binds2}.

-spec env_set(any(), any(), env()) -> env().
env_set(K, V, Env) ->
  case env_set1(K, V, Env) of
    {error, E} -> em_core_forms:error_(E, K, Env);
    Env2 -> Env2
  end.

-spec env_set1(any(), any(), env() | undefined) ->
                  env() | {error, env_not_found}.
env_set1(_K, _V, undefined) -> {error, env_not_found};
env_set1(K, V, #frame{binds={ets, Tab}}=F) ->
  case ets:lookup(Tab, K) of
    [{K, _}] -> ets:update_element(Tab, K, {2, V});
    [] ->
      env_set1(K, V, enclosing_frame(F))
  end;
env_set1(K, V, #frame{addr=A, binds=Binds}=F) ->
  case dict:find(K, Binds) of
    {ok, _} ->
      Binds2 = dict:store(K, V, Binds),
      ets:update_element(?FRAMES, A, {#frame.binds, Binds2}),
      F#frame{binds=Binds2};
    error ->
      env_set1(K, V, enclosing_frame(F))
  end.

-spec env_get(any(), env()) -> any().
env_get(K, Env) ->
  case env_get1(K, Env) of
    {ok, Val} -> Val;
    {error, E} -> em_core_forms:error_(E, K, Env)
  end.

-spec env_get1(any(), env() | undefined) ->
                  {ok, any()} | {error, env_not_found}.
env_get1(_K, undefined) -> {error, env_not_found};
env_get1(K, #frame{binds={ets, Tab}}=F) ->
  case ets:lookup(Tab, K) of
    [{K, Val}] -> {ok, Val};
    [] -> env_get1(K, enclosing_frame(F))
  end;
env_get1(K, #frame{binds=Binds}=F) ->
  case dict:find(K, Binds) of
    {ok, Val} -> {ok, Val};
    error -> env_get1(K, enclosing_frame(F))
  end.

-spec env_keep(env()) -> env().
env_keep(#frame{addr=A, uses=N}=Frame) ->
  ets:update_element(?FRAMES, A, {#frame.uses, N+1}),
  Frame#frame{uses=N+1}.

env_unkeep(#frame{binds={ets, ?GLOBAL}}) -> ok;
env_unkeep(#frame{addr=A, uses=1, binds={ets, Tab}}) ->
  ets:delete(?FRAMES, A),
  ets:delete(Tab);
env_unkeep(#frame{addr=A, uses=1}) ->
  ets:delete(?FRAMES, A);
env_unkeep(#frame{addr=A, uses=N}) ->
  ets:update_element(?FRAMES, A, {#frame.uses, N-1}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?FRAMES, [public, named_table, {keypos, #frame.addr}]),
  ets:new(?GLOBAL, [public, named_table]),
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

-spec enclosing_frame(frame()) -> frame() | undefined.
enclosing_frame(#frame{enclosing_addr=undefined}) -> undefined;
enclosing_frame(#frame{enclosing_addr=A}) ->
  [{A, Frame}] = ets:lookup(?FRAMES, A),
  Frame.
