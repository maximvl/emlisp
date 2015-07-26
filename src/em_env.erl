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
-export([start_link/0, null_env/0, global_env/0,
         env_add_frame/1, env_set/3, env_get/2,
         env_set_many/2, env_set_global/2, env_merge/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("emlisp/include/emlisp.hrl").

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec null_env() -> env().
null_env() -> [].

-spec global_env() -> env().
global_env() -> [{ets, ?TAB}].

-spec env_add_frame(env()) -> env().
env_add_frame(Env) -> [dict:new()|Env].

-spec env_set(any(), any(), env()) -> env().
env_set(K, V, []=Env) ->
  env_set_global(K, V),
  Env;
env_set(K, V, [{ets, Tab}|_]=Env) ->
  ets:insert(Tab, {K, V}),
  Env;
env_set(K, V, [Frame|Rest]) ->
  [dict:store(K, V, Frame)|Rest].

env_set_global(K, V) ->
  ets:insert(?TAB, {K, V}).

env_set_many(KVs, []=Env) ->
  ets:insert(?TAB, KVs),
  Env;
env_set_many(KVs, [{ets, Tab}|_]=Env) ->
  ets:insert(Tab, KVs),
  Env;
env_set_many(KVs, [Frame1|Rest]) ->
  Frame2 = lists:foldl(fun({K, V}, Frame) ->
                           dict:store(K, V, Frame)
                       end, Frame1, KVs),
  [Frame2|Rest].

-spec env_get(any(), env()) -> any().
env_get(K, Envs) -> env_get1(K, Envs, Envs).

env_get1(K, [], Envs) ->
  case ets:lookup(?TAB, K) of
    [{K, Val}] -> Val;
    _ -> em_core_forms:error_(env_not_found, K, Envs)
  end;
env_get1(K, [{ets, Tab}|Rest], Envs) ->
  case ets:lookup(Tab, K) of
    [{K, Val}] -> Val;
    _ -> env_get1(K, Rest, Envs)
  end;
env_get1(K, [Frame|Rest], Envs) ->
  case dict:find(K, Frame) of
    {ok, Val} -> Val;
    error -> env_get1(K, Rest, Envs)
  end.

env_merge(Frames) ->
  NewEnv = env_add_frame(null_env()),
  lists:foldl(fun(Frame, Acc) ->
                  KVs = dict:to_list(Frame),
                  env_set_many(KVs, Acc)
              end, NewEnv, lists:reverse(Frames)).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?TAB, [public, named_table]),
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
