-module(wbg).
-behaviour(gen_server).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-record(state,{}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  wf:reg(?MAIN_CH),

  [handle_notice([ok, user,    init], [Id, Feeds]) || #user{email=Id, feeds=Feeds} <- kvs:all(user)],
  [handle_notice([ok, group,   init], [Id, Feeds]) || #group{id = Id, feeds=Feeds} <- kvs:all(group)],
  [handle_notice([ok, product, init], [Id, Feeds]) || #product{id=Id, feeds=Feeds} <- kvs:all(product)],

  {ok, #state{}}.

handle_call(_,_,S) -> {reply, ok, S}.
handle_cast(_,S) -> {noreply, S}.
handle_info({delivery, Route, Msg}, S) -> handle_notice(Route, Msg), {noreply, S};
handle_info(_,S) -> {noreply, S}.
terminate(_,_S) -> ok.
code_change(_,S,_) -> {ok, S}.

handle_notice([_, Type, init], [Id, Feeds]) -> workers_sup:start_child([{id, Id}, {type, Type}, {feeds, Feeds}]);
handle_notice(_,_) -> skip.