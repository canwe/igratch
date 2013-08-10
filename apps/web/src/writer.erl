-module(writer).
-behaviour(gen_server).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feed_state.hrl").
-include("records.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link(Params) -> gen_server:start_link(?MODULE, [Params], []).

init([Params]) ->
  error_logger:info_msg("init worker ~p", [Params]),
  wf:reg(?MAIN_CH),
  {ok, #state{
    owner = proplists:get_value(id,    Params),
    type  = proplists:get_value(type,  Params),
    feeds = proplists:get_value(feeds, Params) }}.

handle_call(_,_,S) -> {reply,ok, S}.
handle_cast(_,S) -> {noreply, S}.
handle_info({delivery, Route, Msg}, #state{callback = M} = S) -> M:handle(Route, Msg, S);
handle_info(_,S) -> {noreply, S}.
terminate(_,_) -> ok.
code_change(_,S,_) -> {ok, S}.
