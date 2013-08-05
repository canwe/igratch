-module(writer).
-behaviour(gen_server).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feed_state.hrl").
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link(Params) -> gen_server:start_link(?MODULE, [Params], []).

init([Params]) ->
  wf:reg(product_channel),
  Id      = proplists:get_value(id,      Params),
  Type    = proplists:get_value(type,    Params),
  Feed    = proplists:get_value(feed,    Params),
  Blog    = proplists:get_value(blog,    Params, undefined),
  Features= proplists:get_value(features,Params, undefined),
  Specs   = proplists:get_value(specs,   Params, undefined),
  Gallery = proplists:get_value(gallery, Params, undefined),
  Videos  = proplists:get_value(videos,  Params, undefined),
  Bundles = proplists:get_value(bundles, Params, undefined),
  error_logger:info_msg("init worker ~p", [Params]),
  {ok, #state{
    owner = Id,
    type =Type, feed = Feed, blog = Blog, features=Features, specs=Specs, gallery=Gallery, videos=Videos, bundles=Bundles}}.

handle_call(_,_From,State) -> {reply,ok, State}.
handle_cast(_M, State) -> {noreply, State}.

handle_info({delivery, Route, Msg}, #state{callback = M} = State)->
  M:handle(Route, Msg, State);

handle_info(_I, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_O, State, _E) -> {ok, State}.
