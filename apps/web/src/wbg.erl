-module(wbg).
-behaviour(gen_server).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/users.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, pid/1]).
-record(state,{}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  wf:reg(product_channel),
  [begin
    Props = [{id, Id}, {type, product}, {feed, F}, {blog, B}, {features, Ft}, {specs, S},{gallery, G}, {videos, V}, {bundles, Bn}],
    workers_sup:start_child(Props)
  end || #product{id=Id, feed=F, blog = B, features = Ft, specs = S, gallery = G, videos =V, bundles=Bn} <-kvs:all(product)],

  [workers_sup:start_child([{id, Id}, {type, group},{feed, F}, {products, D}]) || #group{id=Id, feed=F, products=D} <- kvs:all(group)],
  [workers_sup:start_child([{id, Id}, {type, user}, {feed, F}]) || #user{email = Id, feed=F} <- kvs:all(user)],

  {ok, #state{}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({delivery, Route, Msg}, State)->
  handle_notice(Route, Msg),
  {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_notice([kvs_products, product, init], [Id, F, B, Ft, S, G, V, Bn])->
  Props = [{id, Id},{type, product}, {feed, F}, {blog, B}, {features, Ft}, {specs, S},{gallery, G}, {videos, V}, {bundles, Bn}],
  workers_sup:start_child(Props);
handle_notice([kvs_group, group, init], [Id, Fid, PFid])->
  workers_sup:start_child([{id, Id}, {type, group}, {feed, Fid}, {products, PFid}]);
handle_notice(_R, _M) -> skip.

pid(Name) ->
  R=[Pid||{{p,l,SName},Pid,_Value}<-qlc:e(gproc:table()),Name==SName],
  case R of [] -> undefined; [A] -> A; _ -> ambiguous end.
