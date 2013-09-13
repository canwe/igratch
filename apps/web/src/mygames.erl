-module(mygames).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include_lib("feed_server/include/records.hrl").

-include("records.hrl").


main()-> #dtl{file="prod", bindings=[{title,<<"my games">>},{body, body()}]}.

body()-> Nav = {wf:user(), mygames, []},
    Id = case lists:keyfind(products, 1, element(#iterator.feeds, case wf:user() of undefined-> #user{}; U->U end)) of {_, V} -> V; false -> undefined end,
    Fs = ?FD_STATE(Id)#feed_state{view=product, html_tag=panel, entry_id=#entry.entry_id, enable_selection=true},
    Is = #input_state{show_upload=true, entry_type=product, show_price=true},
    index:header() ++ dashboard:page(Nav, [
        #feed2{title= <<"My games">>, icon="icon-gamepad", state=Fs,
            header=[
            #input{title= <<"New Game">>,placeholder_rcp= <<"Categories">>, placeholder_ttl= <<"Game title">>, role=group, state=Is, feed_state=Fs, class=["feed-table-header"]}
        ]}
    ]) ++index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({update, #product{}=P, #input_state{}=Is}) ->
  User = wf:user(),
  Title = wf:q(Is#input_state.title_id),
  Descr = wf:q(Is#input_state.body_id),
  Currency = wf:q(Is#input_state.currency_id),

  TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
  Medias = case wf:session(medias) of undefined -> []; L -> L end,

  Product = P#product{
    title = list_to_binary(Title),
    brief = list_to_binary(Descr),
    cover = TitlePic,
    price = product_ui:to_price(wf:q(Is#input_state.price_id)),
    currency = Currency},

  Entry = #entry{
    created=Product#product.created,
    entry_id=Product#product.id,
    from=Product#product.owner,
    type= product,
    media=Medias,
    title=Product#product.title,
    description=Product#product.brief,
    shared=""},

  kvs:put(Product),

  Cats = wf:q(Is#input_state.recipients_id),
    error_logger:info_msg("Categories: ~p", [Cats]),
  Groups = ordsets:from_list([case kvs:get(group,S) of {error,_}->[]; {ok,G} ->G end || S<-string:tokens(Cats, ",")]),
  Participate = ordsets:from_list([ case kvs:get(group, Where) of {ok, G}->G;_->[] end  || #group_subscription{where=Where} <- kvs_group:participate(P#product.id)]),
  Intersection = ordsets:intersection(Groups, Participate),
  OldSubs = ordsets:subtract(Participate, Intersection),
  NewSubs = ordsets:subtract(Groups, Intersection),

  % stay in group and edit entry
  Rec1 = [{user, P#product.owner, lists:keyfind(products, 1, User#user.feeds)} |
    [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Intersection]],
  error_logger:info_msg("Edit recipients: ~p~n", [Rec1]),

  [msg:notify([kvs_feed, RouteType, To, entry, Eid, edit],
    Entry#entry{to = {RouteType, To}, feed_id=Fid, id={Product#product.id, Fid}}) || {RouteType, To, {_,Fid}=Eid} <- Rec1],

  % leave group and remove entry
  Rec2 = [{group,Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- OldSubs],
  error_logger:info_msg("Remove recipients: ~p~n", [Rec2]),
  [msg:notify([kvs_feed, RouteType, To, entry, Eid, delete], [Entry, skip]) || {RouteType, To, Eid} <- Rec2],
  [msg:notify([kvs_group, Product#product.id, leave, G], {}) || {_,G,_} <- Rec2],
  [kvs_group:leave(Product#product.id, G) || {_,G,_} <- Rec2],

  % join group and add entry
  Rec3 = [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- NewSubs],
  [kvs_group:join(Product#product.id, G) ||{_, G,_} <- Rec3],
  error_logger:info_msg("Add recipients: ~p~n", [Rec3]),
  [kvs_group:join(Product#product.id, G) || {_,G,_} <- Rec3],
  [msg:notify([kvs_group, Product#product.id, join, G], {}) || {_,G,_} <- Rec3],
  [msg:notify([kvs_feed, RoutingType, To, entry, Product#product.id, add],
  [Entry#entry{id={Product#product.id, Fid},feed_id=Fid,to = {RoutingType, To}},skip,skip,skip,skip,To]) || {RoutingType, To, {_, Fid}} <- Rec3],

  event({edit_product, #entry{}});
event({read, product, {Id,_}})-> wf:redirect(?URL_PRODUCT(Id));
event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event({edit_product, #entry{}=E})->
    wf:session(medias, E#entry.media),
%    wf:update(wf:session(game_input), input(E)),
    wf:wire("$('.selectpicker').each(function() {var $select = $(this); $select.selectpicker($select.data());});");
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed2:process_delivery(R,M).
