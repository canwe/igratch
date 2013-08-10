-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"product">>},{body, body()}]}.

body() ->
  Id = case wf:qs(<<"id">>) of undefined -> <<"no">>; I-> I end,
%  case wf:qs(<<"tab">>) of undefined -> wf:wire("$('a[href=\"#features\"]').tab('show');"); Tab -> wf:wire(io_lib:format("$('a[href=\"#~s\"]').tab('show');",[Tab])) end,
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, binary_to_list(Id)) of
      {ok, P} -> [
        #panel{class=["row-fluid", "page-header"], body=[
          #h4{class=[span9], style="line-height:30px;", body= [#link{url= <<"/reviews">>, body= <<"Categories ">>, style="color:#999"}, #small{body=[[
            begin
              Name = case kvs:get(group,I) of {ok, G}-> G#group.name; _ -> "noname" end,
              [<<" | ">>, #link{url="/store?id="++I, body=[#span{class=["icon-asterisk"]},Name]}]
            end
          ] || #group_subscription{where=I} <- kvs_group:participate(P#product.id)]} ]},

          #panel{class=[span3, "input-append"], style="margin:10px 0", body=[
            #textbox{id="search-button", placeholder= <<"Search">>},
            #button{class=[btn], body= <<"Go!">>}
          ]}
        ]},
        #section{class=[section, alt], body=#panel{class=[container], body=[
          essential(P)
        ]}},
        #section{class=[section], body=#panel{class=[container], body=#panel{class=["row-fluid"], body=[
          #panel{class=[span9], body=details(P)},
          #panel{class=[span3], body=aside()}
        ]}}} ];
      {error, E} -> #panel{class=[alert, "alert-danger","alert-block"], body=[
        #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>},
        #strong{body= atom_to_list(E)} ]}
    end
  }}
  ]++index:footer().

essential(P)->[
  #product_hero{product=P},
  #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
    #li{class=[active], body=[#link{url= <<"#features">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Features">>}]},
    #li{body=[#link{url= <<"#specs">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Specification">>}]},
    #li{body=[#link{url= <<"#gallery">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Gallery">>}]},
    #li{body=[#link{url= <<"#videos">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Videos">>}]},
    #li{body=[#link{url= <<"#reviews">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Reviews">>}]},
    #li{body=[#link{url= <<"#news">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"News">>}]},
    #li{body=[#link{url= <<"#bundles">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Bundles">>}]}
  ]} ].
details(P) -> #panel{class=["tab-content"], body=[
  #panel{id=Feed, class=["tab-pane"], body=[ feed(P#product.id, Fid, Feed), entry_form(P, Fid, Feed)]} || {Feed, Fid} <- P#product.feeds]}.


entry_form(P, Fid, Feed) ->
  TitleId = wf:temp_id(),
  EditorId = wf:temp_id(),
  SaveId = wf:temp_id(),
  User = wf:user(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
  MsId = wf:temp_id(),
  Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
  case User of undefined->[]; _-> [
    #h3{body="post "++ atom_to_list(Feed)},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200}, {139, 80}]},
        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, postback={post_entry, Fid, P#product.id, EditorId, TitleId, Feed, MsId}, source=[TitleId, EditorId], class=[btn, "btn-large", "btn-success"], body= <<"Post">>}]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias)}
      ]},
      #panel{class=[span3], body=[]}
    ]}
  ] end.

feed(ProdId, Fid, features)-> feed(ProdId, kvs_feed:entries({ProdId,Fid}, undefined, 10));
feed(ProdId, Fid, _TabId) -> feed(ProdId, kvs_feed:entries({ProdId, Fid}, undefined, 10)).

feed(ProdId, Entries)-> #panel{class=[feed], body=[
  [#product_entry{entry=E, prod_id=ProdId} || E <- Entries],
  #list{class=[pager], body=[
    #li{class=[previous], body=#link{body=[#i{class=["icon-chevron-left"]}, <<" Older">> ]}},
    #li{class=[next], body=#link{body=[#i{class=["icon-chevron-right"]}, <<" Newer">> ]}}
  ]} ]}.


aside()-> [
    #aside{class=[sidebar], body=[
      #panel{class=["sidebar-widget"], body=[
        #h2{class=["sidebar-header"], body= <<"">>},
        #p{body= <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.">>}
      ]},
      #panel{class=["sidebar-widget"], body=[
        #h2{class=["sidebar-header"], body= <<"More shooters">>},
        #list{class=[unstyled], body=[
          #li{body=[#h4{body=#link{body = <<"Quis nostrud exercitation">>}},
            #p{body=#small{body= <<"June 12, 2012">>}}]}
        ]}
      ]},
      #panel{class=["sidebar-widget"], body=[
        #h2{class=["sidebar-header"], body= <<"Recent posts">>},
        #list{class=[unstyled], body=[
          #li{body=[#h4{body=#link{body = <<"Quis nostrud exercitation">>}},
            #p{body=#small{body= <<"June 12, 2012">>}}]}
        ]}
      ]},
      #panel{class=["sidebar-widget"], body=[
        #h2{class=["sidebar-header"], body= <<"Popular posts">>},
        #list{class=[unstyled], body=[
          #li{body=[#h4{body=#link{body = <<"Lorem ipsum dolor sit">>}},
            #p{body=#small{body= <<"November 12, 2012">>}}]}
        ]}
      ]}
    ]}
].

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({post_entry, Fid, ProductId, EditorId, Ttid, Feed, MediasId}) ->
  User = wf:user(),
  Desc = wf:q(EditorId),
  Title = wf:q(Ttid),
  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(ProductId), Feed==reviews],

  Recipients = [{product, ProductId, {Feed, Fid}} | [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-Groups]] 
    ++ [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}],
  error_logger:info_msg("Recipients: ~p", [Recipients]),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  From = case wf:user() of undefined -> "anonymous"; User-> User#user.email end,
  EntryId = kvs:uuid(),
  [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
              [#entry{id={EntryId, FeedId},
                      entry_id=EntryId,
                      feed_id=FeedId,
                      created = now(),
                      to = {RoutingType, To},
                      from=From,
                      type=Feed,
                      media=Medias,
                      title=Title,
                      description=Desc,
                      shared=""}, Ttid, EditorId, MediasId, Feed]) || {RoutingType, To, {_, FeedId}} <- Recipients];

event({edit_entry, E=#entry{}, ProdId, Title, Desc, MsId}) ->
  Tid = wf:temp_id(), Did = wf:temp_id(),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  Dir = "static/"++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:update(Title, #textbox{id=Tid, value=wf:js_escape(wf:q(Title))}),
  wf:update(Desc, #htmlbox{id=Did, html=wf:js_escape(wf:q(Desc)), root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200} , {139, 80}]}),
  wf:insert_bottom(Desc, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, ProdId, Desc, Title, Tid, Did}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E, Title, Desc}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
  ]}),
  wf:insert_bottom(Desc, #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias) });

event({save_entry, #entry{}=E, ProductId, Dbox, Tbox, Tid, Did})->
  Title = wf:q(Tid),
  Description = wf:q(Did),

  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(ProductId), E#entry.type == reviews],
  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-Groups]],

  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [ msg:notify([kvs_feed, RouteType, To, entry, Fid, edit], [E#entry{title=Title, description=Description}, Tbox, Dbox]) || {RouteType, To, Fid} <- Recipients];

event({cancel_entry, E=#entry{}, Title, Desc}) ->
  wf:update(Title, wf:js_escape(E#entry.title)),
  wf:update(Desc, wf:js_escape(E#entry.description));

event({remove_entry, E=#entry{entry_id=Eid}, ProductId, Id}) ->
  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(ProductId), E#entry.type == reviews],
  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Gid, lists:keyfind(feed, 1, Feeds)} || #group{id=Gid, feeds=Feeds} <-Groups]],

  error_logger:info_msg("Recipients: ~", [Recipients]),

  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [E, (wf:user())#user.email, Id]) || {RouteType, To, Fid} <- Recipients];

event({read, entry, {Id,_}})-> wf:redirect("/review?id="++Id);
event({remove_media, M, Id}) ->
  Ms = case wf:session(medias) of undefined -> []; Mi -> Mi end,
  New = lists:filter(fun(E)-> error_logger:info_msg("take ~p compare with ~p and = ~p", [E,M, E/=M]),  E/=M end, Ms),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New));
event({check_more, Start, Info = #info_more{}}) ->
  read_entries(case Start of undefined -> undefined; S -> S#entry.entry_id end, Info),
  wf:update(Info#info_more.toolbar, []);
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(attach_media, Args, _Tag)->
  Props = n2o_json:decode(Args),
  Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
  Id = proplists:get_value(<<"id">>, Props#struct.lst),
  File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
  Type = proplists:get_value(<<"type">>, Props#struct.lst),
  Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)),
  Media = #media{id = Id,
    url = File,
    type = {attachment, Type},
    thumbnail_url = Thumb},
  Medias = case wf:session(medias) of undefined -> []; M -> M end,
  NewMedias = [Media | Medias],
  wf:session(medias, NewMedias),
  wf:update(Target, product_ui:preview_medias(Target, NewMedias));
api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([product, To, entry, _, add],
                 [#entry{} = Entry, Tid, Eid, MsId, TabId])->
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:insert_top(TabId, #product_entry{entry=Entry, prod_id=To}),
  wf:wire("Holder.run();");

process_delivery([product,_,entry,_,edit], [#entry{title=Title, description=Desc}, Tbox, Dbox]) ->
  wf:update(Tbox, wf:js_escape(Title)),
  wf:update(Dbox, wf:js_escape(Desc));

process_delivery([show_entry], [Entry, #info_more{} = Info]) ->
  error_logger:info_msg("show!"),
  wf:insert_bottom(Info#info_more.entries, #product_entry{entry=Entry, mode=line}),
  wf:wire("Holder.run();"),
  wf:update(Info#info_more.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Entry, Info}});
process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery([product, _, entry, _, delete], [_,_|Id]) -> wf:remove(Id);
process_delivery(_R, _M) -> skip.

read_entries(StartFrom, #info_more{fid=Fid}=I)->
  Feed = case StartFrom of
    undefined-> kvs:get(feed, Fid);
    S -> kvs:get(entry, {S, Fid})
  end,
  case Feed of
    {error, not_found} -> [];
    {ok, #feed{}=F} -> traverse_entries(F#feed.top, ?PAGE_SIZE, I);
    {ok, #entry{prev = E}} -> traverse_entries(E, ?PAGE_SIZE, I)
  end.

traverse_entries(undefined,_, #info_more{toolbar=BtnId}) -> self() ! {delivery, [somepath, no_more], [BtnId]}, [];
traverse_entries(_,0,_) -> [];
traverse_entries(Next, Count, I)->
  case kvs:get(entry, Next) of
    {error, not_found} -> [];
    {ok, R}->
      self() ! {delivery, [somepath, show_entry], [R, I]},
      [R | traverse_entries(R#entry.prev, Count-1, I)]
  end.
