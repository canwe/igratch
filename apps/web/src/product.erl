-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"product">>},{body, body()}]}.

body() ->
  Id = case wf:qs(<<"id">>) of undefined -> <<"no">>; I-> I end,
  case wf:qs(<<"tab">>) of undefined -> wf:wire("$(document).ready(function(){ $('a[href=\"#reviews\"]').tab('show'); });"); Tab -> wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').tab('show');});",[Tab])) end,

  wf:wire(#api{name=tabshow}),
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){ console.log(e.target); tabshow($(e.target).attr('href'));});"),

  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, binary_to_list(Id)) of
      {ok, P} ->
        wf:session(product, P),
        [
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
          #panel{class=[span9], body= #panel{class=["tab-content", dashboard], body=[
            #panel{id=Feed, class=["tab-pane"], body=[]} || {Feed, _} <- P#product.feeds]}},
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

%entry_form(P, Fid, Feed) ->
%  TitleId = wf:temp_id(),
%  EditorId = wf:temp_id(),
%  SaveId = wf:temp_id(),
%  User = wf:user(),
%  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
%  MsId = wf:temp_id(),
%  Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
%  case User of undefined->[]; _-> [
%    #h3{body="post "++ atom_to_list(Feed)},
%    #panel{class=["row-fluid"], body=[
%      #panel{class=[span9], body=[
%        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
%        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE},
%        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, postback=
%          {post_entry, Fid, P#product.id, EditorId, TitleId, Feed, MsId},

%          {post_entry, RecipientsId, EditorId, TitleId, MsId}

%        source=[TitleId, EditorId], class=[btn, "btn-large", "btn-success"], body= <<"Post">>}]},
%        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias, product)}
%      ]},
%      #panel{class=[span3], body=[]}
%    ]}
%  ] end.

feed(Tab)->
    User = wf:user(),
    P = wf:session(product),
    {Tab,Fid} = lists:keyfind(Tab,1,P#product.feeds),
    Entries = kvs:entries({Tab,Fid}, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    BtnId = wf:temp_id(),
    Info = #info_more{fid=Fid, entries=Tab, toolbar=BtnId},
    NoMore = length(Entries) < ?PAGE_SIZE,

    Subscriptions =  kvs_group:participate(P#product.id),
    Groups = lists:flatten([case kvs:get(group, Where) of {error, _}-> []; {ok, G} -> "group"++G#group.id++"="++G#group.name end || #group_subscription{where=Where} <- Subscriptions]),
    Recipients = string:join([Groups, 
        "product"++wf:to_list(P#product.id)++"="++wf:to_list(P#product.title), 
        "user"++User#user.email++"="++wf:to_list(User#user.display_name)], ","),
    error_logger:info_msg("Recipients: ~p", [Recipients]),
    [
%    entry_form(P, Fid, Tab),
    #input{title= "Write "++atom_to_list(Tab), placeholder_rcp= <<"">>, placeholder_ttl= <<"Title">>, collapsed=true, feed=Tab, recipients=[Recipients]},
    dashboard:section([
        #h3{class=[blue], body= wf:to_list(Tab) },
        #panel{id=Tab, class=[feed], body=[#product_entry{entry=E, prod_id=P#product.id} || E <- Entries]},
        #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
            if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Last, Info}} end ]}], "icon-circle")].

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

controls(#entry{type=Type} =  E) -> [
  #link{body=[case Type of product -> <<"view ">>; _-> <<"read more ">> end, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, Type, E#entry.id}} ].


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({post_entry, Fid, ProductId, EditorId, Ttid, Feed, MediasId}) ->
  User = wf:user(),
  Desc = wf:q(EditorId),
  Title = wf:q(Ttid),
  Subscriptions =  kvs_group:participate(ProductId),
  Groups = lists:flatten([case kvs:get(group, Where) of {error, _}-> []; {ok, G} -> G end|| #group_subscription{where=Where} <- Subscriptions, Feed==reviews]),

  Recipients = [{product, ProductId, {Feed, Fid}} | [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-Groups]] 
    ++ if Feed==reviews -> [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}];true-> [] end,

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

event({edit_entry, E=#entry{title=Title, description=Desc}, ProdId, MsId}) ->
  Tid = ?ID_TITLE(E#entry.entry_id), Did = ?ID_DESC(E#entry.entry_id), Toid = ?ID_TOOL(E#entry.entry_id),
  Dir = "static/"++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:replace(Tid, #textbox{id=Tid, value=wf:js_escape(Title)}),
  wf:replace(Did, #panel{body=[#htmlbox{id=Did, html=wf:js_escape(Desc), root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE}]}),
  wf:update(Toid, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, ProdId}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E#entry{title=wf:js_escape(Title), description=wf:js_escape(Desc)}}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
  ]});
event({save_entry, #entry{}=E, ProductId})->
  Title = wf:q(?ID_TITLE(E#entry.entry_id)),
  Description = wf:q(?ID_DESC(E#entry.entry_id)),
  User = wf:user(),

  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(ProductId), E#entry.type==reviews],

  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-Groups]] 
    ++ if E#entry.type==reviews -> [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}];true-> [] end,

  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [ msg:notify([kvs_feed, RouteType, To, entry, Fid, edit], E#entry{title=Title, description=Description}) || {RouteType, To, Fid} <- Recipients];

event({cancel_entry, E=#entry{title=Title, description=Desc}}) ->
  Tid = ?ID_TITLE(E#entry.entry_id), Did = ?ID_DESC(E#entry.entry_id),
  wf:replace(Tid, #span{id=Tid, body=Title}),
  wf:replace(Did, #panel{id=Did, body=Desc, data_fields=[{<<"data-html">>, true}]}),
  wf:update(?ID_TOOL(E#entry.entry_id), []);

event({remove_entry, E=#entry{}, ProductId}) ->
  User = wf:user(),
  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where} <- kvs_group:participate(ProductId), E#entry.type == reviews],
  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Gid, lists:keyfind(feed, 1, Feeds)} || #group{id=Gid, feeds=Feeds} <-Groups]]
  ++ [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}],

  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [E, (wf:user())#user.email]) || {RouteType, To, Fid} <- Recipients];

event({read, entry, {Id,_}})-> wf:redirect("/review?id="++Id);
event({remove_media, M, Id}) ->
  Ms = case wf:session(medias) of undefined -> []; Mi -> Mi end,
  New = lists:filter(fun(E)-> E/=M end, Ms),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New));
event({check_more, Start, Info = #info_more{}}) ->
  read_entries(case Start of undefined -> undefined; S -> S#entry.entry_id end, Info),
  wf:update(Info#info_more.toolbar, []);
event({checkout, Pid}) -> wf:redirect("/checkout?product_id="++Pid);
event({add_cart, #product{}=P}) ->
  error_logger:info_msg("Add to cart: ~p", [P]),
  case wf:session(shoing_cart) of 
    undefined -> wf:session(shopping_cart, [P]);
    L -> wf:session([P|L])
  end,
  wf:redirect("/shopping_cart");
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    error_logger:info_msg("Show tab ~p", [Id]),
    wf:update(list_to_atom(Id), feed(list_to_atom(Id)));
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
  wf:update(Target, product_ui:preview_medias(Target, NewMedias, product));
api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([product, To, entry, _, add],
                 [#entry{description=D, title=T} = Entry, Tid, Eid, MsId, TabId]) when TabId /= myreviews->
    error_logger:info_msg("[product] product, entry, add"),
%  wf:session(medias, []),
%  wf:update(MsId, []),
%  wf:wire(wf:f("$('#~s').val('');", [Tid])),
%  wf:wire(wf:f("$('#~s').html('');", [Eid])),
%  wf:insert_top(TabId, #product_entry{entry=Entry#entry{description=wf:js_escape(D), title=wf:js_escape(T)}, prod_id=To}),
  wf:wire("Holder.run();");

process_delivery([_,_,entry,_,edit], #entry{entry_id=Id, title=Title, description=Desc, media=Media}) ->
  wf:session(medias, []),
  Tid = ?ID_TITLE(Id), Did = ?ID_DESC(Id),
  wf:replace(Tid, #span{id =Tid, body=wf:js_escape(Title)}),
  wf:replace(Did, #panel{id=Did, body=wf:js_escape(Desc), data_fields=[{<<"data-html">>, true}]}),
  wf:update(?ID_MEDIA(Id), #entry_media{media=Media, mode=reviews}),
%  wf:update(?ID_TOOL(Id), []),
  wf:wire("Holder.run();");

process_delivery([show_entry], [Entry, #info_more{} = Info]) ->
  wf:insert_bottom(Info#info_more.entries, #product_entry{entry=Entry, mode=line, controls=controls(Entry)}),
  wf:wire("Holder.run();"),
  wf:update(Info#info_more.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Entry, Info}});
process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery([_,_,entry,_,delete], [E,_]) -> wf:remove(E#entry.entry_id);
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
