-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, 4).
-define(ROOT, code:priv_dir(web)++"/static").
-record(struct, {lst=[]}).

main() -> #dtl{file="dev", bindings=[{title,<<"product">>},{body, body()}]}.

body() ->
  Id = wf:qs(<<"id">>),
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, list_to_integer(binary_to_list(Id))) of
      {ok, P} -> [
        #panel{class=["row-fluid", "page-header"], body=[
          #h5{class=[span9],body=[<<"Categories:">>,
            #small{body=[
              #link{body= <<" Action ">>}, <<"|">>,
              #link{body= <<" Shooter (FPS) ">>},<<"|">>,
              #link{body= <<" Weekly Deals ">>}, <<"|">>,
              #link{body= <<" Collections ">>}
            ]}
          ]},
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
details(P) ->
  [
  #panel{class=["tab-content"], body=[
    #panel{id=features, class=["tab-pane", active], body=[
      feed(P#product.features, features),
      entry_form(P, P#product.features, "feature", features)
    ]},
    #panel{id=specs, class=["tab-pane"], body=[
      feed(P#product.specs, specs),
      entry_form(P, P#product.specs, "specifications", specs)
    ]},
    #panel{id=gallery, class=["tab-pane"], body=[
      feed(P#product.gallery, gallery),
      entry_form(P, P#product.gallery, "picture", gallery)
    ]},
    #panel{id=videos, class=["tab-pane"], body=[
      feed(P#product.videos, videos),
      entry_form(P, P#product.videos, "video", videos)
    ]},
    #panel{id=reviews, class=["tab-pane"], body=[
      feed(P#product.feed, reviews),
      entry_form(P, P#product.feed, "review", reviews)
    ]},
    #panel{id=news, class=["tab-pane"], body=[
      feed(P#product.blog, news),
      entry_form(P, P#product.blog, "news", news)
    ]},
    #panel{id=bundles, class=["tab-pane"], body=[
      feed(P#product.bundles, bundles),
      entry_form(P, P#product.bundles, "stuff@!!!1111", bundles)
    ]}
  ]}
].

entry_form(P, Fid, Title, TabId) ->
  TitleId = wf:temp_id(),
  EditorId = wf:temp_id(),
  SaveId = wf:temp_id(),
  User = wf:user(),
  LayoutId = wf:temp_id(),
  case User of undefined->[]; _-> [
    #h3{body="post "++Title},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
        #htmlbox{id=EditorId, class=[span12]}
      ]},
      #panel{class=[span3], body=[
        #upload{root=?ROOT++"/"++User#user.email, post_write=attach_media, img_tool=gm, preview=true},
        #h3{body= <<"Layout:">>},
        #select{id=LayoutId, style="width:100%", body=[
          #option{label= <<"default">>, value= <<"default">>},
          #option{label= <<"jumbotron">>, value= <<"jumbotron">>},
          #option{label= <<"figure">>, value= <<"figure">>}
        ]}
      ]}
    ]},
    #panel{class=["btn-toolbar"], body=[#link{id=SaveId, postback={post_entry, Fid, P#product.id, EditorId, TitleId, TabId, LayoutId}, source=[TitleId, EditorId, LayoutId], class=[btn, "btn-large", "btn-success"], body= <<"Post">>}]} 
  ] end.

feed(Fid, features)-> feed(lists:reverse(kvs_feed:entries(Fid, undefined, 10)));
feed(Fid, _TabId) -> feed(kvs_feed:entries(Fid, undefined, 10)).

feed(Entries)-> #panel{class=[feed], body=[
  [#product_entry{entry=E} || E <- Entries],
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



event(init) -> wf:reg(product_channel), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({post_entry, Fid, Id, Eid, Ttid, TabId, Lid}) ->
  Desc = wf:q(Eid),
  Title = wf:q(Ttid),
  Layout = wf:q(Lid),
  Recipients = [{Id, product}],
  Type = {TabId, Layout},
  error_logger:info_msg("Entry ~p ~p", [Title, Type]),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  User = wf:user(),
  From = User#user.email,

  [msg:notify([kvs_feed, feed, RoutingType, To, entry, uuid(), add], [Fid, From, Title, Desc, Medias, Type]) || {To, RoutingType} <- Recipients],

  wf:session(medias, []);
event({edit_entry, E=#entry{}, Title, Desc}) ->
  Tid = wf:temp_id(), Did = wf:temp_id(),
  wf:update(Title, #textbox{id=Tid, value=wf:q(Title)}),
  wf:update(Desc, #htmlbox{id=Did, html=wf:js_escape(wf:q(Desc))}),
  wf:insert_bottom(Desc, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, Desc, Title, Tid, Did}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E, Title, Desc}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
  ]}),
  ok;
event({save_entry, #entry{id=Eid}, Dbox, Tbox, Tid, Did})->
  Title = wf:q(Tid), Description = wf:q(Did),
  msg:notify([kvs_feed, feed, product, (wf:user())#user.email, entry, Eid, edit], [Tbox, Dbox, Title, Description]);
event({cancel_entry, E=#entry{}, Title, Desc})->
  wf:update(Title, wf:js_escape(E#entry.title)),
  wf:update(Desc, wf:js_escape(E#entry.description));
event({remove_entry, E=#entry{}, Id})->
  msg:notify([kvs_feed, product, E#entry.from, entry, E#entry.id, delete], [(wf:user())#user.email]),
  wf:remove(Id);
event({read_entry, {Id,_}})->
  wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(attach_media, Args, _)->
  Props = n2o_json:decode(Args),
  Id = proplists:get_value(<<"id">>, Props#struct.lst),
  File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)) -- ?ROOT,
  Type = proplists:get_value(<<"type">>, Props#struct.lst),
  Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)) -- ?ROOT,
  Media = #media{id = Id,
    width = 200,
    height = 200,
    url = "/static"++File,
    type = {attachment, Type},
    thumbnail_url = "/static"++Thumb},
  error_logger:info_msg("~p attached", [Media]),
  Medias = case wf:session(medias) of undefined -> []; M -> M end,
  NewMedias = [Media | Medias],
  wf:session(medias, NewMedias);
api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([feed, _, To, entry, EntryId, add],
                 [Fid, From, Title, Desc, Medias, {TabId, _L}=Type])->
  Entry = #entry{id = {EntryId, Fid},
                 entry_id = EntryId,
                 type=Type,
                 created = now(),
                 from = From,
                 to = To,
                 title = Title,
                 description = Desc,
                 media = Medias,
                 feed_id = Fid},

  E = #product_entry{entry=Entry},
  wf:insert_top(TabId, E);

process_delivery([feed, _, _Who, entry, _, edit],
                 [Tbox, Dbox, Title, Desc]) ->
  wf:update(Tbox, Title),
  wf:update(Dbox, Desc);

process_delivery(_R, _M) -> skip.

uuid() ->
  R1 = random:uniform(round(math:pow(2, 48))) - 1,
  R2 = random:uniform(round(math:pow(2, 12))) - 1,
  R3 = random:uniform(round(math:pow(2, 32))) - 1,
  R4 = random:uniform(round(math:pow(2, 30))) - 1,
  R5 = erlang:phash({node(), now()}, round(math:pow(2, 32))),

  UUIDBin = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>,
  <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = UUIDBin,

  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b-~8.16.0b",
                              [TL, TM, THV, CSR, CSL, N, R5])).
