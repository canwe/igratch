-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, 4).
-define(ROOT, code:priv_dir(web)).
-record(struct, {lst=[]}).

main() -> #dtl{file="prod", bindings=[{title,<<"product">>},{body, body()}]}.

body() ->
  Id = wf:qs(<<"id">>),
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, list_to_integer(binary_to_list(Id))) of
      {ok, P} -> [
        #panel{class=["row-fluid", "page-header"], body=[
          #h4{class=[span9], style="line-height:30px;", body= [#link{url= <<"/reviews">>, body= <<"Categories ">>, style="color:#999"}, #small{body=[[
            begin
              Name = case kvs:get(group,I) of {ok, G}-> G#group.name; _ -> "noname" end,
              [<<" | ">>, #link{url="/reviews?id="++I, body=[#span{class=["icon-asterisk"]},Name]}]
            end
          ] || #group_subscription{where=I} <- kvs_group:participate("product"++integer_to_list(P#product.id))]} ]},

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
      feed(P#product.id, P#product.features, features),
      entry_form(P, P#product.features, "feature", features)
    ]},
    #panel{id=specs, class=["tab-pane"], body=[
      feed(P#product.id, P#product.specs, specs),
      entry_form(P, P#product.specs, "specifications", specs)
    ]},
    #panel{id=gallery, class=["tab-pane"], body=[
      feed(P#product.id, P#product.gallery, gallery),
      entry_form(P, P#product.gallery, "picture", gallery)
    ]},
    #panel{id=videos, class=["tab-pane"], body=[
      feed(P#product.id, P#product.videos, videos),
      entry_form(P, P#product.videos, "video", videos)
    ]},
    #panel{id=reviews, class=["tab-pane"], body=[
      feed(P#product.id, P#product.feed, reviews),
      entry_form(P, P#product.feed, "review", reviews)
    ]},
    #panel{id=news, class=["tab-pane"], body=[
      feed(P#product.id, P#product.blog, news),
      entry_form(P, P#product.blog, "news", news)
    ]},
    #panel{id=bundles, class=["tab-pane"], body=[
      feed(P#product.id, P#product.bundles, bundles),
      entry_form(P, P#product.bundles, "stuff@!!!1111", bundles)
    ]}
  ]}
].

entry_form(P, Fid, Title, TabId) ->
  TitleId = wf:temp_id(),
  EditorId = wf:temp_id(),
  SaveId = wf:temp_id(),
  User = wf:user(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
  MsId = wf:temp_id(),
  case User of undefined->[]; _-> [
    #h3{body="post "++Title},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir="static/"++User#user.email, post_write=attach_media, img_tool=gm, post_target=MsId},
        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, postback={post_entry, Fid, P#product.id, EditorId, TitleId, TabId, MsId}, source=[TitleId, EditorId], class=[btn, "btn-large", "btn-success"], body= <<"Post">>}]},
        #panel{id=MsId, body=preview_medias(MsId, Medias)}
      ]},
      #panel{class=[span3], body=[]}
    ]}
  ] end.

preview_medias(Id, Medias)->
  L = length(Medias),
  if L > 0 ->
    #carousel{indicators=false, style="border:1px solid #eee;", items=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[close], style="position:absolute; right:10px;top:5px;",  body= <<"&times;">>, postback={remove_media, M, Id}},
          #link{class=[thumbnail], body=[
            #image{image= case M#media.thumbnail_url of undefined -> <<"holder.js/100%x120">>;Th -> Th end}
          ]}
        ]}|| M <- lists:sublist(Medias, I, 4)
      ]}|| I <- lists:seq(1, L, 4) ],
      caption=#panel{body= <<"Entry will be posted with this medias.">>}};
    true-> [] end.

feed(ProdId, Fid, features)-> feed(ProdId, lists:reverse(kvs_feed:entries(Fid, undefined, 10)));
feed(ProdId, Fid, _TabId) -> feed(ProdId, kvs_feed:entries(Fid, undefined, 10)).

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



event(init) -> wf:reg(product_channel), [];
event(<<"PING">>) -> [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({post_entry, Fid, ProductId, Eid, Ttid, TabId, MsId}) ->
  Desc = wf:q(Eid),
  Title = wf:q(Ttid),
  Recipients = [{ProductId, product}|[{Where, group} || #group_subscription{where=Where, type=member} <- kvs_group:participate("product"++integer_to_list(ProductId)), TabId==reviews]],
  EntryType = {TabId, default},
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  User = wf:user(),
  From = User#user.email,
  EntryId =uuid(),
  [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add], [Fid, From, Title, Desc, Medias, EntryType]) || {To, RoutingType} <- Recipients],

  wf:session(medias, []),
  wf:update(MsId, []);

event({edit_entry, E=#entry{}, ProdId, Title, Desc}) ->
  Tid = wf:temp_id(), Did = wf:temp_id(),
  wf:update(Title, #textbox{id=Tid, value=wf:js_escape(wf:q(Title))}),
  wf:update(Desc, #htmlbox{id=Did, html=wf:js_escape(wf:q(Desc))}),
  wf:insert_bottom(Desc, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, ProdId, Desc, Title, Tid, Did}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E, Title, Desc}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
  ]});

event({save_entry, #entry{id=Eid, type={Type, _Layout}}, ProductId, Dbox, Tbox, Tid, Did})->
  Title = wf:q(Tid), Description = wf:q(Did),
  Recipients = [{ProductId, product}|[{Where, group} || #group_subscription{where=Where, type=member} <- kvs_group:participate("product"++integer_to_list(ProductId)), Type==reviews]],

  [ msg:notify([kvs_feed, RouteType, To, entry, Eid, edit], [Tbox, Dbox, Title, Description]) || {To, RouteType} <- Recipients];

event({cancel_entry, E=#entry{}, Title, Desc})->
  wf:update(Title, wf:js_escape(E#entry.title)),
  wf:update(Desc, wf:js_escape(E#entry.description));
event({remove_entry, E=#entry{type={Type, _Layout}}, ProductId, Id})->
  Recipients = [{ProductId, product}|[{Where, group} || #group_subscription{where=Where, type=member} <- kvs_group:participate("product"++integer_to_list(ProductId)), Type==reviews]],

  [msg:notify([kvs_feed, RouteType, To, entry, E#entry.id, delete], [(wf:user())#user.email, Id]) || {To, RouteType} <- Recipients];

event({read_entry, {Id,_}})-> wf:redirect("/review?id="++Id);
event({remove_media, M, Id}) ->
  Ms = case wf:session(medias) of undefined -> []; Mi -> Mi end,
  New = lists:filter(fun(E)-> error_logger:info_msg("take ~p compare with ~p and = ~p", [E,M, E/=M]),  E/=M end, Ms),
  wf:session(medias, New),
  wf:update(Id, preview_medias(Id, New));
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(attach_media, Args, Tag)->
  error_logger:info_msg("Tag~p", [Tag]),
  Props = n2o_json:decode(Args),
  Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
  Id = proplists:get_value(<<"id">>, Props#struct.lst),
  File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
  Type = proplists:get_value(<<"type">>, Props#struct.lst),
  Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)),
  Media = #media{id = Id,
    width = 200,
    height = 200,
    url = File,
    type = {attachment, Type},
    thumbnail_url = Thumb},
  Medias = case wf:session(medias) of undefined -> []; M -> M end,
  NewMedias = [Media | Medias],
  wf:session(medias, NewMedias),
  wf:update(Target, preview_medias(Target, NewMedias));
api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([product, To, entry, EntryId, add],
                 [Fid, From, Title, Desc, Medias, {TabId, _L}=Type])->
  Entry = #entry{id = {EntryId, Fid},
                 entry_id = EntryId,
                 type=Type,
                 created = now(),
                 from = From,
                 to = To,
                 title = wf:js_escape(Title),
                 description = wf:js_escape(Desc),
                 media = Medias,
                 feed_id = Fid},

  E = #product_entry{entry=Entry, prod_id=To},
  wf:insert_top(TabId, E);
process_delivery([product, _UsrId, entry, _Eid, edit],
                 [Tbox, Dbox, Title, Description])->
  wf:update(Tbox, wf:js_escape(Title)),
  wf:update(Dbox, wf:js_escape(Description));

process_delivery([product, _, entry, _, delete], [_, Id]) ->
  wf:remove(Id);

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
