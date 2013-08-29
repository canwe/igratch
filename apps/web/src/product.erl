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
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){tabshow($(e.target).attr('href'));});"),
    Tab = case wf:qs(<<"tab">>) of undefined -> <<"reviews">>; T ->  T end,
    wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').tab('show');});",[Tab])),


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
    #li{body=[#link{url= <<"#features">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Features">>}]},
    #li{body=[#link{url= <<"#specs">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Specification">>}]},
    #li{body=[#link{url= <<"#gallery">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Gallery">>}]},
    #li{body=[#link{url= <<"#videos">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Videos">>}]},
    #li{body=[#link{url= <<"#reviews">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Reviews">>}]},
    #li{body=[#link{url= <<"#news">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"News">>}]},
    #li{body=[#link{url= <<"#bundles">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Bundles">>}]}
  ]} ].

feed(Tab)->
    User = wf:user(),
    P = wf:session(product),

    Subscriptions =  kvs_group:participate(P#product.id),
    Groups = lists:flatten([case kvs:get(group, Where) of {error, _}-> []; {ok, G} -> "group"++G#group.id++"="++G#group.name end || #group_subscription{where=Where} <- Subscriptions]),
    Recipients = string:join([Groups,
        "product"++wf:to_list(P#product.id)++"="++wf:to_list(P#product.title), 
        "user"++User#user.email++"="++wf:to_list(User#user.display_name)], ","),
    error_logger:info_msg("Recipients: ~p", [Recipients]),
    [
    #input{expand_btn= "Write "++atom_to_list(Tab),  placeholder_ttl= <<"Title">>, class="alt", icon="", collapsed=true, role=product, type=Tab, recipients=[Recipients]},
    #feed_view{owner=P, feed=Tab, title= wf:to_list(Tab), icon="icon-circle", mode=blog} ].

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
    wf:update(list_to_atom(Id), feed(list_to_atom(Id))),
    wf:wire("Holder.run();");
api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([_,_,entry,_,edit], #entry{entry_id=Id, title=Title, description=Desc, media=Media}=E) ->
  wf:session(medias, []),
  Tid = ?ID_TITLE(Id), Did = ?ID_DESC(Id),
  wf:replace(Tid, #span{id =Tid, body=wf:js_escape(Title)}),
  wf:replace(Did, #panel{id=Did, body=wf:js_escape(Desc), data_fields=[{<<"data-html">>, true}]}),
  wf:update(?ID_MEDIA(Id), #entry_media{media=Media, mode=reviews}),
  wf:update(?ID_TOOL(Id), feed:controls(E)),
  wf:wire("Holder.run();");

process_delivery([_,_,entry,_,delete], [E,_]) -> wf:remove(E#entry.entry_id);
process_delivery(R,M) -> stop.
