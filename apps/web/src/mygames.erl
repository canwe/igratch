-module(mygames).
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").

-define(GRP_CACHE, kvs:all(group)).

main()-> #dtl{file="prod", bindings=[{title,<<"my games">>},{body, body()}]}.

body()->
  index:header() ++[
  #section{id=content, body=
    #panel{class=[container, account], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(mygames)},
        #panel{class=[span9], body=[
          dashboard:section(input(), "icon-user"),
          dashboard:section(games(), "icon-user")
        ]} ]} } }
  ]++index:footer().


input() ->
  User = wf:user(),
  Curs = [{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}],
  Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
  MsId = wf:temp_id(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,

  case User of undefined ->[]; _-> [
    #h3{class=[blue], body= [<<"Add new game">>, #span{class=["pull-right", span3], style="color: #555555;",  body= <<"cover">>}]},
    #panel{class=["row-fluid"], body=[
    #panel{class=[span9], body=[
      #textboxlist{id=cats, placeholder= <<"Categoried/Tags">>},
      #textbox{id=title, class=[span12], placeholder="Game title"},
      #textarea{id=brief, class=[span12], rows="5", placeholder="Brief description"},

      #panel{class=["input-append"], body=[
        #textbox{id = price, class=[span2]},
        #select{id=currency, class=[selectpicker], body=[#option{label= L, body = V} || {L,V} <- Curs]}
      ]},
      #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias)},

      #panel{class=["btn-toolbar"],body=[
      #link{id=save_prod, class=[btn, "btn-large"], body=[#i{class=["icon-file-alt", "icon-large"]}, <<" Create">>],
        postback={save, myproducts, MsId}, source=[title, brief, price, currency, cats]}]}
    ]},
    #panel{class=[span3], body=[
      #upload{preview=true, root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200}, {139, 80}, {1170, 380}]}
    ]}
  ]}
 ] end.

games()->
  case wf:user() of undefined -> []; User ->
    {_, Fid} = Feed = lists:keyfind(products,1,User#user.feeds),
    Entries = kvs_feed:entries(Feed, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    EsId = wf:temp_id(),
    BtnId = wf:temp_id(),
    Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId},
    NoMore = length(Entries) < ?PAGE_SIZE,
    [#h3{body= <<"My games">>, class=[blue]},
    #panel{id=myproducts, body=[
      #panel{id=EsId, body=[#product_entry{entry=E, mode=line, controls=[[
%        #link{body= [#i{class=["icon-edit", "icon-large"]},<<"edit">>], postback={edit_product, E#entry.entry_id}},
        #link{body=[#i{class=["icon-remove", "icon-large"]}, <<"remove">>], postback={remove_product, E}}
      ]]} || E <- Entries]},
      #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
        if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback = {check_more, Last, Info}} end ]} ]} ] end.

control_event("cats", _) ->
  SearchTerm = wf:q(term),
  Data = [ [list_to_binary(Id++"="++Name), list_to_binary(Name)] || #group{id=Id, name=Name} <- ?GRP_CACHE, string:str(string:to_lower(Name), string:to_lower(SearchTerm)) > 0],
  element_textboxlist:process_autocomplete("cats", Data, SearchTerm);
control_event(_, _) -> ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({save, TabId, MediasId}) ->
  User = wf:user(),
  Title = wf:q(title),
  Descr = wf:q(brief),
  Cats = wf:q(cats),
  PriceStr = wf:q(price),
  PriceStr2 = case string:to_float(PriceStr) of {error, no_float} -> PriceStr; {F, _} -> float_to_list(F, [{decimals, 2}]) end,
  {P1, Rest} = case string:to_integer(PriceStr2) of {error, no_integer} -> {0, "0"}; {Pa, [_|Ra]} -> {Pa, Ra}; {Pa, Ra} -> {Pa, Ra} end,
  P2 = case string:to_integer(Rest) of {error,no_integer}-> 0; {Re,_} -> Re  end,
  Currency = wf:q(currency),
  TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
  Product = #product{
    creator = User#user.email,
    owner = User#user.email,
    title = list_to_binary(Title),
    brief = list_to_binary(Descr),
    cover = TitlePic,
    price = P2+P1*100,
    currency = Currency,
    feeds = ?PRD_CHUNK
  },
  case kvs_products:register(Product) of
    {ok, P} ->
      Groups = [case kvs:get(group,S) of {error,_}->[]; {ok,G} ->G end || S<-string:tokens(Cats, ",")],

      Recipients = [{user, P#product.owner, lists:keyfind(products, 1, User#user.feeds)} |
        [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Groups]],

      error_logger:info_msg("Message recipients: ~p", [Recipients]),
      Medias = case wf:session(medias) of undefined -> []; M -> M end,

      [kvs_group:join(P#product.id, Id) || #group{id=Id} <- Groups],

      [msg:notify([kvs_feed, RoutingType, To, entry, P#product.id, add],
                  [#entry{id={P#product.id, Fid},
                          feed_id=Fid,
                          entry_id=P#product.id,
                          created = now(),
                          to = {RoutingType, To},
                          from=P#product.owner,
                          type= product,
                          media=Medias,
                          title=Title,
                          description=Descr,
                          shared=""}, title, brief, MediasId, TabId]) || {RoutingType, To, {_, Fid}} <- Recipients],

      msg:notify([kvs_products, product, init], [P#product.id, P#product.feeds]);
    _ -> error
  end;
event({read, product, {Id,_}})-> wf:redirect("/product?id="++Id);
event({edit_product, Id}) ->
  error_logger:info_msg("Edit ~p", [Id]);
event({remove_product, E}) ->
  User = wf:user(),
  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(E#entry.entry_id)],

  Recipients = [{user, User#user.email, lists:keyfind(products, 1, User#user.feeds)} |
        [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Groups]],
  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [E, User#user.email]) || {RouteType, To, Fid} <- Recipients],
  kvs_products:delete(E#entry.entry_id);
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery([user, _, entry, _, add],
                 [#entry{description=D, title=T} = Entry, Eid, Tid, MsId, TabId])->
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:insert_top(TabId, #product_entry{entry=Entry#entry{description=wf:js_escape(D), title=wf:js_escape(T)}, mode=line, controls=[[
%      #link{body= [#i{class=["icon-edit", "icon-large"]},<<"edit">>], postback={edit_product, Entry}},
      #link{body=[#i{class=["icon-remove", "icon-large"]}, <<"remove">>], postback={remove_product, Entry}}
    ]]}),
  wf:wire("Holder.run();");
process_delivery(R,M) -> product:process_delivery([no_more], M);
