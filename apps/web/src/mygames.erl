-module(mygames).
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").

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
      #upload{preview=true, root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200}, {139, 80}]}
    ]}
  ]}
 ] end.

games()->
  case wf:user() of undefined -> []; User ->[
    #h3{body= <<"My games">>, class=[blue]},
    #panel{id=myproducts, body= [#product_entry{entry=E} || E <- lists:reverse(kvs_feed:entries(User#user.direct, undefined, 10))]} ] end.

control_event("cats", _) ->
  SearchTerm = wf:q(term),
  Data = [ [list_to_binary(Id++"="++Name), list_to_binary(Name)] || #group{id=Id, name=Name} <- kvs:all(group), string:str(string:to_lower(Name), string:to_lower(SearchTerm)) > 0],
  element_textboxlist:process_autocomplete("cats", Data, SearchTerm);
control_event(_, _) -> ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(product_channel), [];
event({delivery, [_|Route], Msg}) -> error_logger:info_msg("Message delivered: ~p", [Msg]),process_delivery(Route, Msg);
event({save, TabId, MsId}) ->
  User = wf:user(),
  Title = wf:q(title),
  Descr = wf:q(brief),
  Cats = wf:q(cats),
  {Price, _Rest} = string:to_float(wf:q(price)),
  Currency = wf:q(currency),
  Categories = [1],
  TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
  Product = #product{
    creator= User#user.email,
    owner=User#user.email,
    title = list_to_binary(Title),
    cover = TitlePic,
    brief = list_to_binary(Descr),
    categories = Categories,
    price = Price,
    currency = Currency
  },
  case kvs_products:register(Product) of
    {ok, P} ->
      msg:notify([kvs_products, product, init], [P#product.id, P#product.feed, P#product.blog, P#product.features, P#product.specs, P#product.gallery, P#product.videos, P#product.bundles]),

      [kvs_group:join(P#product.name, G) || G <- string:tokens(Cats, ",")],

      Recipients = [{user, P#product.owner, User#user.direct}|[{{group, products}, Where, auto} || Where <- string:tokens(Cats, ",")]],
      Medias = case wf:session(medias) of undefined -> []; L -> L end,
      EntryId = "product_" ++ integer_to_list(P#product.id), %product:uuid() put to etc.

      error_logger:info_msg("Recipients: ~p", [Recipients]),
      error_logger:info_msg("Media: ~p", [Medias]),

      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add], [Fid, P#product.owner, Title, Descr, Medias, {TabId, product}, title, brief, MsId]) || {RoutingType, To, Fid} <- Recipients];

    E -> error_logger:info_msg("E: ~p", [E]), error
  end;
event({product_feed, Id})-> wf:redirect("/product?id="++integer_to_list(Id));
event({read_entry, {Id,_}})-> error_logger:info_msg("read ~p", [Id]),wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[account]Page event: ~p", [Event]), ok.

process_delivery([user, To, entry, EntryId, add],
                 [Fid, From, Title, Desc, Medias, {TabId, _L}=Type, Eid, Tid, MsId])->

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
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:insert_top(TabId, E);

process_delivery(_R, _M) -> skip.

