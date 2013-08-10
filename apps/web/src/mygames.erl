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
      #upload{preview=true, root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200}, {139, 80}]}
    ]}
  ]}
 ] end.

games()->
  case wf:user() of undefined -> []; User ->
    {_, Fid} = Feed = lists:keyfind(products,1,User#user.feeds),
    error_logger:info_msg("More id feed ~p", [Feed]),
    Entries = kvs_feed:entries(Feed, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    EsId = wf:temp_id(),
    BtnId = wf:temp_id(),
    error_logger:info_msg("Pid: ~p", [self()]),
    Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId, module=pid_to_list(self())},
    NoMore = length(Entries) < ?PAGE_SIZE,
    [#h3{body= <<"My games">>, class=[blue]},
    #panel{id=myproducts, body=[
      #panel{id=EsId, body=[#product_entry{entry=E, mode=line} || E <- Entries]},
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
  {Price, _Rest} = string:to_float(wf:q(price)),
  Currency = wf:q(currency),
  TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
  Product = #product{
    creator = User#user.email,
    owner = User#user.email,
    title = list_to_binary(Title),
    brief = list_to_binary(Descr),
    cover = TitlePic,
    price = Price,
    currency = Currency,
    feeds = ?PRD_CHUNK
  },
  case kvs_products:register(Product) of
    {ok, P} ->
      Groups = [case kvs:get(group,S) of {error,_}->[]; {ok,G} ->G end || S<-string:tokens(Cats, ",")],

      Recipients = [{user, P#product.owner, lists:keyfind(products, 1, User#user.feeds)} |
        [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Groups]],

      Medias = case wf:session(medias) of undefined -> []; M -> M end,

      [kvs_group:join(P#product.id, Id) || #group{id=Id} <- Groups],

      [msg:notify([kvs_feed, RoutingType, To, entry, P#product.id, add],
                  [#entry{id={P#product.id, Fid},
                          feed_id=Fid,
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
event({read, product, {Id,_}})-> error_logger:info_msg("read ~p", [Id]),wf:redirect("/product?id="++Id);
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery([user, _, entry, _, add],
                 [#entry{} = Entry, Eid, Tid, MsId, TabId])->
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:insert_top(TabId, #product_entry{entry=Entry, mode=line}),
  wf:wire("Holder.run();");

process_delivery([show_entry], M) -> product:process_delivery([show_entry], M);
process_delivery([no_more], M) -> product:process_delivery([no_more], M);
process_delivery(_,_) -> skip.
