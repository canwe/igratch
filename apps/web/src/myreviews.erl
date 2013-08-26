-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"my reviews">>},{body, body()}]}.

body()->
  index:header() ++[
  #section{id=content, body=
    #panel{class=[container, account], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidenav(wf:user(), wf:user(), myreviews, [])},
        #panel{class=[span9], body=[
          dashboard:section(input(), "icon-edit"),
          dashboard:section([#h3{class=[blue], body= <<"My reviews">>}, reviews(wf:user())], "icon-list")
        ]} ]} } }
  ]++index:footer().

input()->
  User = wf:user(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
  MsId = wf:temp_id(),
  TitleId = wf:temp_id(),
  EditorId = wf:temp_id(),
  SaveId = wf:temp_id(),

  Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
  case User of undefined -> []; _->[
    #h3{class=[blue], body= <<"Submit review">>},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textboxlist{id=products, placeholder= <<"Games/Tags">>},
        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE },
        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, class=[btn, "btn-large", "btn-success"], body= <<" Post">>,
          postback={post_entry, EditorId, TitleId, MsId}, source=[TitleId, EditorId, products] }]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias, myreviews)}
      ]},
      #panel{class=[span2], body=[]}
  ]} ] end.


reviews(undefined)->[];
reviews(User)->
    {_, Fid} = Feed = lists:keyfind(feed,1,User#user.feeds),
    Entries = kvs:entries(Feed, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    EsId = wf:temp_id(),
    BtnId = wf:temp_id(),
    Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId},
    NoMore = length(Entries) < ?PAGE_SIZE,
%    [#h3{body= <<"My reviews">>, class=[blue]},
    [#panel{id=myreviews, body=[
      #panel{id=EsId, body=[#product_entry{entry=E, mode=line, controls=product:controls(E)} || E <- Entries]},
      #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
        if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback = {check_more, Last, Info}} end ]} ]} ].

control_event("products", _) ->
  SearchTerm = wf:q(term),
  Data = [ [list_to_binary(Id++"="++binary_to_list(Name)), Name] || #product{id=Id, title=Name} <- kvs:all(product), string:str(string:to_lower(binary_to_list(Name)), string:to_lower(SearchTerm)) > 0],
  element_textboxlist:process_autocomplete("products", Data, SearchTerm);
control_event(_, _) -> ok.


event(init) -> wf:reg(?MAIN_CH), [];
event({post_entry, EditorId, TitleId, MediasId}) ->
  User = wf:user(),
  Desc = wf:q(EditorId),
  Title = wf:q(TitleId),
  Prods = wf:q(products),

  Products = [case kvs:get(product,S) of {error,_}->[]; {ok,G} ->G end || S<-string:tokens(Prods, ",")],

  Groups = lists:flatten([ [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where, type=member} <- kvs_group:participate(ProdId)] 
    || #product{id=ProdId} <- Products]),

  Recipients = lists:append([
    [{product, ProdId, lists:keyfind(reviews,1,Feeds)} || #product{id=ProdId, feeds=Feeds} <- Products],
    [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <- Groups],
    [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}] 
  ]),
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
                      type=reviews,
                      media=Medias,
                      title=Title,
                      description=Desc,
                      shared=""}, TitleId, EditorId, MediasId, myreviews]) || {RoutingType, To, {_, FeedId}} <- Recipients];

event({remove_media, M, Id}) ->
  Ms = case wf:session(medias) of undefined -> []; Mi -> Mi end,
  New = lists:filter(fun(E)-> error_logger:info_msg("take ~p compare with ~p and = ~p", [E,M, E/=M]),  E/=M end, Ms),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New));

event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id="++Id);
event({read, reviews, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[account]Page event: ~p", [Event]), ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([user,_,entry,_,add],
                 [#entry{} = Entry, Tid, Eid, MsId, myreviews])->
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:wire("$('#products').html('');"),
  wf:insert_top(myreviews, #product_entry{entry=Entry, mode=line, controls=product:controls(Entry)}),
  wf:wire("Holder.run();");
process_delivery(R,M) -> product:process_delivery(R,M).
