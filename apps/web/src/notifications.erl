-module(notifications).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main()-> case wf:user() of undefined -> wf:redirect("/"); _-> #dtl{file="prod", bindings=[{title,<<"notifications">>},{body, body()}]} end.

body()->
  index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{id=side_menu, class=[span3], body=dashboard:sidebar_menu(wf:user(), wf:user(), notifications, [subnav()])},
        #panel{class=[span9], body=[
          dashboard:section(input(), "icon-edit"),
          dashboard:section(feed(), "icon-user")
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
    #h3{class=[blue], body= <<"Write message">>},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textboxlist{id=users, placeholder= <<"e-mail/User">>},
        #textbox{id=TitleId, class=[span12], placeholder= <<"Title">>},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE },
        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, class=[btn, "btn-large", "btn-success"], body= <<" Post">>,
          postback={post_entry, EditorId, TitleId, MsId}, source=[TitleId, EditorId, users] }]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias)}
      ]},
      #panel{class=[span2], body=[]}
  ]} ] end.

feed()->
  User = wf:user(),
  {Feed,Fid} = lists:keyfind(direct,1,User#user.feeds),
  Entries = kvs:entries({Feed,Fid}, undefined, ?PAGE_SIZE),
%  error_logger:info_msg("Entries: ~p", [Entries]),
  Last = case Entries of []-> []; E-> lists:last(E) end,
  BtnId = wf:temp_id(),
  Info = #info_more{fid=Fid, entries=direct, toolbar=BtnId},
  NoMore = length(Entries) < ?PAGE_SIZE,

  [
  #h3{class=[blue], body= <<"Notifications">>},
  #panel{id=direct, body=[#feature_req{entry=E} || E <- Entries]},
  #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
      if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Last, Info}} end ]}
  ].

notifications()->[
  #h3{class=[blue], body=[ <<"Notifications">>]},
  #accordion{nav_stacked = false, items= [
    message(true),
    message(false),
    message(false)
  ]},
  #panel{class=["btn-toolbar", "text-center"], body=[
    #link{url= <<"#">>, class=[btn, "btn-warning", "btn-large"], body=[#i{class=["icon-trash", "icon-white"]}, <<"Delete Entries">> ]},
    #link{url= <<"#">>, class=[btn, "btn-info", "btn-large"], body=[#i{class=["icon-ok", "icon-white"]}, <<"Mark as Read">>]}
  ]}
  ].

subnav()-> [
%  #li{class=[divider]},
%  #li{body=#link{body=[<<"messages   ">>, #span{class=[label, "label-info"], body= <<"3">>}]}},
%  #li{body=#link{body=[<<"comments   ">>, #span{class=[label, "label-info"], body= <<"0">>}]}} 
  ].

message(Text) -> {
  #panel{body=[
    #strong{body=[#span{class=["icon-chevron-sign-down"]}, <<" Message \"Lorem ipsum\" from User282">>,  #span{class=["pull-right"],body= [#i{class=["icon-trash", "icon-large"]}, #i{class=["icon-flag", "icon-large"]}]} ]}
  ]},
  #panel{class=["notif-message-inner", clearfix], body=[
    #panel{class=[span10, "message-text"], body=[
      #p{body= <<"Donec libero velit, rutrum ac mollis a, porttitor id libero. Mauris congue cursus scelerisque. Sed eu commodo metus. Vestibulum vel lobortis risus. Proin a quam felis. Vestibulum et nulla eu dolor egestas elementum vestibulum vel ipsum. Nam dui erat, varius non placerat et, cursus eget libero. Pellentesque sed ornare nunc. Vivamus volutpat nunc in felis tempor consectetur.">>}
    ]}
  ]}}.

control_event("users", _) ->
  SearchTerm = wf:q(term),
  Data = [begin
    error_logger:info_msg("found-> ~p | ~p", [Name, Id]),
    error_logger:info_msg(":~p", [[list_to_binary(Id++"="++wf:to_list(Name)), Name]]),
    [list_to_binary(Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
    end ||
    #user{email=Id, display_name=Name} <- kvs:all(user), string:str(string:to_lower(wf:to_list(Id)), string:to_lower(SearchTerm)) > 0],

  element_textboxlist:process_autocomplete("users", Data, SearchTerm);
control_event(_, _) -> ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({allow, Whom, Eid, Feature}) ->
  error_logger:info_msg("Allow ~p : ~p", [Whom, Feature]),
  kvs_acl:define_access({user, Whom}, Feature, allow),
  User = wf:user(),

  case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
      ReplyRecipients = [{user, U#user.email, lists:keyfind(direct, 1, U#user.feeds)}],
      error_logger:info_msg("Reply recipients ~p", [ReplyRecipients]),
      EntryId = kvs:uuid(),
      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
                  [#entry{id={EntryId, FeedId},
                          entry_id=EntryId,
                          feed_id=FeedId,
                          created = now(),
                          to = {RoutingType, To},
                          from=User#user.email,
                          type=reply,
                          media=[],
                          title= <<"Re: Feature request">>,
                          description= "You have been granted "++ io_lib:format("~p", [Feature])++"!",
                          shared=""}, skip, skip, skip, direct]) || {RoutingType, To, {_, FeedId}} <- ReplyRecipients] end,

  Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
  error_logger:info_msg("Remove recipients: ~p", [Recipients]),
  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [#entry{id={Eid, Feedid},entry_id=Eid}, User#user.email]) || {RouteType, To, {_, Feedid}=Fid} <- Recipients];

event({cancel, From, Eid, {feature, Feature}=Type}) ->
  User = wf:user(),
  % send message to user
  case kvs:get(user, From) of {error, not_found} -> skip;
    {ok, U} ->
      ReplyRecipients = [{user, U#user.email, lists:keyfind(direct, 1, U#user.feeds)}],
      error_logger:info_msg("Reply recipients ~p", [ReplyRecipients]),
      EntryId = kvs:uuid(),
      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
                  [#entry{id={EntryId, FeedId},
                          entry_id=EntryId,
                          feed_id=FeedId,
                          created = now(),
                          to = {RoutingType, To},
                          from=User#user.email,
                          type=reply,
                          media=[],
                          title= <<"Re: Feature request">>,
                          description= "You request for "++ io_lib:format("~p", [Feature])++" has been rejected!",
                          shared=""}, skip, skip, skip, direct]) || {RoutingType, To, {_, FeedId}} <- ReplyRecipients] end,

  % delete message from feed
  Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
  error_logger:info_msg("Remove recipients: ~p", [Recipients]),
  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [#entry{id={Eid, Feedid}, entry_id=Eid}, User#user.email]) || {RouteType, To, {_, Feedid}=Fid} <- Recipients];

event({post_entry, EditorId, TitleId, MediasId}) ->
  User = wf:user(),
  Desc = wf:q(EditorId),
  Title = wf:q(TitleId),
  Usrs = wf:q(users),

  Users = [case kvs:get(user,S) of {error,_}->[]; {ok,G} ->G end || S<-string:tokens(Usrs, ",")],

  Recipients = lists:append([
    [{user, Id, lists:keyfind(direct,1,Feeds)} || #user{email=Id, feeds=Feeds} <- Users],
    [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}] 
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
                      type=direct,
                      media=Medias,
                      title=Title,
                      description=Desc,
                      shared=""}, TitleId, EditorId, MediasId, direct]) || {RoutingType, To, {_, FeedId}} <- Recipients];

event({remove_media, M, Id}) ->
  Ms = case wf:session(medias) of undefined -> []; Mi -> Mi end,
  New = lists:filter(fun(E)-> error_logger:info_msg("take ~p compare with ~p and = ~p", [E,M, E/=M]),  E/=M end, Ms),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New));

event(Event) -> error_logger:info_msg("Notif Page event: ~p", [Event]), ok.

process_delivery([user,To,entry,_,add],
                 [#entry{type=Type, feed_id=Fid, description=D, title=T}=E,Tid, Eid, MsId, direct])->
  What = case kvs:get(user, To) of {error, not_found} -> #user{}; {ok, U} -> U end,
  User = wf:user(),
  {_, Direct} = lists:keyfind(direct, 1, User#user.feeds),
  if Direct == Fid -> wf:insert_top(direct, #feature_req{entry=E#entry{title=wf:js_escape(T), description=wf:js_escape(D)}}); true -> ok end,
  wf:update(side_menu, dashboard:sidebar_menu(User, User, notifications, [subnav()])),
  wf:wire(wf:f("$('#~s').val('');", [Tid])),
  wf:wire(wf:f("$('#~s').html('');", [Eid])),
  wf:wire("$('#users').html('');"),
  wf:session(medias, []),
  wf:update(MsId, []),
  wf:wire("Holder.run();");

process_delivery([show_entry], [Entry, #info_more{} = Info]) ->
  wf:insert_bottom(Info#info_more.entries, #feature_req{entry=Entry}),
  wf:wire("Holder.run();"),
  wf:update(Info#info_more.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Entry, Info}});
process_delivery([_,_,entry,Fid,delete], [E,From]) -> 
  User = wf:user(),
  Direct = lists:keyfind(direct, 1, User#user.feeds),
  if Direct == Fid ->
    wf:remove(E#entry.entry_id),
    wf:update(side_menu, dashboard:sidebar_menu(wf:user(), wf:user(), notifications, [subnav()]) );
  true -> ok end;

process_delivery(R,M) -> product:process_delivery(R,M).
