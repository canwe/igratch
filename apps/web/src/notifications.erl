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
            #input{title= <<"Write message">>, placeholder_rcp= <<"e-mail/User">>, placeholder_ttl= <<"Title">>},
            dashboard:section(feed(), "icon-user")
        ]} ]} } }

  ]++index:footer().

feed()->
  User = wf:user(),
  {Feed,Fid} = lists:keyfind(direct,1,User#user.feeds),
  Entries = kvs:entries({Feed,Fid}, undefined, ?PAGE_SIZE),
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

subnav()-> [
  #li{class=[divider]},
  #li{body=#link{body=[<<"sent">>, #span{class=[label, "label-info"], body= <<"1">>}]}},
  #li{body=#link{body=[<<"archive">>, #span{class=[label, "label-info"], body= <<"1">>}]}} 
  ].

control_event(_, _) -> ok.
api_event(_,_,_) -> ok.

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

event(Event) -> error_logger:info_msg("[notification] event: ~p", [Event]), ok.

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
