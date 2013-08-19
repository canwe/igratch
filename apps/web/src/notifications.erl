-module(notifications).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main()-> case wf:user() of undefined -> wf:redirect("/"); _-> #dtl{file="prod", bindings=[{title,<<"notifications">>},{body, body()}]} end.

body()->
  index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{id=side_menu, class=[span3], body=dashboard:sidebar_menu(wf:user(), wf:user(), notifications, [subnav()])},
        #panel{class=[span9], body=[
          dashboard:section(feed(), "icon-user")
        ]} ]} } }

  ]++index:footer().

feed()->
  User = wf:user(),
  {Feed,Fid} = lists:keyfind(direct,1,User#user.feeds),
  Entries = kvs_feed:entries({Feed,Fid}, undefined, ?PAGE_SIZE),
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

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({allow, To, Feature}) ->
  error_logger:info_msg("Allow ~p : ~p", [To, Feature]),
  kvs_acl:define_access({user, To}, Feature, allow),
  ok;
event({cancel, From, Eid, {feature, Feature}=Type}) ->
  error_logger:info_msg("Cancel ~p: ~p", [From, Eid]),
  User = wf:user(),

  % delete message from feed
  Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
  error_logger:info_msg("Recipients: ~p", [Recipients]),
  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [#entry{entry_id=Eid}, User#user.email]) || {RouteType, To, Fid} <- Recipients],

  % send message to user 
  case kvs:get(user, From) of {error, not_found} -> skip;
    {ok, U} ->
      ReplyRecipients = [{user, U#user.email, lists:keyfind(direct, 1, User#user.feeds)}],
      EntryId = kvs:uuid(),
      From = User#user.email,
      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
                  [#entry{id={EntryId, FeedId},
                          entry_id=EntryId,
                          feed_id=FeedId,
                          created = now(),
                          to = {RoutingType, To},
                          from=From,
                          type=reply,
                          media=[],
                          title= <<"Re: Feature request">>,
                          description= "You request for "++ io_lib:format("~p", [Feature])++" has been rejected!",
                          shared=""}, skip, skip, skip, direct]) || {RoutingType, To, {_, FeedId}} <- ReplyRecipients] end;

event(Event) -> error_logger:info_msg("Notif Page event: ~p", [Event]), ok.

process_delivery([user,To,entry,_,add],
                 [#entry{type=T}=E,Tid, Eid, MsId, TabId])->
%  error_logger:info_msg("[notification]: Entry ADD ~p", [E]),
  What = case kvs:get(user, To) of {error, not_found} -> #user{}; {ok, U} -> U end,
  User = wf:user(),
  error_logger:info_msg("[notification]: ~p receive Entry ADD from ~p", [User#user.email, What#user.email]),
  if What == User ->
    wf:insert_top(direct, #feature_req{entry=E}),
    wf:update(side_menu, dashboard:sidebar_menu(User, What , notifications, [subnav()]));
  true -> [] end;

process_delivery([show_entry], [Entry, #info_more{} = Info]) ->
  wf:insert_bottom(Info#info_more.entries, #feature_req{entry=Entry}),
  wf:wire("Holder.run();"),
  wf:update(Info#info_more.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Entry, Info}});
process_delivery([_,_,entry,_,delete], [E,From]) -> 
  error_logger:info_msg("Entry removed~p", [E#entry.entry_id]),
  What = case kvs:get(user, From) of {error, not_found} -> #user{}; {ok, U} -> U end,
  wf:remove(E#entry.entry_id),
  wf:update(side_menu, dashboard:sidebar_menu(wf:user(), wf:user(), notifications, [subnav()]) );
process_delivery(R,M) -> product:process_delivery(R,M).
