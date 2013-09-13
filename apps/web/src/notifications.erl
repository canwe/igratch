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
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){ console.log(e.target); tabshow($(e.target).attr('href'));});"),
    Tab = case wf:qs(<<"tab">>) of undefined -> <<"notifications">>; T ->  T end,
    wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').tab('show');});",[Tab])),

    Nav = {wf:user(), notifications, subnav()},
    index:header() ++ dashboard:page(Nav,
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
            #panel{id=Id, class=["tab-pane"], body=[]} || Id <- [notifications,sent,archive]] }) ++ index:footer().

subnav()-> [{sent, "sent"}, {archive, "archive"}].

feed(notifications)->
    User = wf:user(),
    {_, Id} = lists:keyfind(direct, 1, element(#iterator.feeds, User)),
    State = ?FD_STATE(Id)#feed_state{view=direct, entry_id = #entry.entry_id, html_tag=panel, enable_selection=true, enable_traverse=true},
    Is = #input_state{entry_type=direct},
    error_logger:info_msg("Notification feed state: ~p", [State]),
    #feed2{title= <<"Notification ">>, icon="icon-envelope-alt", state=State, header=[
        #input{placeholder_rcp= <<"E-mail/User">>, placeholder_ttl= <<"Subject">>, role=user, collapsed=true, expand_btn= <<"compose">>, class=["feed-table-header"], icon="", 
            state=Is, feed_state=State}
    ]};

feed(sent)->
    User = wf:user(),
    {_, Id} = lists:keyfind(sent, 1, element(#iterator.feeds, User)),
    State = #feed_state{container_id=Id,view=direct, enable_selection=true},
    #feed2{title= <<"Sent Messages ">>, icon="icon-signout", state=State,
        header=[#tr{class=["feed-table-header"], cells=[]} ]};

feed(archive)->
    User = wf:user(),
    {_, Id} = lists:keyfind(archive, 1, element(#iterator.feeds, User)),
    State = #feed_state{container_id=Id,view=direct, enable_selection=true},
    #feed2{title= <<"Archive ">>, icon="icon-signout", state=State,
        header=[#tr{class=["feed-table-header"], cells=[]} ]};

feed(_) -> [index:error("404")].

control_event(_, _) -> ok.
api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    error_logger:info_msg(""),
    wf:update(list_to_atom(Id), feed(list_to_atom(Id)));
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({allow, Whom, Eid, Feature, #feed_state{}=S}) ->
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
                          type=direct,
                          media=[],
                          title= <<"Re: Feature request">>,
                          description= "You have been granted "++ io_lib:format("~p", [Feature])++"!",
                          shared=""}, #input_state{}, S]) || {RoutingType, To, {_, FeedId}}=R <- ReplyRecipients] end,

  Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
  error_logger:info_msg("Remove recipients: ~p", [Recipients]),
  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [#entry{id={Eid, Feedid},entry_id=Eid}, #input_state{}, S]) || {RouteType, To, {_, Feedid}=Fid} <- Recipients];

event({cancel, From, Eid, {feature, Feature}=Type, #feed_state{}=S}) ->
    error_logger:info_msg("Reject ~p", [Feature]),
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
                          type=direct,
                          media=[],
                          title= <<"Re: Feature request">>,
                          description= "You request for "++ io_lib:format("~p", [Feature])++" has been rejected!",
                          shared=""}, #input_state{}, S]) || {RoutingType, To, {_, FeedId}}=R <- ReplyRecipients] end,

  % delete message from feed
  Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
  error_logger:info_msg("Remove recipients: ~p", [Recipients]),
  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [#entry{id={Eid, Fid}, entry_id=Eid}, #input_state{}, S]) || {RouteType, To, {_, Fid}} <- Recipients];

event(Event) -> error_logger:info_msg("[notification] event: ~p", [Event]), ok.

process_delivery([user,_,entry,_,add]=R, M)->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed2:process_delivery(R,M);

process_delivery([_,_,entry,_,delete]=R, M) -> 
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed2:process_delivery(R,M);

process_delivery([_,unregister]=R, M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed2:process_delivery(R,M);

process_delivery(R,M) -> feed2:process_delivery(R,M).
