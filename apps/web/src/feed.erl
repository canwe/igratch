-module(feed).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

render_element(#feed_view{icon=Icon, title=Title, feed=Feed, owner=Owner})->
    wf:render(feed(Owner, Feed, Title, Icon)).

feed(Owner, FeedName, Title, Icon) ->
    Who = wf:user(),
    {_, Fid} = Feed = lists:keyfind(FeedName, 1, Owner#user.feeds),
    Entries = kvs:entries(Feed, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    EsId = wf:temp_id(),
    BtnId = wf:temp_id(),
    Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId},
    NoMore = length(Entries) < ?PAGE_SIZE,
    dashboard:section([
        #h3{class=[blue], body= Title},
        #panel{id=?ID_FEED(Fid), body=[
            #panel{id=EsId, body=[#product_entry{entry=E, mode=line, controls=product:controls(E)} || E <- Entries]},
            #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
            if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback = {check_more, Last, Info}} end ]} ]}
    ], Icon).

control_event(_, _) -> ok.
api_event(_,_,_) -> ok.

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

%process_delivery([show_entry], [Entry, #info_more{} = Info]) ->
%  wf:insert_bottom(Info#info_more.entries, #feature_req{entry=Entry}),
%  wf:wire("Holder.run();"),
%  wf:update(Info#info_more.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Entry, Info}});

process_delivery([user,_,entry,_,add],
                 [#entry{feed_id=Fid} = Entry, Rid, Tid, Eid, MsId,_])->
    error_logger:info_msg("[Feed-process_delivery]Add entry ~p", [Fid]),
    wf:session(medias, []),
    wf:update(MsId, []),
    wf:wire(wf:f("$('#~s').val('');", [Tid])),
    wf:wire(#jq{target=Eid, method=[html], args="''"}),
    wf:wire(wf:f("$('#~s').parent().find(':hidden').parent().html('');", [Rid])),
    wf:insert_top(?ID_FEED(Fid), #product_entry{entry=Entry, mode=line, controls=product:controls(Entry)}),
    wf:wire("Holder.run();");

process_delivery(R,M) -> error_logger:info_msg("delegate to product->"),product:process_delivery(R,M).
