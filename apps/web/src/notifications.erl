-module(notifications).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

-define(NOTIFICATION_STATE(Id), ?FD_STATE(Id)#feed_state{
    view=direct,
    entry_id = #entry.entry_id,
    html_tag=panel,
    enable_selection=true,
    enable_traverse=true}).

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
    State = ?NOTIFICATION_STATE(Id),
    Is = #input_state{entry_type=direct,collapsed=true, show_media = false},
    #feed_ui{title=title(notification), icon=icon(notification), state=State, header=[
        #input{placeholder_rcp= <<"E-mail/User">>,
            placeholder_ttl= <<"Subject">>,
            role=user,
            expand_btn= <<"compose">>,
            class=["feed-table-header"],
            icon="",
            state=Is, feed_state=State}]};
feed(Feed)->
    Feeds = case wf:user() of undefined -> []; User -> element(#iterator.feeds, User) end,
    case lists:keyfind(Feed, 1, Feeds) of false -> index:error("404");
    {_, Id} -> #feed_ui{title=title(Feed), icon=icon(Feed),
        state=?NOTIFICATION_STATE(Id),
        header=[#tr{class=["feed-table-header"], cells=[]}]};
    R -> error_logger:info_msg("EE: ~p", [R]) end.

title(sent)-> <<"Sent Messages ">>;
title(notification)-> <<"Notification ">>;
title(archive)-> <<"Archive ">>;
title(_) -> <<"">>.

icon(sent)-> "icon-envelope";
icon(notification)-> "icon-envelope-alt";
icon(archive) -> "icon-archive";
icon(_)-> "".

feature_reply(#user{}=Whom, Feature, Msg, Eid, #feed_state{}=S) ->
    case lists:keyfind(direct, 1, Whom#user.feeds) of false -> skip;
        {_,Id}=Feed ->
            Type = direct,
            Is = #input_state{
                collect_msg = false,
                show_recipients = false,
                entry_type = direct,
                recipients = [{user, Whom#user.email, Feed}],
                title = "Re: Feature <b>"++ wf:to_list(Feature)++"</b> request",
                description = Msg},
            input:event({post, Type, Is, ?NOTIFICATION_STATE(Id)}),

            User = wf:user(),
            Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
            [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete],
                        [#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid}, Is, ?FD_STATE(Fid,S)])
                || {RouteType, To, {_,Fid}} <- Recipients] end.

control_event(_, _) -> ok.
api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(list_to_atom(Id)));
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({allow, Whom, Eid, Feature, #feed_state{}=S}) ->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, Whom}, Feature, allow),
        feature_reply(U, Feature, "Your request accepted!", Eid, S) end;

event({cancel, From, Eid, Feature, #feed_state{}=S}) ->
    case kvs:get(user, From) of {error, not_found} -> skip;
    {ok, U} -> feature_reply(U, Feature, "Your request is prohibited!", Eid, S) end;

event(Event) -> error_logger:info_msg("[notification] event: ~p", [Event]), ok.

process_delivery([user,_,entry,_,add]=R, M)->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed_ui:process_delivery(R,M);

process_delivery([_,_,entry,_,delete]=R, M) -> 
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed_ui:process_delivery(R,M);

process_delivery([_,unregister]=R, M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed_ui:process_delivery(R,M);

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
