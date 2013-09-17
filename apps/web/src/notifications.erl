-module(notifications).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

-jsmacro([on_shown/0,show/1]).

on_shown() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("shown", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main()-> case wf:user() of undefined -> wf:redirect("/"); _-> #dtl{file="prod", bindings=[{title,<<"notifications">>},{body, body()}]} end.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(on_shown()),
    wf:wire(show(case wf:qs(<<"tab">>) of undefined -> "'notifications'"; T ->  "'"++wf:to_list(T)++"'" end)),

    Nav = {wf:user(), notifications, subnav()},
    index:header() ++ dashboard:page(Nav,
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
            #panel{id=Id, class=["tab-pane"], body=[]} || Id <- [notifications,sent,archive]] }) ++ index:footer().

subnav()-> [{sent, "sent"}, {archive, "archive"}].

feed(notifications)->
    User = wf:user(),
    {_, Id} = lists:keyfind(direct, 1, element(#iterator.feeds, User)),
    State = ?DIRECT_STATE(Id),
    Is = #input_state{entry_type=direct,collapsed=true, show_media = false},
    #feed_ui{title=title(notification), icon=icon(notification), state=State, header=[
        #input{placeholder_rcp= <<"E-mail/User">>,
            placeholder_ttl= <<"Subject">>,
            expand_btn= <<"compose">>,
            class=["feed-table-header"],
            role=user,
            icon="",
            state=Is, feed_state=State}],
        selection_ctl=[
            #link{class=[btn], body=#i{class=["icon-archive"]},
            data_fields=?TOOLTIP, title= <<"archive">>, postback={archive, State}}
        ]};
feed(Feed)->
    error_logger:info_msg("Show feed: ~p", [Feed]),
    Feeds = case wf:user() of undefined -> []; User -> element(#iterator.feeds, User) end,
    case lists:keyfind(Feed, 1, Feeds) of false -> index:error("404");
    {_, Id} ->
        #feed_ui{title=title(Feed), icon=icon(Feed),
            state=?DIRECT_STATE(Id),
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

control_event(_, _) -> ok.
api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(list_to_atom(Id)));
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({archive, #feed_state{selected_key=Key} = S}) ->
    User = wf:user(),
    case lists:keyfind(archive, 1, User#user.feeds) of false -> error_logger:info_msg("no archive");
    {_,Fid} ->
        Is = #input_state{},
        [case kvs:get(entry, {Id, S#feed_state.container_id}) of {error,_} -> error_logger:info_msg("No object");
        {ok, E} ->
            msg:notify( [kvs_feed, user, User#user.email, entry, Id, add],
                        [E#entry{id={Id, Fid}, feed_id=Fid}, Is, ?FD_STATE(Fid, S)]),

            msg:notify( [kvs_feed, user, User#user.email, entry, Fid, delete], [E, Is, S])
        end || Id <- wf:session(Key)] end;
event(Event) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    if IsAdmin -> admin:event(Event); true -> ok end.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed_ui:process_delivery(R,M).
