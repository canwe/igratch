-module(dashboard).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").

-define(PAGES, [
    {profile,       <<"profile">>,          undefined},
    {myreviews,     <<"reviews">>,          feed},
    {mygames,       <<"games">>,            products},
    {notifications, <<"notifications">>,    direct},
    {admin,         <<"admin">>,            undefined}
    ]).

sidenav(Who, What, Active, Tabs)->
    #panel{class=["dash-sidebar-menu"], body=#list{class=[nav, "nav-list","dash-sidebar-nav", "affix-top"], data_fields=[{<<"data-spy">>, <<"affix">>}], body=[
        begin
            SubTabs = if Active == Page -> [
                if Tabs /= [] -> #li{class=[divider]}; true-> [] end,
                [#li{class=if IsActive-> [active]; true-> [] end, body=[
                #link{url= "#"++wf:to_list(Id), data_fields=[{<<"data-toggle">>, <<"tab">>}], body= Label}]} || {Id, Label, IsActive} <- Tabs]]; true -> [] end,
            Class = if Active == Page -> [active]; true -> [] end,
            PageFeed = lists:keyfind(Feed, 1, What#user.feeds),
            error_logger:info_msg("Page feed: ~p", [PageFeed]),
            Badge = if PageFeed /= false -> {_, Fid} = PageFeed,
                    case kvs:get(feed, Fid) of {error,_}-> [];
                    {ok, F}-> #span{class=[label, "label-info"], body=integer_to_list(F#feed.entries_count)} end; true -> [] end,

            [#li{class=Class, body=#link{url="/"++atom_to_list(Page), body=[Title, Badge]}}, SubTabs]
        end || {Page, Title, Feed} <- lists:filter(fun({P,_,_})-> if Who == What ->
            case P of
                admin -> kvs_acl:check_access(What#user.email, {feature, admin})==allow;
                _-> true end; true -> P==Active end end, ?PAGES)
    ]}}.

alt_section(Body,Icon)-> section(wf:temp_id(), Body, Icon, "alt").
section(Body, Icon)-> section(wf:temp_id(),Body, Icon).
section(Id, Body, Icon) -> section(Id, Body,Icon, "").
section(Id, Body, Icon, Section) ->
  #section{class=["row-fluid", "dashboard-section"], body = [
    #panel{class=[span1], body=#i{class=[Icon, "icon-2x", blue]}},
    #panel{id=Id, class=[span11, "dashboard-unit", Section], body=Body} ]}.