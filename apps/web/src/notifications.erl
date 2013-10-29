-module(notifications).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

-jsmacro([on_shown/0,show/1]).

on_shown() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("shown", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main()-> case wf:user() of undefined -> wf:redirect("/");
    _-> #dtl{file="prod", bindings=[{title,<<"notifications">>},
                                    {body, body()},{css,?DIRECT_CSS},{less,?LESS},{bootstrap, ?DIRECT_BOOTSTRAP}]} end.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(on_shown()),

    Nav = {wf:user(), notifications, subnav()},
    index:header() ++ dashboard:page(Nav,
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
            #panel{id=notifications, class=["tab-pane", active], body=feed(notifications)},
            [#panel{id=Id, class=["tab-pane"], body=[]} || Id <- [sent,archive]]] }) ++ index:footer().

subnav()-> [{sent, "sent"}, {archive, "archive"}].

feed(Feed)->
    error_logger:info_msg("Show ~p", [Feed]),
    User = wf:user(),
    case lists:keyfind(case Feed of notifications -> direct; _-> Feed end, 1, element(#iterator.feeds, User)) of false->
        index:error("No feed "++wf:to_list(Feed));
    {_, Id} ->
        State = case wf:session(Id) of undefined ->
            Fs = ?DIRECT_STATE(Id), wf:session(Id, Fs), Fs; FS->FS end,
        InputState = case wf:session(?FD_INPUT(Id)) of undefined ->
            Is = ?DIRECT_INPUT(Id), wf:session(?FD_INPUT(Id), Is), Is; IS->IS end,
        #feed_ui{title=title(Feed),
                 icon=icon(Feed),
                 state=State,
                 header=[case Feed of notifications ->
                    #input{icon="", state=InputState}; _-> #tr{class=["feed-table-header"]} end],
                 selection_ctl=case Feed of notifications -> [
                    #link{class=[btn], body=#i{class=["icon-archive"]},
                    data_fields=?TOOLTIP, title= <<"archive">>, postback={archive, State}}];_-> [] end } end.

title(sent)-> <<"Sent Messages ">>;
title(notifications)-> <<"Notifications ">>;
title(archive)-> <<"Archive ">>;
title(_) -> <<"">>.

icon(sent)-> "icon-envelope";
icon(notifications)-> "icon-envelope-alt";
icon(archive) -> "icon-archive";
icon(_)-> "".

% Render direct messages

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=direct}=State})->
    Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    User = wf:user(),
    From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
    IsAdmin = case User of undefined -> false; 
        Us when Us#user.email==User#user.email -> true; 
        _-> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,

    wf:render([
        #p{body=[#small{body=["[", product_ui:to_date(E#entry.created), "] "]},
            #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
            <<" ">>,
            E#entry.title,
            case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end ]},
        #p{body= E#entry.description},
        #panel{id=?EN_TOOL(Id), body= case E#entry.type of {feature, _} when IsAdmin ->
            #panel{class=["btn-toolbar"], body=[
                #link{class=[btn, "btn-success"], body= <<"allow">>,
                    postback={allow, E#entry.from, E#entry.entry_id, E#entry.type}, delegate=admin},
                #link{class=[btn, "btn-info"], body= <<"reject">>, 
                    postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type}, delegate=admin} ]};
        _ -> [] end }]);
render_element(E)->feed_ui:render_element(E).

% Events

control_event(_, _) -> ok.
api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case list_to_atom(Id) of notifications -> ok;
    _-> wf:update(list_to_atom(Id), feed(list_to_atom(Id))) end;
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({archive, #feed_state{selected_key=Selected, visible_key=Visible}}) ->
    Selection = sets:from_list(wf:session(Selected)),
    User = wf:user(),
    case lists:keyfind(archive, 1, User#user.feeds) of false -> ok;
    {_,Fid} ->
        [case kvs:get(entry, Id) of {error,_} -> ok;
        {ok, E} ->
            msg:notify( [kvs_feed, user, User#user.email, entry, Eid, add],
                        [E#entry{id={Eid, Fid}, feed_id=Fid}]),

            msg:notify( [kvs_feed, User#user.email, entry, delete], [E])
        end || {Eid,FeedId}=Id <- wf:session(Visible), sets:is_element(wf:to_list(erlang:phash2(Id)), Selection)] end;
event(_) -> ok.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), notifications, subnav()})),
    feed_ui:process_delivery(R,M).
