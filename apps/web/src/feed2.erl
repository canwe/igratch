%%
%% View feed element
%%
-module(feed2).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

%% todo: create element js base and wire the javascript postbacks for traverse and 
render_element(#feed2{title=Title, icon=Icon, entry_type=Type, container=Container, container_id=ContainerId, page_size=PageSize, header=TableHeader, entry_view=View, 
    traverse_mode=TraverseMode, table_mode=TableMode}=F2) ->
    error_logger:info_msg("Traverse mode: ~p", [TraverseMode]),
    FeedTitle = wf:temp_id(), SelectAllCtl = wf:temp_id(), SelectAll = wf:temp_id(),
    SelToolbar = wf:temp_id(), DeleteBtn = wf:temp_id(), ChangeFeed = wf:temp_id(),
    FeedToolbar = wf:temp_id(), PageLbl = wf:temp_id(), PrevId = wf:temp_id(), NextId = wf:temp_id(), Close = wf:temp_id(),
    EntriesId = wf:temp_id(),MoreToolbar = wf:temp_id(),

    error_logger:info_msg("Container: ~p ~p", [Container, ContainerId]),
    case kvs:get(Container, ContainerId) of {error, not_found} -> wf:render(dashboard:section([#h3{class=[blue], body= Title}, index:info("empty")], Icon));
    {ok, Feed} ->
    Entries = kvs:entries(Feed, Type, PageSize),
    Total = element(#container.entries_count, Feed),
    Current = length(Entries),

    {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,

    SelectedKey = wf:temp_id(),
    wf:session(SelectedKey,[]),

    State = #feed_state{
        view = View,
        entry_type = Type,
        container = Container,
        container_id = ContainerId,
        feed_title=FeedTitle,
        selection = F2#feed2.selection,
        selectall_ctl = SelectAllCtl,
        select_all=SelectAll,
        select_toolbar=SelToolbar,
        delete_btn = DeleteBtn,
        feed_toolbar=FeedToolbar,
        more_toolbar = MoreToolbar,
        prev=PrevId,
        next=NextId,
        page_label=PageLbl,
        close=Close,
        entries=EntriesId,
        start_element = First,
        last_element = Last,
        start = 1,
        total = Total,
        current = Current,
        page_size = PageSize,
        selected_key=SelectedKey},

    Header = #panel{id=FeedTitle, class=["row-fluid", "feed-title"], body=[
        #panel{class=[span1], body=#h3{body=[
            #i{class=[Icon, blue]},
            if F2#feed2.selection == true ->
                #span{id=SelectAllCtl, body=#checkbox{id=SelectAll, class=[checkbox, inline],
                    postback={select, SelectAll, State}, delegate=feed2, source=[SelectAll],
                    value=[io_lib:format("~p", [element(#iterator.id, E)]) ++"|"|| E <- Entries],
                    style= if Total > 0 -> [] ; true-> "display:none;" end}}; true -> [] end ]}},

        #panel{class=[span11], body=#h3{body=[
                Title,
                #span {id=SelToolbar, style="display:none;", body=[
                    #link{id=DeleteBtn, class=[btn], body=[<<"delete">>], postback={delete, State}, delegate=feed2 },
                    #link{id=ChangeFeed, class=[btn], body=[<<"archive">>]} ]},

                if TraverseMode == true -> #span{class=["pull-right"], body=[
                    #panel{id=FeedToolbar, body=if Total > 0 -> [
                        #small{id=PageLbl, body=[integer_to_list(State#feed_state.start), "-", integer_to_list(Current), " of ", integer_to_list(Total)]},
                        #link{id=PrevId,class=[btn, case element(#iterator.next, First) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>],
                            postback={traverse, #iterator.next, First, State}, delegate=feed2},
                        #link{id=NextId,class=[btn, case element(#iterator.prev, Last)  of undefined -> "disabled"; _ -> "" end], body=[<<">">>],
                            postback={traverse, #iterator.prev, Last, State}, delegate=feed2}]; true -> [] end},
                    #link{id=Close, class=[close, "text-error"], postback={cancel_select, State}, delegate=feed2, body= <<"&times;">>} ]}; true -> [] end
        ]}} ]},

    Body = if TableMode == true -> #table{id=EntriesId, class=[table, "table-hover"],
        header=[TableHeader],
        body=[[#feed_entry2{entry=G, state=State, view=View} || G <- Entries]]};
    true -> #panel{id=EntriesId, class=[feed], body=[
        TableHeader,
        [#feed_entry2{entry=G, state=State, view=View} || G <- Entries]]} end,

    Footer = if TraverseMode == false -> #panel{id=MoreToolbar, class=["btn-toolbar", "text-center"], body=[
        if Current < PageSize -> []; true -> #link{class=?BTN_INFO, body= <<"more">>, delegate=feed2, postback = {check_more, Last, State}} end
    ]}; true -> [] end,

    wf:render([Header, Body, Footer]) end;

% feed entry representation

render_element(#feed_entry2{entry=#group{id=Id, name=Name, description=Desc, scope=Scope}, state=State}) ->
    Tr = #tr{id=Id++"tr", class=[case Scope of private -> "info"; _-> "" end], cells=[
        if State#feed_state.selection == true ->
            #td{body= [#checkbox{id=Id, postback={select, Id, State}, delegate=feed2, source=[Id], value=Id}]}; true -> [] end,
        #td{body=Id},
        #td{body=Name},
        #td{body=Desc},
        #td{body=atom_to_list(Scope)}]},
    element_tr:render_element(Tr);
render_element(#feed_entry2{entry=#user{username=Id, email=Email}=U, state=State}) ->
    Tr = #tr{id=Id++"tr",cells=[
        if State#feed_state.selection ->
            #td{body=#checkbox{id=Id, postback={select, Id, State}, delegate=feed2, source=[Id], value=Id}}; true -> [] end,
        #td{body=#link{body=Email, postback={view, Email}}},
        #td{body=[profile:features(wf:user(), U, "icon-2x")]},
        #td{body=case kvs:get(user_status, Email) of {ok,Status} -> product_ui:to_date(Status#user_status.last_login); {error, not_found}-> "" end} ]},
    element_tr:render_element(Tr);

render_element(#feed_entry2{entry=#product{id=Id, title=Title}, state=State})->
    Tr = #tr{id=Id++"tr", cells=[
        if State#feed_state.selection == true ->
            #td{body= [#checkbox{id=Id, postback={select, Id, State}, delegate=feed2, source=[Id], value=Id}]}; true -> [] end,
        #td{body=Title}
    ]},
    element_tr:render_element(Tr);

render_element(#feed_entry2{entry=#acl_entry{id=Id, accessor={user, Accessor}, action=Action}, state=State}) ->
    Aid = io_lib:format("~p", [Id]),
    Tr = #tr{cells=[
        if State#feed_state.selection == true -> 
            #td{body= [#checkbox{id=Aid, postback={select, Aid, State}, delegate=feed2, source=[Aid], value=Aid}]}; true -> [] end,
        #td{body= Aid},
        #td{body= Accessor},
        #td{body= atom_to_list(Action)}]},
    element_tr:render_element(Tr);

render_element(#feed_entry2{entry=#entry{entry_id=Id}=E, state=State, view=direct})->
    User = wf:user(),
    Id = E#entry.entry_id,
    From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
    error_logger:info_msg("Entry id: ~p", [Id]),
    error_logger:info_msg("Entry type: ~p", [E#entry.type]),
    IsAdmin = case User of undefined -> false; Us when Us#user.email==User#user.email -> true; _-> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,

    Entry = #panel{id=E#entry.entry_id, class=["row-fluid", article], body=[
        if State#feed_state.selection == true ->
            #checkbox{id=Id, postback={select, Id, State}, delegate=feed2, source=[Id], value=Id}; true -> [] end,
        #panel{class=[], body=[
            #p{body=[
                #small{body=["[", product_ui:to_date(E#entry.created), "] "]},
                #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
                <<" ">>,
                wf:js_escape(wf:to_list(E#entry.title)),
                case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end
            ]},
            #p{body= wf:js_escape(E#entry.description)},
            #panel{id=?ID_TOOL(Id), class=[], body= [
                case E#entry.type of {feature, _} when IsAdmin ->
                    #panel{class=["btn-toolbar"], body=[
                        #link{class=[btn, "btn-success"], body= <<"allow">>, postback={allow, E#entry.from, E#entry.entry_id, E#entry.type, State}},
                        #link{class=[btn, "btn-info"], body= <<"reject">>, postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type, State}} ]};
                direct -> [];
                reply -> [];
                product -> [
                    #link{body= [#i{class=["icon-edit", "icon-large"]},<<"edit">>], postback={edit_product, E}},
                    #link{body=[#i{class=["icon-remove", "icon-large"]}, <<"remove">>], postback={remove_product, E}},
                    #link{body=[<<"view ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, product, E#entry.entry_id}}];
                 T -> [#link{body=[<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, T, E#entry.entry_id}}] end
            ]}
        ]}
    ]},
    element_panel:render_element(Entry);

render_element(_) -> error_logger:info_msg("[feed2]render_element(#unknown{})").

% events

event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({traverse, Direction, Start, #feed_state{}=S}) -> traverse(Direction, Start, S);

event({delete, #feed_state{selected_key=Key}=S}) ->
    [begin {ok, Obj} = kvs:get(S#feed_state.entry_type, Id),
        msg:notify([kvs_feed, S#feed_state.entry_type, unregister], [Obj, S])
    end || Id <- wf:session(Key)];

event({cancel_select, #feed_state{}=S}) -> deselect(S);

event({select, Sel, #feed_state{selected_key=Key}=S})-> 
    Selection = wf:session(Key),
    NewSel = case wf:q(Sel) of "undefined" -> if Sel == S#feed_state.select_all -> sets:new(); true -> sets:from_list(Selection--[Sel]) end;
    Val -> Vals = string:tokens(Val,"|"),
        [begin
            wf:wire(#jq{target=C, method=["prop"], args=["'checked', 'checked'"]}),
            wf:wire(#jq{target=C++"tr", method=["addClass"], args=["'warning'"]})
        end || C <- Vals],
        sets:from_list(Vals++Selection)
    end,
    case sets:size(NewSel) of 0 -> deselect(S);
    Size ->
        wf:wire(#jq{target=S#feed_state.select_toolbar, method=["show"]}),
        wf:wire(#jq{target=S#feed_state.close, method=["show"]}),
        wf:wire(#jq{target=S#feed_state.feed_title, method=["attr"], args=["'style', 'background-color:lightblue'"]}),
        wf:wire(#jq{target=S#feed_state.feed_toolbar, method=["hide"]}),
        if Size == S#feed_state.page_size orelse Size == S#feed_state.current ->
            wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', 'checked'"]});
        true ->
            wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', false"]}) end
    end,
    wf:session(Key, sets:to_list(NewSel));

event({check_more, Start, #feed_state{}=S}) ->
    traverse_entries(S#feed_state.entry_type, element(#iterator.prev,Start), S#feed_state.page_size, S),
    wf:update(S#feed_state.more_toolbar, []);

event(E)-> error_logger:info_msg("[feed2] event: ~p", [E]).

deselect(#feed_state{selected_key=Key}=S) ->
    [wf:wire(#jq{target=C, method=["prop"], args=["'checked', false"]}) || C <- wf:session(Key)],
    [wf:wire(#jq{target=C++"tr", method=["removeClass"], args=["'warning'"]}) || C <- wf:session(Key)],
    wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', false"]}),
    wf:wire(#jq{target=S#feed_state.select_toolbar, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_toolbar, method=["show"]}),
    wf:wire(#jq{target=S#feed_state.close, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_title, method=["attr"], args=["'style', 'background-color:none;'"]}),
    wf:session(Key, []).

traverse(Direction, Start, #feed_state{}=S)->
    error_logger:info_msg("=> Traverse ~p from ~p~n", [Direction ,Start]),
    Entries = case element(Direction, Start) of
        undefined  -> kvs:entries(kvs:get(S#feed_state.container, S#feed_state.container_id), S#feed_state.entry_type, S#feed_state.page_size);
        Prev -> kvs:entries(S#feed_state.entry_type, Prev, S#feed_state.page_size, Direction)
    end,

    {NewLast, NewFirst} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,

    Total = S#feed_state.total,
    Current = length(Entries),
    NewStart = case Direction of #iterator.prev -> S#feed_state.start+S#feed_state.page_size; #iterator.next -> S#feed_state.start-S#feed_state.page_size end,

    State = S#feed_state{start=NewStart, start_element=NewFirst, last_element=NewLast, current=Current},

    wf:update(S#feed_state.entries, [
        wf_tags:emit_tag(<<"thead">>, wf:render([#tr{class=["feed-table-header"], cells=[
            #th{body= <<"">>},
            #th{body= <<"id">>},
            #th{body= <<"name">>},
            #th{body= <<"description">>},
            #th{body= <<"scope">>}]}]), []),

        [#feed_entry2{entry=G, state=State} || G <- Entries]]),

    wf:replace(S#feed_state.prev, #link{id=State#feed_state.prev, class=[btn, case element(#iterator.next, NewFirst) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>], 
        postback={traverse, #iterator.next, NewFirst, State}, delegate=feed2}),
    wf:replace(S#feed_state.next, #link{id=State#feed_state.next, class=[btn, case element(#iterator.prev, NewLast) of undefined -> "disabled"; _ -> "" end], body=[<<">">>], 
        postback={traverse, #iterator.prev, NewLast, State}, delegate=feed2}),

    wf:update(S#feed_state.page_label, [integer_to_list(NewStart), "-", integer_to_list(NewStart+Current-1), " of ", integer_to_list(Total)]),

    if State#feed_state.selection == true ->
        wf:update(S#feed_state.selectall_ctl,
        #checkbox{id=State#feed_state.select_all, class=[checkbox, inline], postback={select, State#feed_state.select_all, State}, delegate=feed2,
            source=[State#feed_state.select_all], value=[element(#iterator.id, G)++"|"|| G <- Entries],
            style=if Total > 0 -> [] ; true-> "display:none;" end}); true -> [] end,
    wf:replace(State#feed_state.delete_btn, #link{id=State#feed_state.delete_btn, class=[btn], body=[<<"delete">>], postback={delete, State}, delegate=feed2}).

traverse_entries(_,undefined,_, #feed_state{more_toolbar=BtnId}) -> self() ! {delivery, [somepath, no_more], [BtnId]}, [];
traverse_entries(_,_,0,_) -> [];
traverse_entries(RecordName, Next, Count, S)->
    case kvs:get(RecordName, Next) of {error, not_found} -> [];
    {ok, R}-> self() ! {delivery, [somepath, show_entry], [R, S]}, [R | traverse_entries(RecordName, element(#iterator.prev, R), Count-1, S)] end.

process_delivery([create], [{_Creator, _Id, _Name, _Desc, _Publicity}]) ->
    error_logger:info_msg("responce to create group");

process_delivery([Type, unregistered], {{ok, Id}, [S]})->
    error_logger:info_msg("=>>~p unregistered: ~p", [Type, Id]),
    deselect(S),

    Start = S#feed_state.start_element,
    State = S#feed_state{total=S#feed_state.total-1},

    case element(#iterator.next, Start) of
        undefined -> traverse(#iterator.next, #iterator{}, State);

        N when Id == element(#iterator.id, Start) ->
            case element(#iterator.prev, Start) of
                undefined -> traverse(#iterator.next, Start, State);
                _ -> case kvs:get(S#feed_state.entry_type,N) of {error,_} -> ok; {ok, G} -> traverse(#iterator.prev, G, State) end end;

        N -> case kvs:get(S#feed_state.entry_type, element(#iterator.id, Start)) of 
                {error, not_found} ->  traverse(#iterator.next, Start, State);
                _-> case kvs:get(S#feed_state.entry_type,N) of {error,_} -> ok; {ok, G} -> traverse(#iterator.prev, G, State) end end end;

process_delivery([_,_,entry,_,add],
                 [#entry{} = E, #input_state{}=I, #feed_state{}=S])->
    error_logger:info_msg("[Feed2 - process_delivery] Add entry: ~p", [E]),
%    wf:session(medias, []),
%    wf:update(MsId, []),
%    wf:wire(wf:f("$('#~s').val('');", [Tid])),
%    wf:wire(#jq{target=Eid, method=[html], args="''"}),
%    wf:wire(wf:f("$('#~s').trigger('Reset');", [Rid])),
%    error_logger:info_msg("Render entry of type: ~p", [Entry#entry.type]),
%    error_logger:info_msg("MODE: ~p", [Mode]),
    error_logger:info_msg("insert top to ~p view: ~p", [S#feed_state.entries, S#feed_state.view]),
    wf:insert_top(S#feed_state.entries, #feed_entry2{entry=E, state=S, view=S#feed_state.view}),
    wf:wire("Holder.run();");

process_delivery([show_entry], [Entry, #feed_state{} = S]) ->
    error_logger:info_msg("[feed2] show_entry ~p", [Entry]),
    wf:insert_bottom(S#feed_state.entries, #feed_entry2{entry=Entry, state=S}),
    wf:wire("Holder.run();"),
    wf:update(S#feed_state.more_toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=feed2, postback={check_more, Entry, S}});

process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;

process_delivery(R, M) -> error_logger:info_msg("[feed2] ~p:~p", [R,M]).


