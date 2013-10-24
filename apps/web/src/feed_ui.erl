-module(feed_ui).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

feed_state(Id) ->
    M = ?CTX#context.module,
    case lists:keyfind(exports, 1, M:module_info()) of false -> false;
        {exports, Ex} -> case lists:keyfind(feed_states, 1, Ex) of
            {feed_states, 0} ->
                case lists:keyfind(Id, 1, M:feed_states()) of false -> ok;
                    {Id, State} -> State end;
            _ -> false end end.

input_state(Id) -> M = ?CTX#context.module,
    case lists:keyfind(exports, 1, M:module_info()) of false -> false;
        {exports, Ex} -> case lists:keyfind(input_states, 1, Ex) of
            {input_states, 0} ->
                case lists:keyfind(Id, 1, M:input_states()) of false -> ok;
                    {Id, State} -> State end;
            _ -> false end end.

render_element(#feed_ui{state=S}=F) ->
    Title = F#feed_ui.title,
    Icon = F#feed_ui.icon,
    IconUrl = F#feed_ui.icon_url,
    Class= F#feed_ui.class,
    TableHeader = F#feed_ui.header,
    SelectionCtl = F#feed_ui.selection_ctl,

    wf:render(#section{class=[feed, Class], body=[
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_}->
            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1],  body=#h4{body=[#i{class=[Icon]}]}},
                #panel{class=[span11], body=#h4{body=[wf:to_list(Title), #span{class=["text-warning"], body= <<" [no feed]">>}]}}]};
        {ok, Feed} ->
            wf:session(S#feed_state.selected_key,[]),

            Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),

            wf:session(S#feed_state.visible_key, [element(S#feed_state.entry_id, E)|| E<-Entries]),

            Total = element(#container.entries_count, Feed),
            Current = length(Entries),
            {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,
            State = S#feed_state{
                start_element = First,
                last_element = Last,
                start = 1,
                total = Total,
                current = Current},
            [

            %% header

            if S#feed_state.show_title == true ->
            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1], body=#h4{body=[
                    case IconUrl of undefined -> #i{class=[Icon]};
                    Url -> #link{url=Url, body=[#i{class=[Icon]}], data_fields=?DATA_TAB} end,
                    % select all element control
                    if S#feed_state.enable_selection == true ->
                        #span{id=S#feed_state.selectall_ctl, body=[
                            #checkbox{id=S#feed_state.select_all, class=[checkbox, inline], 
                                postback={select, S#feed_state.select_all, State},
                                delegate=S#feed_state.delegate_sel,
                                source=[S#feed_state.select_all],
                                value= string:join([wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))) || E <- Entries], "|"),
                                style= if Total > 0 -> [] ; true-> "display:none;" end}]}; true -> [] end]}},

                #panel{class=[span11], body=#h4{body=[
                    if is_atom(Title) == true -> wf:to_list(Title); true -> Title end,
                    if S#feed_state.enable_traverse == false ->
                        #span{id=S#feed_state.page_label, class=["text-warning"], body=if Current == 0 -> <<" [no entries]">>;
                        true -> " [" ++ integer_to_list(Total) ++ "]" end};
                    true -> [] end,

                    #span{id=S#feed_state.select_toolbar, style="display:none;", class=["selection-ctl"], body=[
                        #link{id=S#feed_state.delete_btn, class=[btn], body=[#i{class=["icon-trash"]}],
                            data_fields=?TOOLTIP, title= <<"delete">>,
                            postback={delete, State}, delegate=feed_ui},
                        SelectionCtl]},

                    if S#feed_state.enable_traverse == true ->
                        #span{class=["pull-right", "traverse-ctl"], body=[
                            #span{id=S#feed_state.feed_toolbar, body=if Total > 0 -> [
                                #small{id=S#feed_state.page_label, body=[
                                    integer_to_list(State#feed_state.start), "-", integer_to_list(Current), " of ", integer_to_list(Total)]},
                                #button{id=S#feed_state.prev,
                                    disabled = element(#iterator.next, First) == undefined,
                                    class=[btn, case element(#iterator.next, First) of undefined -> "disabled"; _ -> "" end],
                                    body=[#i{class=["icon-chevron-left"]}], data_fields=?TOOLTIP, title= <<"previous">>,
                                    postback={traverse, #iterator.next, First, State}, delegate=feed_ui},
                                #button{id=S#feed_state.next,
                                    disabled = element(#iterator.prev, Last) == undefined,
                                    class=[btn, case element(#iterator.prev, Last)  of undefined -> "disabled"; _ -> "" end],
                                    body=[#i{class=["icon-chevron-right"]}], data_fields=?TOOLTIP, title= <<"next">>,
                                    postback={traverse, #iterator.prev, Last, State}, delegate=feed_ui}];
                            true-> [
                                #small{id=S#feed_state.page_label, body=[#span{class=["text-warning"], body= <<" [no entries]">>}]},
                                #i{id=S#feed_state.prev},#i{id=S#feed_state.next} ] end} ]}; true -> [] end,

                    #span{class=["pull-right"], body=[
                        #link{id=S#feed_state.close, class=[close, "text-error"], postback={cancel_select, State}, delegate=feed_ui, body= <<"&times;">>}
                    ]}
                ]}}
            ]}; true -> [] end,

            %% feed body

            if S#feed_state.html_tag == table ->
                #table{class=[table, "table-hover"], header=if S#feed_state.show_header == true -> [TableHeader]; true -> [] end,
                    body=#tbody{id=S#feed_state.entries, class=["feed-body"], body=[#feed_entry{entry=G, state=State} || G <- Entries]}};
                true -> [if S#feed_state.show_header == true -> TableHeader; true -> [] end,
                    #panel{id=S#feed_state.entries, body=[#feed_entry{entry=G, state=State} || G <- Entries]}] end,

            %% footer

            if S#feed_state.enable_traverse == false ->
                #panel{id=S#feed_state.more_toolbar, class=["btn-toolbar", "text-center"], body=
                    if Current < S#feed_state.page_size -> []; 
                    true -> #link{class=?BTN_INFO, body= <<"more">>, delegate=feed_ui, postback = {check_more, Last, State}} end}; true -> [] end
            ]
        end ]});

% feed entry representation

render_element(#feed_entry{entry=E, state=S})->
    error_logger:info_msg("Render ~p with help of ~p", [E, S#feed_state.delegate]),
    Id = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    SelId = ?EN_SEL(Id),
    wf:render(if S#feed_state.html_tag == table ->
        #tr{id=?EN_ROW(Id), cells=[
            if S#feed_state.enable_selection == true ->
                #td{body= [#checkbox{id=SelId,
                    postback={select, SelId, S},
                    delegate=S#feed_state.delegate_sel,
                    source=[SelId], value=Id}]}; true -> [] end,
            #row_entry{entry=E, state=S, module=S#feed_state.delegate}
        ]};
        true -> #panel{id=?EN_ROW(Id), class=["row-fluid", article], body=[
            if S#feed_state.enable_selection == true -> [
                #panel{class=[span1], body=#checkbox{id=SelId, class=["text-center"],
                    postback={select, SelId, S},
                    delegate=S#feed_state.delegate_sel,
                    source=[SelId], value=Id}},
                #panel{class=[span11, "row-fluid"], body= #div_entry{entry=E, state=S, module=S#feed_state.delegate}}];
            true -> #div_entry{entry=E, state=S, module=S#feed_state.delegate} end
        ]} end);

% table rows

render_element(#row_entry{entry=#group{name=Name, description=Desc, scope=Scope}=E, state=#feed_state{}=S}) -> 
    error_logger:info_msg("[feed_ui]Render group ~p ~p", [Name, element(S#feed_state.entry_id, E)]),
    wf:render([
        #td{body=wf:to_list(element(S#feed_state.entry_id, E))},
        #td{body=wf:js_escape(Name)},
        #td{body=wf:js_escape(Desc)},
        #td{body=atom_to_list(Scope)}]);

render_element(#row_entry{entry=#user{email=Email}=U}) -> wf:render([
    #td{body=#link{body=Email, postback={view, Email}}},
    #td{body=[profile:features(wf:user(), U, "icon-2x")]},
    #td{body=case kvs:get(user_status, Email) of {ok,Status} -> product_ui:to_date(Status#user_status.last_login); {error,_}-> "" end}]);

render_element(#row_entry{entry=#product{title=Title, brief=Description}}) -> wf:render([
    #td{body=Title},
    #td{body=Description}]);

render_element(#row_entry{entry=#acl_entry{accessor={user, Accessor}, action=Action}=E, state=S})-> wf:render([
    #td{body= wf:to_list(element(S#feed_state.entry_id, E))},
    #td{body= Accessor},
    #td{body= atom_to_list(Action)}]);

% divs
% product

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=product}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,
%    Groups = [G ||#group_subscription{where=G} <- kvs_group:participate(Id)],
    wf:render(article(product, {Id, UiId}, From, E#entry.created, E#entry.media, E#entry.title, E#entry.description));
render_element(#div_entry{entry=#product{}=P, state=#feed_state{}=State}) ->
    Id = element(State#feed_state.entry_id, P),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
    From = case kvs:get(user, P#product.owner) of {ok, U} -> {P#product.owner, U#user.display_name}; {error, _} -> {P#product.owner,P#product.owner} end,
    Media = case P#product.cover of undefined -> #media{};
    File -> #media{url = File, thumbnail_url = filename:join([filename:dirname(File), "thumbnail", filename:basename(File)])} end,
    wf:render(article(product, {Id, UiId}, From, P#product.created, [Media], P#product.title, P#product.brief));

% review

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,
    wf:render(article(review, {Id, UiId}, From, E#entry.created, E#entry.media, E#entry.title, E#entry.description));

% direct message

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=direct}=State})->
    Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    User = wf:user(),
    From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
    IsAdmin = case User of undefined -> false; Us when Us#user.email==User#user.email -> true; _-> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,

    wf:render([
        #p{body=[#small{body=["[", product_ui:to_date(E#entry.created), "] "]},
            #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
            <<" ">>,
            wf:js_escape(wf:to_list(E#entry.title)),
            case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end ]},
        #p{body= wf:js_escape(E#entry.description)},
        #panel{id=?EN_TOOL(Id), body= case E#entry.type of {feature, _} when IsAdmin ->
            #panel{class=["btn-toolbar"], body=[
                #link{class=[btn, "btn-success"], body= <<"allow">>, postback={allow, E#entry.from, E#entry.entry_id, E#entry.type, State}},
                #link{class=[btn, "btn-info"], body= <<"reject">>, postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type, State}} ]};
        _ -> [] end }]);


% Blog view

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=blog}=State})->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error,_} -> E#entry.from end,

    Entry = #panel{class=["blog-post"], body=[
        #header{class=["blog-header"], body=[
            #h2{body=[#span{id=?EN_TITLE(UiId), body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, 
            #small{body=[<<" by ">>, #link{body=From}, product_ui:to_date(E#entry.created)]}]}]},

        #figure{class=["thumbnail-figure"], body=[
            #carousel{items=[#entry_media{media=Media, mode=blog} || Media <- E#entry.media]},
            if length(E#entry.media) > 1 ->
                #figcaption{class=["thumbnail-title"], body=[#h4{body=#span{body=wf:js_escape(E#entry.title)}}]}; true -> [] end ]},

        #panel{id=?EN_DESC(UiId), body=product_ui:shorten(wf:js_escape(E#entry.description)), data_fields=[{<<"data-html">>, true}]},

        #footer{class=["blog-footer", "row-fluid"], body=[
            #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, 
                #span{class=[badge, "badge-info"], body= <<"...">>} ], postback={read, entry, Id}},
            #link{body=[ #i{class=["icon-comments-alt", "icon-large"]},
                #span{class=[?ID_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))}],
                postback={read, entry, Id}},
            #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, entry, Id}} ]} ]},
    element_panel:render_element(Entry);

% Detached review

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=detached}=State})->
    Eid = element(State#feed_state.entry_id, E),
   {_, Fid} = lists:keyfind(comments, 1, E#entry.feeds),
    Recipients = [{RoutingType, To, {Eid, FeedId, lists:keyfind(comments, 1, Feeds)}}
        || #entry{to={RoutingType, To}, feed_id=FeedId, feeds=Feeds} <- kvs:all_by_index(entry,entry_id, Eid)],

    Is = #input_state{
        recipients=Recipients,
        entry_type = comment,
        show_recipients=false,
        show_title = false,
        show_media = false},

    CmState = ?FD_STATE(Fid)#feed_state{view=comment,  entry_type=comment, entry_id=#comment.comment_id, recipients=Recipients, show_title=false},

    Entry = #panel{class=["blog-post"], body=[
        #h3{class=[blue], body=[#span{id=?EN_TITLE(Eid), body=E#entry.title, data_fields=[{<<"data-html">>, true}]} ]},
        #panel{id=?EN_DESC(Eid), body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

        #panel{class=[comments, "row-fluid"], body=[
            #feed_ui{icon="icon-comments-alt",
                title=[#span{class=[?ID_CM_COUNT(Eid)], body=[integer_to_list(kvs_feed:comments_count(entry, Eid))]}, <<" comments">>],
                state=CmState},
%            #input{title= <<"Add your comment">>, class=["comments-form"], state=Is, feed_state=CmState}
            #input{title= <<"Add your comment">>, class=["comments-form"], state=Is}
       ]}
    ]},
    element_panel:render_element(Entry);

% Comment

render_element(#div_entry{entry=#comment{}=C, state=#feed_state{}=State})->
    Id = element(State#feed_state.entry_id, C),
    {Author, Avatar} = case kvs:get(user, C#comment.from) of 
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,

    Date = product_ui:to_date(C#comment.created),
    {_, Fid} = lists:keyfind(comments, 1, C#comment.feeds),

    Recipients = lists:flatten([case kvs:get(comment, {Id, {E,F}}) of {error,_}-> [];
        {ok, #comment{feeds=Feeds}} ->
            Cs = lists:keyfind(comments, 1, Feeds),
            {R, T, {E, F, Cs}} end ||{R,T,{E,F,_}}<-State#feed_state.recipients]),

    CmState = ?FD_STATE(Fid, State)#feed_state{recipients=Recipients,
        show_title=State#feed_state.show_title andalso not State#feed_state.flat_mode,
        show_header=State#feed_state.show_header andalso not State#feed_state.flat_mode},

    Is = #input_state{
        recipients= Recipients,
        entry_type = comment,
        show_recipients = false,
        show_title = false,
        show_media = false,
        collapsed = true,
        post_collapse = true,
        expand_btn= [<<"reply">>, #i{class=["icon-reply"]}]},

    InnerFeed = #feed_ui{state=CmState, class="comments",  header=[
        #input{state=Is, role=comment, class=["comment-reply"], expand_class=[]}]},

    wf:render([
        #panel{class=[media, "media-comment"], body=[
            #link{class=["pull-left"], body=[Avatar]},
            #panel{class=["media-body"], body=[
                #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
                #p{body= wf:js_escape(C#comment.content)},
                if State#feed_state.flat_mode == true -> []; true -> #p{class=["media-heading"], body=[InnerFeed]} end ]} ]},
        if State#feed_state.flat_mode == true -> InnerFeed; true -> [] end]);

% Media elements

render_element(#entry_media{media=undefined, mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}], alt="no media", class=[]});
render_element(#entry_media{media=[], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt="no media", class=[]});
render_element(#entry_media{media=[#media{thumbnail_url=undefined, title=T}|_], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt=T, class=[]});
render_element(#entry_media{media=undefined, mode=store}) ->
    element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/191x88/text:no media">>}], alt="no media", class=[]});
render_element(#entry_media{media=[#media{}=Media|_], mode=reviews}) -> element_image:render_element(image(Media, "270x124"));
render_element(#entry_media{media=#media{}=Media, mode=blog}) -> element_image:render_element(image(Media, "716x480"));
render_element(#entry_media{media=#media{}=Media, mode=store}) -> element_image:render_element(image(Media, "270x124"));
render_element(#entry_media{}) -> element_panel:render_element(#panel{body=[]});

render_element(E) -> error_logger:info_msg("[feed_ui]render_element(#unknown{}) ~p", [E]).

% product entry components

article(Type, {Id, UiId}, {FromId, From}, Date, Media, Title, Description, Ctl)-> [
    #panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue], body= <<"">>},
        #p{class=[username], body= #link{body=From, url= "/profile?id="++wf:to_list(FromId)}},
        #p{class=[datestamp], body=[ #span{body= product_ui:to_date(Date)} ]},
        #p{class=[statistics], body=[
            #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[], body= <<"...">>} ]},
            #link{url="#",body=[ #i{class=["icon-comment-alt", "icon-large"]},
                #span{class=[?ID_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))} ]} ]} ]},

    #panel{id=?EN_MEDIA(UiId), class=[span4, "media-pic"], body = #entry_media{media=Media, mode=reviews}},

    #panel{class=[span5, "article-text"], body=[
        #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=wf:js_escape(Title)}},
        #p{id=?EN_DESC(UiId), body=wf:js_escape(product_ui:shorten(Description))},
        #panel{id=?EN_TOOL(UiId), class=[more], body=[
            Ctl,
            #link{body=[<<"view ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, Type, Id}}]} ]}].

article(Type, {Id, UiId}, {FromId, From}, Date, Media, Title, Description)->
    article(Type, {Id, UiId}, {FromId, From}, Date, Media, Title, Description, []).

image(#media{}=Media, Size) ->
    Thumb = Media#media.thumbnail_url,
    Ext = filename:extension(Thumb),
    Name = filename:basename(Thumb, Ext),
    Dir = filename:dirname(Thumb),
    #image{alt=Media#media.title, image=filename:join([Dir, Name++"_"++wf:to_list(Size)++Ext])}.

% events

event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({traverse, Direction, Start, #feed_state{}=S}) -> traverse(Direction, Start, S);

event({delete, #feed_state{selected_key=Selected, visible_key=Visible}=S}) ->
    Selection = sets:from_list(wf:session(Selected)),
    error_logger:info_msg("Selection~p", [Selection]),
    V = wf:session(Visible),
    error_logger:info_msg("Visible: ~p", [V]),
    User = wf:user(),
    [begin
        Type = case S#feed_state.view of product -> product; _ -> S#feed_state.entry_type end,
        error_logger:info_msg("Delete ~p", [Id]),

        case kvs:get(Type, Id) of {error,_} -> error_logger:info_msg("No object");
        {ok, Obj} ->
            case Type of
                product ->  msg:notify([kvs_product, User#user.email, delete], [Obj]);
                group ->    msg:notify([kvs_group, User#user.email, delete], [Obj]);
                entry ->
                    {Eid,_} = Id,
                    R1 = if S#feed_state.del_by_index == true ->
                        [{RoutingType, To, {somefeed, Fid}} || #entry{feed_id=Fid, to={RoutingType, To}}
                            <- kvs:all_by_index(entry, entry_id, Eid)];
                        true -> {RoutingType, To} = Obj#entry.to, [{RoutingType, To, {feed, Obj#entry.feed_id}}] end,
                    R2 = [{user, user, {feed, entries}}],
                    [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete],
                        [Obj#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid}, #input_state{}, ?FD_STATE(Fid, S)])
                        || {RouteType, To, {_, Fid}} <- lists:flatten([R1,R2])];
                _ -> error_logger:info_msg("delete ~p. no recipients", [Type]), [] end end
       end || Id <- wf:session(Visible), sets:is_element(wf:to_list(erlang:phash2(Id)), Selection)];

event({cancel_select, #feed_state{}=S}) -> deselect(S);

event({select, Sel, #feed_state{selected_key=Key}=S})->
    Selection = wf:session(Key),
    NewSel = case wf:q(Sel) of "undefined" -> if Sel == S#feed_state.select_all -> sets:new(); true ->
        SelEn = ?EN_FROMSEL(Sel),
        wf:wire(#jq{target=?EN_ROW(SelEn), method=["removeClass"], args=["'warning'"]}),
        sets:from_list(Selection--[SelEn]) end;
    Val -> Vals = string:tokens(Val,"|"),
        [begin
            wf:wire(#jq{target=?EN_SEL(C), method=["prop"],     args=["'checked', 'checked'"]}),
            wf:wire(#jq{target=?EN_ROW(C), method=["addClass"], args=["'warning'"]})
        end || C <- Vals],
        sets:from_list(Vals++Selection) end,

    case sets:size(NewSel) of 0 -> deselect(S);
    Size ->
        wf:wire(#jq{target=S#feed_state.select_toolbar, method=["show"]}),
        wf:wire(#jq{target=S#feed_state.close,          method=["show"]}),
        wf:wire(#jq{target=S#feed_state.feed_title,     method=["attr"], args=["'style', 'background-color:lightblue'"]}),
        wf:wire(#jq{target=S#feed_state.feed_toolbar,   method=["hide"]}),
        wf:wire(#jq{target=S#feed_state.select_all,     method=["prop"], args=["'checked'," ++
            if Size == S#feed_state.page_size orelse Size == S#feed_state.current -> "'checked'"; true -> "false" end]})
    end,

    wf:session(Key, sets:to_list(NewSel));

event({check_more, Start, #feed_state{}=S}) ->
    traverse_entries(S#feed_state.entry_type, element(#iterator.prev,Start), S#feed_state.page_size, S),
    wf:update(S#feed_state.more_toolbar, []);

event(E)-> error_logger:info_msg("[feed] event: ~p", [E]).

deselect(#feed_state{selected_key=Key}=S) ->
    [begin
        wf:wire(#jq{target=?EN_SEL(C), method=["prop"], args=["'checked', false"]}),
        wf:wire(#jq{target=?EN_ROW(C), method=["removeClass"], args=["'warning'"]})
     end|| C <- case wf:session(Key) of undefined -> []; Ims -> Ims end],
    SA = if is_tuple(S#feed_state.container_id) -> ?FD_SELALL(erlang:phash2(S#feed_state.container_id)); true -> S#feed_state.select_all end,
    wf:wire(#jq{target=SA, method=["prop"], args=["'checked', false"]}),
    wf:wire(#jq{target=S#feed_state.select_toolbar, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_toolbar, method=["show"]}),
    wf:wire(#jq{target=S#feed_state.close, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_title, method=["attr"], args=["'style', 'background-color:none;'"]}),
    wf:session(Key, []).

% Traverse

traverse(Direction, Start, #feed_state{}=S)->
    {ok, Container} = kvs:get(S#feed_state.container, S#feed_state.container_id),
    Top = element(#container.top, Container),
    Entries = case element(Direction, Start) of
        undefined  -> kvs:entries(Container, S#feed_state.entry_type, S#feed_state.page_size);
        Prev -> kvs:entries(S#feed_state.entry_type, Prev, S#feed_state.page_size, Direction)
    end,

    wf:session(S#feed_state.visible_key, [element(S#feed_state.entry_id, E)|| E<-Entries]),

    {NewLast, NewFirst} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,
    Total = element(#container.entries_count, Container),
    Current = length(Entries),

    NewStart = case Direction of
        #iterator.prev -> S#feed_state.start + S#feed_state.page_size;
        #iterator.next -> if Top==element(#iterator.id, Start) -> 1; true -> S#feed_state.start-S#feed_state.page_size end end,

    State = S#feed_state{
        start=NewStart,
        start_element=NewFirst,
        last_element=NewLast,
        current=Current},

    wf:update(S#feed_state.entries, [#feed_entry{entry=G, state=State} || G <- Entries]),

    if Total == 0 ->
        wf:replace(S#feed_state.prev, #i{id=S#feed_state.prev}),
        wf:replace(S#feed_state.next, #i{id=S#feed_state.next});
    true ->
        wf:replace(S#feed_state.prev, #button{id=State#feed_state.prev,
            disabled = element(#iterator.next, NewFirst) == undefined,
            class=[btn, case element(#iterator.next, NewFirst) of undefined -> "disabled"; _ -> "" end],
            body=[#i{class=["icon-chevron-left"]}], data_fields=?TOOLTIP, title= <<"previous">>,
            postback={traverse, #iterator.next, NewFirst, State}, delegate=feed_ui}),
        wf:replace(S#feed_state.next, #button{id=State#feed_state.next,
            disabled = element(#iterator.prev, NewLast) == undefined,
            class=[btn, case element(#iterator.prev, NewLast) of undefined -> "disabled"; _ -> "" end],
            body=[#i{class=["icon-chevron-right"]}], data_fields=?TOOLTIP, title= <<"next">>,
            postback={traverse, #iterator.prev, NewLast, State}, delegate=feed_ui}) end,

    wf:update(S#feed_state.page_label, [
        if Total == 0 -> #small{class=["text-warning"],body= <<"[no entries]">>};
        true-> [integer_to_list(NewStart), "-", integer_to_list(NewStart+Current-1), " of ", integer_to_list(Total)] end
    ]),

    if State#feed_state.enable_selection == true ->
        wf:update(S#feed_state.selectall_ctl,
        #checkbox{id=State#feed_state.select_all, class=[checkbox, inline],
            postback={select, State#feed_state.select_all, State}, 
            delegate=S#feed_state.delegate_sel,
            source=[State#feed_state.select_all],
            value = string:join([wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))) || E <- Entries], "|"),
            style=if Total > 0 -> [] ; true-> "display:none;" end}); true -> [] end,
    wf:replace(State#feed_state.delete_btn,
        #link{id=S#feed_state.delete_btn, class=[btn], body=[#i{class=["icon-trash"]}],
            data_fields=?TOOLTIP, title= <<"delete">>,
            postback={delete, State}, delegate=feed_ui}),
    wf:wire("Holder.run();").

traverse_entries(_,undefined,_, #feed_state{more_toolbar=BtnId}) -> self() ! {delivery, [somepath, no_more], [BtnId]}, [];
traverse_entries(_,_,0,_) -> [];
traverse_entries(RecordName, Next, Count, S)->
    case kvs:get(RecordName, Next) of {error, not_found} -> [];
    {ok, R}-> self() ! {delivery, [somepath, show_entry], [R, S]}, [R | traverse_entries(RecordName, element(#iterator.prev, R), Count-1, S)] end.

process_delivery([Type, Id, created], [{error,E}]) -> 
    error_logger:info_msg("~p ~p create failed: ~p", [Type, Id, E]);
process_delivery([Type, Id, created], [P]) ->
    error_logger:info_msg("[feed_ui] ~p created ~p", [Type, Id]),
    case feed_state(?FEED(Type)) of #feed_state{} = S ->
        error_logger:info_msg("Delegate: ~p", [S#feed_state.delegate]),
        deselect(S),
        wf:session(medias, []),
        PrevVisible = wf:session(S#feed_state.visible_key),
        wf:session(S#feed_state.visible_key, [element(S#feed_state.entry_id, P) | PrevVisible]),

        if S#feed_state.enable_traverse ->
            error_logger:info_msg("TRAVERSE"),
            traverse(#iterator.next, P, S);
        true ->
            error_logger:info_msg("INSERT"),
            wf:insert_top(S#feed_state.entries, #feed_entry{entry=P, state=S}) end,
        wf:wire("Holder.run();");

    _ -> skip end,

    case input_state(?FEED(Type)) of #input_state{}=Is -> input:event({clear_input, Is}); _ -> skip end;

process_delivery([entry, {Id, Fid}, added], [#entry{}=E]) ->
    case feed_state(Fid) of #feed_state{}=S ->
        deselect(S),
        wf:session(medias, []),
        PrevVisible = wf:session(S#feed_state.visible_key),
        wf:session(S#feed_state.visible_key, [element(S#feed_state.entry_id, E) | PrevVisible]),

        if S#feed_state.enable_traverse ->
            error_logger:info_msg("TRAVERSE"),
            traverse(#iterator.next, E, S);
        true ->
            error_logger:info_msg("INSERT"),
            wf:insert_top(S#feed_state.entries, #feed_entry{entry=E, state=S}) end,

        wf:wire("Holder.run();");
    _-> skip end,

    case input_state(Fid) of #input_state{}=Is -> input:event({clear_input, Is}); _ -> skip end;

process_delivery([product, Id, updated], [{error,E}, #input_state{}=Is,_]) ->
    wf:update(Is#input_state.alert_id, index:error(E));
process_delivery([product, Id, updated], [#product{}=P, #input_state{}=Is])->
    wf:update(Is#input_state.alert_id, index:success("updated")),
    case feed_state(?FEED(product)) of #feed_state{}=S ->
        UiId = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, P))),
        wf:session(medias, []),
        wf:replace(?EN_ROW(UiId), #feed_entry{entry=P, state=S}),
        wf:wire("Holder.run();");
    _ -> skip
    end;
process_delivery([entry, {_,Fid}, updated], [#entry{}=E])->
    case feed_state(Fid) of #feed_state{}=S ->
        UiId = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
        wf:replace(?EN_ROW(UiId), #feed_entry{entry=E, state=S}),
        wf:wire("Holder.run();");
    _-> skip end;
process_delivery([entry, {_,Fid}, deleted], [#entry{}=E])->
    case feed_state(Fid) of #feed_state{}=S ->
        UiId = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
        error_logger:info_msg("uid:~p", [UiId]),
        wf:remove(?EN_ROW(UiId)),
        deselect(S);
    _-> skip
    end,
    ok;

process_delivery([Type, Id, deleted], [{error, E}]) -> ok;

process_delivery([Type, Id, deleted], [P]) ->
    case feed_state(?FEED(Type)) of #feed_state{} = S ->
        UiId = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, P))),
        wf:remove(?EN_ROW(UiId)),
        deselect(S); _ -> skip end;

process_delivery([show_entry], [Entry, #feed_state{} = S]) ->
    error_logger:info_msg("~n[feed] show_entry ~p", [element(#iterator.id, Entry)]),
    wf:insert_bottom(S#feed_state.entries, #feed_entry{entry=Entry, state=S}),
    wf:wire("Holder.run();"),
    wf:update(S#feed_state.more_toolbar, #link{class=?BTN_INFO, body= <<"more">>, delegate=feed_ui, postback={check_more, Entry, S}});

process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery(_,_) ->
    %error_logger:info_msg("[feed_ui]unhandled route:~p", [R]),
    ok.
