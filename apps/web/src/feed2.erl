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

render_element(#feed2{title=Title, icon=Icon, class=Class, header=TableHeader, state=S, selection_ctl=SelectionCtl}) ->
    wf:render(#section{class=[feed, Class], body=[
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_}->
            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1],  body=#h4{body=[#i{class=[Icon]}]}},
                #panel{class=[span11], body=#h4{body=[wf:to_list(Title), #span{class=["text-warning"], body= <<" [no feed]">>}]}}]};
        {ok, Feed} ->
            wf:session(S#feed_state.selected_key,[]),
            Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),
            Total = element(#container.entries_count, Feed),
            Current = length(Entries),
            {Last, First} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,
            State = S#feed_state{start_element = First, last_element = Last, start = 1, total = Total, current = Current},
            [

            %% header

            #panel{id=S#feed_state.feed_title, class=["row-fluid", "feed-title", Class], body=[
                #panel{class=[span1], body=#h4{body=[
                    #i{class=[Icon, blue]},

                    % select all element control
                    if S#feed_state.enable_selection == true ->
                        #span{id=S#feed_state.selectall_ctl, body=[
                            #checkbox{id=S#feed_state.select_all, class=[checkbox, inline], postback={select, S#feed_state.select_all, State},
                                delegate=feed2, source=[S#feed_state.select_all],
                                value= string:join([wf:to_list(element(S#feed_state.entry_id, E)) || E <- Entries], "|"),
                                style= if Total > 0 -> [] ; true-> "display:none;" end}]}; true -> [] end]}},

                #panel{class=[span11], body=#h4{body=[
                    wf:to_list(Title),
                    #span{class=["text-warning"], body=if Current == 0 -> <<" [no entries]">>; true -> [] end},

                    #span{id=S#feed_state.select_toolbar, style="display:none;", class=["selection-ctl"], body=[
                        #link{id=S#feed_state.delete_btn, class=[btn], body=[<<"delete">>], postback={delete, State}, delegate=feed2},
                        SelectionCtl]},

                    if S#feed_state.enable_traverse == true ->
                        #span{class=["pull-right", "traverse-ctl"], body=[
                            #span{id=S#feed_state.feed_toolbar, body=if Total > 0 -> [
                                #small{id=S#feed_state.page_label, body=[
                                    integer_to_list(State#feed_state.start), "-", integer_to_list(Current), " of ", integer_to_list(Total)]},
                                #link{id=S#feed_state.prev, class=[btn, case element(#iterator.next, First) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>],
                                    postback={traverse, #iterator.next, First, State}, delegate=feed2},
                                #link{id=S#feed_state.next, class=[btn, case element(#iterator.prev, Last)  of undefined -> "disabled"; _ -> "" end], body=[<<">">>],
                                    postback={traverse, #iterator.prev, Last, State}, delegate=feed2}]; true-> [] end} ]}; true -> [] end,

                    #span{class=["pull-right"], body=[
                        #link{id=S#feed_state.close, class=[close, "text-error"], postback={cancel_select, State}, delegate=feed2, body= <<"&times;">>}
                    ]}
                ]}}
            ]},

            %% feed body

            if S#feed_state.html_tag == table ->
                #table{class=[table, "table-hover"], header=[TableHeader], body=
                    #tbody{id=S#feed_state.entries, class=["feed-body"], body=[#feed_entry2{entry=G, state=State} || G <- Entries]}};
                true -> [TableHeader, #panel{id=S#feed_state.entries, body=[#feed_entry2{entry=G, state=State} || G <- Entries]}] end,

            %% footer

            if S#feed_state.enable_traverse == false ->
                #panel{id=S#feed_state.more_toolbar, class=["btn-toolbar", "text-center"], body=
                    if Current < S#feed_state.page_size -> []; 
                    true -> #link{class=?BTN_INFO, body= <<"more">>, delegate=feed2, postback = {check_more, Last, State}} end}; true -> [] end
            ]
        end ]});

% feed entry representation

render_element(#feed_entry2{entry=E, state=S})->
    Id = wf:to_list(element(S#feed_state.entry_id, E)),
    SelId = ?EN_SEL(Id),
    wf:render(if S#feed_state.html_tag == table ->
        #tr{id=?EN_ROW(Id), cells=[
            if S#feed_state.enable_selection == true ->
                #td{body= [#checkbox{id=SelId, postback={select, SelId, S}, delegate=feed2, source=[SelId], value=Id}]}; true -> [] end,
            #row_entry{entry=E, state=S}
        ]};
        true -> #panel{id=?EN_ROW(wf:to_list(Id)), class=["row-fluid", article], body=[
            if S#feed_state.enable_selection == true -> [
                #panel{class=[span1], body=#checkbox{id=SelId, class=["text-center"], postback={select, SelId, S}, delegate=feed2, source=[SelId], value=Id}},
                #panel{class=[span11, "row-fluid"], body= #div_entry{entry=E, state=S}}];
            true -> #div_entry{entry=E, state=S} end
        ]} end);

% table rows

render_element(#row_entry{entry=#group{name=Name, description=Desc, scope=Scope}=E, state=#feed_state{}=S}) -> wf:render([
        #td{body=wf:to_list(element(S#feed_state.entry_id, E))},
        #td{body=wf:js_escape(Name)},
        #td{body=wf:js_escape(Desc)},
        #td{body=atom_to_list(Scope)}]);

render_element(#row_entry{entry=#user{email=Email}=U, state=S}) -> wf:render([
    #td{body=#link{body=Email, postback={view, Email}}},
    #td{body=[profile:features(wf:user(), U, "icon-2x")]},
    #td{body=case kvs:get(user_status, Email) of {ok,Status} -> product_ui:to_date(Status#user_status.last_login); {error,_}-> "" end}]);

render_element(#row_entry{entry=#product{title=Title, brief=Description}, state=S}) -> wf:render([
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
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
%    Groups = [G ||#group_subscription{where=G} <- kvs_group:participate(Id)],
    wf:render(article(Id, From, E#entry.created, E#entry.media, E#entry.title, E#entry.description));
render_element(#div_entry{entry=#product{}=P, state=#feed_state{}=State}) ->
    Id = element(State#feed_state.entry_id, P),
    From = case kvs:get(user, P#product.owner) of {ok, U} -> U#user.display_name; {error, _} -> P#product.owner end,
    Media = case P#product.cover of undefined -> #media{};
    File -> #media{url = File, thumbnail_url = filename:join([filename:dirname(File), "thumbnail", filename:basename(File)])} end,
    wf:render(article(Id, From, P#product.created, [Media], P#product.title, P#product.brief));

% review

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
    wf:render(article(Id, From, E#entry.created, E#entry.media, E#entry.title, E#entry.description));

% direct message

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=direct}=State})->
    User = wf:user(),
    Id = element(State#feed_state.entry_id, E),
    From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
    IsAdmin = case User of undefined -> false; Us when Us#user.email==User#user.email -> true; _-> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,

    wf:render([
        #p{body=[#small{body=["[", product_ui:to_date(E#entry.created), "] "]},
            #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
            <<" ">>,
            wf:js_escape(wf:to_list(E#entry.title)),
            case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end ]},
        #p{body= wf:js_escape(E#entry.description)},
        #panel{id=?ID_TOOL(Id), body= case E#entry.type of {feature, _} when IsAdmin ->
            #panel{class=["btn-toolbar"], body=[
                #link{class=[btn, "btn-success"], body= <<"allow">>, postback={allow, E#entry.from, E#entry.entry_id, E#entry.type, State}},
                #link{class=[btn, "btn-info"], body= <<"reject">>, postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type, State}} ]};
        _ -> [] end }]);


% Blog view

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=blog}=State})->
    Id = element(State#feed_state.entry_id, E),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error,_} -> E#entry.from end,

    Entry = #panel{class=["blog-post"], body=[
        #header{class=["blog-header"], body=[
            #h2{body=[#span{id=?EN_TITLE(Id), body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, 
            #small{body=[<<" by ">>, #link{body=From}, product_ui:to_date(E#entry.created)]}]}
        ]},
        #figure{class=["thumbnail-figure"], body=[
            #carousel{items=[#entry_media{media=Media, mode=blog} || Media <- E#entry.media]},
            if length(E#entry.media) > 1 ->
                #figcaption{class=["thumbnail-title"], body=[#h4{body=#span{body=E#entry.title}}]}; true -> [] end
        ]},
        #panel{id=?EN_DESC(Id), body=product_ui:shorten(E#entry.description), data_fields=[{<<"data-html">>, true}]},

        #footer{class=["blog-footer", "row-fluid"], body=[
            #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, 
                #span{class=[badge, "badge-info"], body= <<"...">>} ], postback={read, entry, Id}},
            #link{body=[ #i{class=["icon-comments-alt", "icon-large"]},
                #span{class=[?ID_CM_COUNT(Id)], body=integer_to_list(kvs_feed:comments_count(entry, Id))}],
                postback={read, entry, Id}},
            #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, entry, Id}}
        ]}
    ]},
    element_panel:render_element(Entry);


% Detached review

render_element(#feed_entry2{entry=#entry{}=E, state=#feed_state{view=detached}=State})->
    Eid = element(State#feed_state.entry_id, E),
   {_, Fid} = lists:keyfind(comments, 1, E#entry.feeds),
    Ms = E#entry.media,
    Dir = "static/"++case wf:user() of undefined->"anonymous"; User-> User#user.email end,
    error_logger:info_msg("Entry feed: ~p", [E#entry.feed_id]),
    error_logger:info_msg("Container id for this entry ~p", [State#feed_state.container_id]),

    Recipients = [{RoutingType, To, {Eid, FeedId, lists:keyfind(comments, 1, Feeds)}}
        || #entry{to={RoutingType, To}, feed_id=FeedId, feeds=Feeds} <- kvs:all_by_index(entry,entry_id, Eid)],

    Is = #input_state{
        recipients=Recipients,
        entry_type = comment,
        show_recipients=false,
        show_title = false,
        show_media = false},

    CmState = ?FD_STATE(Fid)#feed_state{view=comment,  entry_type=comment, html_tag=panel, entry_id=#comment.comment_id, recipients=Recipients},

    error_logger:info_msg("Detached view recippients: ~p", [Is#input_state.recipients]),
    Entry = #panel{id=?EN_ROW(Eid), class=["blog-post"], body=[
        #h3{class=[blue], body=[#span{id=?EN_TITLE(Eid), body=E#entry.title, data_fields=[{<<"data-html">>, true}]} ]},

%        #panel{id=?EN_MEDIA(Eid), class=[span4, "media-pic"], body = #entry_media{media=Media, mode=reviews}},
        #figure{class=["thumbnail-figure"], body=[
          [#entry_media{media=M} || M <- Ms]
        ]},
        #panel{id=?EN_DESC(Eid), body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

        #panel{class=[comments, "row-fluid"], body=[
            #feed2{title=[#span{class=[?ID_CM_COUNT(Eid)], body=[list_to_binary(integer_to_list(kvs_feed:comments_count(entry, E#entry.id)))]},<<" comments">>], state=CmState},
            #input{title= <<"Add your comment">>, class=["comments-form"], state=Is, feed_state=CmState}
       ]}
    ]},
    element_panel:render_element(Entry);

% Comment

render_element(#feed_entry2{entry=#comment{}=C, state=#feed_state{}=State})->
    Id = element(State#feed_state.entry_id, C),
    {Author, Avatar} = case kvs:get(user, C#comment.from) of 
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,

    Date = product_ui:to_date(C#comment.created),
    {_, Fid} = lists:keyfind(comments, 1, C#comment.feeds),
    error_logger:info_msg("Received recipients: ~p~n", [State#feed_state.recipients]),
    
    Recipients = lists:flatten([
        begin
        error_logger:info_msg("We handle E:~p EF:~p", [E,F]),
        case kvs:get(comment, {Id, {E,F}}) of {error,_}-> [];
            {ok, #comment{feeds=Feeds}=PC} ->
                error_logger:info_msg("Comment: ~p", [PC]),
                error_logger:info_msg("Feeds of this comment ~p", [PC#comment.feeds]),
                error_logger:info_msg("This comment in recipient exist: ~p", [PC#comment.comment_id]),
                Cs = lists:keyfind(comments, 1, Feeds),
                {R, T, {E, F, Cs}}
        end
        %{ok, C1} = kvs:get(comment, {C#comment.comment_id, {E,F}}),
%        {R,T,{E,F, lists:keyfind(comments, 1, C#comment.feeds) }}
        end ||{R,T,{E,F,Ci}}<-State#feed_state.recipients]),
    error_logger:info_msg("New recipients ~p", [Recipients]),

    CmState = ?FD_STATE(Fid)#feed_state{view=comment,  entry_type=comment, html_tag=panel, entry_id=#comment.comment_id},

    Is = #input_state{
        recipients= Recipients,
        entry_type = comment,
        show_recipients = false,
        show_title = false,
        show_media = false},

    Comment = #panel{id=?EN_ROW(Id), class=[media, "media-comment"], body=[
        #link{class=["pull-left"], body=[Avatar]},
        #panel{class=["media-body"], body=[
            #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
            #p{body= C#comment.content},
%            #p{body= [#entry_media{media=M, fid=Fid, cid = Cid} ||  M <- C#comment.media]},
            #p{class=["media-heading"], body=[
                #feed2{state=CmState, class="comments",  header=[
                    #input{state=Is,
                        feed_state=CmState,
                        collapsed=true,
                        role=comment, class=["comment-reply"], expand_btn= [<<"reply">>, #i{class=["icon-reply"]}], expand_class=[]}
                ]}
            ]}
        ]}
    ]},
    element_panel:render_element(Comment);

% Media elements

render_element(#entry_media{media=undefined, mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}], alt="no media", class=[]});
render_element(#entry_media{media=[], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt="no media", class=[]});
render_element(#entry_media{media=[#media{thumbnail_url=undefined, title=T}|_], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt=T, class=[]});
render_element(#entry_media{media=[#media{title=Title, thumbnail_url=Thumb}|_], mode=reviews}) ->
  Ext = filename:extension(Thumb),
  Name = filename:basename(Thumb, Ext),
  Dir = filename:dirname(Thumb),
  element_image:render_element(#image{alt=Title, image=filename:join([Dir, Name++"_270x124"++Ext])});
render_element(#entry_media{media=#media{}=Media, mode=blog}) ->
    Thumb = Media#media.thumbnail_url,
    Ext = filename:extension(Thumb),
    Name = filename:basename(Thumb, Ext),
    Dir = filename:dirname(Thumb),
    element_image:render_element(#image{alt=Media#media.title, image=filename:join([Dir, Name++"_716x480"++Ext])});
render_element(#entry_media{media=Media}) ->
  element_panel:render_element(#panel{body=[]});

render_element(E) -> error_logger:info_msg("[feed2]render_element(#unknown{}) ~p", [E]).

% product entry components

article(Id, From, Date, Media, Title, Description)-> [
    #panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue], body= <<"">>},
        #p{class=[username], body= #link{body=From, url= "/profile?id="++wf:to_list(From)}},
        #p{class=[datestamp], body=[ #span{body= product_ui:to_date(Date)} ]},
        #p{class=[statistics], body=[
            #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[], body= <<"...">>} ]},
            #link{url="#",body=[ #i{class=["icon-comment-alt", "icon-large"]},
                #span{class=[?ID_CM_COUNT(Id)], body=integer_to_list(kvs_feed:comments_count(entry, Id))} ]} ]} ]},

    #panel{id=?EN_MEDIA(Id), class=[span4, "media-pic"], body = #entry_media{media=Media, mode=reviews}},

    #panel{class=[span5, "article-text"], body=[
        #h3{body=#span{id=?EN_TITLE(Id), class=[title], body=Title}},
        #p{id=?EN_DESC(Id), body=product_ui:shorten(Description)},
        #panel{id=?ID_TOOL(Id), class=[more], body= [
            #link{body= [#i{class=["icon-edit", "icon-large"]},<<"edit">>], postback={edit_product, Id}},
            #link{body=[<<"view ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, product, Id}} ]} ]}].

% events

event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({traverse, Direction, Start, #feed_state{}=S}) -> traverse(Direction, Start, S);

event({delete, #feed_state{selected_key=Key}=S}) ->
    User = wf:user(),
    [begin
        error_logger:info_msg("Session key: ~p", [Id]),
        Type = case S#feed_state.view of product -> product; _ -> S#feed_state.entry_type end,
        FullId = case Type of entry -> {Id, S#feed_state.container_id}; _ -> Id end,

        error_logger:info_msg("Delete ~p", [FullId]),

        case kvs:get(Type, FullId) of {error,_} -> error_logger:info_msg("No object");
        {ok, Obj} ->
            case Type of
                product ->
                    Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(FullId)],
                    R1 = [{user, User#user.email, lists:keyfind(products, 1, User#user.feeds)}],
                    R2 = [{product, Id, {products, ?FEED(product)}}],
                    R3 = [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Groups],
                    msg:notify([kvs_product, Type, unregister], [Obj, #input_state{recipients=lists:flatten([R1,R2,R3])}, S]);

                entry ->
                    R1 = [{RoutingType, To, {somefeed, Fid}} || #entry{feed_id=Fid, to={RoutingType, To}} <-kvs:all_by_index(entry, entry_id, Id)],
                    R2 = [{user, user, {feed, entries}}],
                    [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete],
                        [Obj#entry{id={Id, Fid}, entry_id=Id, feed_id=Fid}, #input_state{}, ?FD_STATE(Fid, S)#feed_state{}])
                        || {RouteType, To, {_, Fid}} <- lists:flatten([R1,R2])];
                group ->
                    msg:notify([kvs_group, Type, unregister], [Obj, #input_state{recipients=[{group, FullId, {feed, ?GRP_FEED}}]}, S]);
                _ -> error_logger:info_msg("delete ~p. no recipients", [Type]), [] end end

       end || Id <- wf:session(Key)];

event({cancel_select, #feed_state{}=S}) -> deselect(S);

event({select, Sel, #feed_state{selected_key=Key}=S})->
    error_logger:info_msg("Select ~p, select all: ~p", [Sel, S#feed_state.select_all]),
    Selection = wf:session(Key),
    error_logger:info_msg("Current selection: ~p", [Selection]),
    NewSel = case wf:q(Sel) of "undefined" -> if Sel == S#feed_state.select_all -> sets:new(); true ->
        PrevSel = lists:sublist(Sel,1, length(Sel)-3), % fixme:
        error_logger:info_msg("Deselect: ~p", [PrevSel]),
        wf:wire(#jq{target=?EN_ROW(PrevSel), method=["removeClass"], args=["'warning'"]}),
        sets:from_list(Selection--[PrevSel]) end;
    Val -> Vals = string:tokens(Val,"|"),
        error_logger:info_msg("select: ~p", [Vals]),
        [begin
            wf:wire(#jq{target=C, method=["prop"], args=["'checked', 'checked'"]}),
            wf:wire(#jq{target=?EN_ROW(C), method=["addClass"], args=["'warning'"]})
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
            error_logger:info_msg("Check select all"),
            wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', 'checked'"]});
        true ->
            error_logger:info_msg("Uncheck select all"),
            wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', false"]}) end
    end,
    wf:session(Key, sets:to_list(NewSel));

event({check_more, Start, #feed_state{}=S}) ->
    traverse_entries(S#feed_state.entry_type, element(#iterator.prev,Start), S#feed_state.page_size, S),
    wf:update(S#feed_state.more_toolbar, []);

event(E)-> error_logger:info_msg("[feed2] event: ~p", [E]).

deselect(#feed_state{selected_key=Key}=S) ->
    [wf:wire(#jq{target=C, method=["prop"], args=["'checked', false"]}) || C <- wf:session(Key)],
    [wf:wire(#jq{target=?EN_ROW(C), method=["removeClass"], args=["'warning'"]}) || C <- wf:session(Key)],
    wf:wire(#jq{target=S#feed_state.select_all, method=["prop"], args=["'checked', false"]}),
    wf:wire(#jq{target=S#feed_state.select_toolbar, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_toolbar, method=["show"]}),
    wf:wire(#jq{target=S#feed_state.close, method=["hide"]}),
    wf:wire(#jq{target=S#feed_state.feed_title, method=["attr"], args=["'style', 'background-color:none;'"]}),
    wf:session(Key, []).

traverse(Direction, Start, #feed_state{}=S)->
    error_logger:info_msg("=> Traverse ~p from ~p", [Direction ,Start]),
    Entries = case element(Direction, Start) of
        undefined  -> kvs:entries(kvs:get(S#feed_state.container, S#feed_state.container_id), S#feed_state.entry_type, S#feed_state.page_size);
        Prev -> kvs:entries(S#feed_state.entry_type, Prev, S#feed_state.page_size, Direction)
    end,
%    error_logger:info_msg("Traverse: ~p", [Entries]),
    {NewLast, NewFirst} = case Entries of [] -> {#iterator{},#iterator{}}; E  -> {lists:last(E), lists:nth(1,E)} end,

    Total = S#feed_state.total,
    Current = length(Entries),
    NewStart = case Direction of #iterator.prev -> S#feed_state.start+S#feed_state.page_size; #iterator.next -> S#feed_state.start-S#feed_state.page_size end,

    State = S#feed_state{start=NewStart, start_element=NewFirst, last_element=NewLast, current=Current},
    error_logger:info_msg("Entries: ~p", [S#feed_state.entries]),
    wf:update(S#feed_state.entries,
%        wf_tags:emit_tag(<<"thead">>, wf:render([#tr{class=["feed-table-header"], cells=[
%            #th{body= <<"">>},
%            #th{body= <<"id">>},
%            #th{body= <<"name">>},
%            #th{body= <<"description">>},
%            #th{body= <<"scope">>}]}]), []),

        [#feed_entry2{entry=G, state=State} || G <- Entries]),

    wf:replace(S#feed_state.prev, #link{id=State#feed_state.prev, class=[btn, case element(#iterator.next, NewFirst) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>], 
        postback={traverse, #iterator.next, NewFirst, State}, delegate=feed2}),
    wf:replace(S#feed_state.next, #link{id=State#feed_state.next, class=[btn, case element(#iterator.prev, NewLast) of undefined -> "disabled"; _ -> "" end], body=[<<">">>], 
        postback={traverse, #iterator.prev, NewLast, State}, delegate=feed2}),

    wf:update(S#feed_state.page_label, [integer_to_list(NewStart), "-", integer_to_list(NewStart+Current-1), " of ", integer_to_list(Total)]),

    if State#feed_state.selection == true ->
        wf:update(S#feed_state.selectall_ctl,
        #checkbox{id=State#feed_state.select_all, class=[checkbox, inline], postback={select, State#feed_state.select_all, State}, delegate=feed2,
            source=[State#feed_state.select_all], 
            value = string:join([wf:to_list(element(State#feed_state.entry_id, E)) || E <- Entries], "|"),
            style=if Total > 0 -> [] ; true-> "display:none;" end}); true -> [] end,
    wf:replace(State#feed_state.delete_btn, #link{id=State#feed_state.delete_btn, class=[btn], body=[<<"delete">>], postback={delete, State}, delegate=feed2}),
    wf:wire("Holder.run();").

traverse_entries(_,undefined,_, #feed_state{more_toolbar=BtnId}) -> self() ! {delivery, [somepath, no_more], [BtnId]}, [];
traverse_entries(_,_,0,_) -> [];
traverse_entries(RecordName, Next, Count, S)->
    case kvs:get(RecordName, Next) of {error, not_found} -> [];
    {ok, R}-> self() ! {delivery, [somepath, show_entry], [R, S]}, [R | traverse_entries(RecordName, element(#iterator.prev, R), Count-1, S)] end.

%process_delivery([Type, unregistered], {{ok, Id}, [S]})->
%    error_logger:info_msg("=>>~p unregistered: ~p", [Type, Id]),
%    deselect(S),

%    Start = S#feed_state.start_element,
%    State = S#feed_state{total=S#feed_state.total-1},

%    case element(#iterator.next, Start) of
%        undefined -> traverse(#iterator.next, #iterator{}, State);

%        N when Id == element(#iterator.id, Start) ->
%            case element(#iterator.prev, Start) of
%                undefined -> traverse(#iterator.next, Start, State);
%                _ -> case kvs:get(S#feed_state.entry_type,N) of {error,_} -> ok; {ok, G} -> traverse(#iterator.prev, G, State) end end;

%        N -> case kvs:get(S#feed_state.entry_type, element(#iterator.id, Start)) of 
%                {error, not_found} ->  traverse(#iterator.next, Start, State);
%                _-> case kvs:get(S#feed_state.entry_type,N) of {error,_} -> ok; {ok, G} -> traverse(#iterator.prev, G, State) end end end;

process_delivery([_,_,Type,_,add],
                 [E, #input_state{}=I, #feed_state{}=S])->
    error_logger:info_msg("[feed2] Add entry: ~p to <~p> ", [element(#iterator.id, E), S#feed_state.entries]),
    wf:session(medias, []),
    wf:update(I#input_state.media_id, []),
    wf:wire(wf:f("$('#~s').val('');", [I#input_state.title_id])),
    wf:wire(wf:f("$('#~s').val('');", [I#input_state.body_id])),
    wf:wire(wf:f("$('#~s').val('0.00');", [I#input_state.price_id])),
    wf:wire(#jq{target=I#input_state.body_id, method=[html], args="''"}),
    wf:wire(wf:f("$('#~s').trigger('Reset');", [I#input_state.recipients_id])),
    wf:wire(wf:f("$('#~s').trigger('reset');", [I#input_state.upload_id])),
    wf:insert_top(S#feed_state.entries, #feed_entry2{entry=E, state=S}),
    wf:wire("Holder.run();");

process_delivery([show_entry], [Entry, #feed_state{} = S]) ->
    error_logger:info_msg("[feed2] show_entry ~p", [Entry]),
    wf:insert_bottom(S#feed_state.entries, #feed_entry2{entry=Entry, state=S}),
    wf:wire("Holder.run();"),
    wf:update(S#feed_state.more_toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, delegate=feed2, postback={check_more, Entry, S}});

process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery([_,_,entry,_,delete], [E, #input_state{}, #feed_state{}=S]) ->
    error_logger:info_msg("[feed2 - delivery] Remove entry ~p from <~p>", [element(S#feed_state.entry_id, E), S#feed_state.entries]),
    wf:remove(?EN_ROW(element(S#feed_state.entry_id, E))),
    deselect(S);

process_delivery(R, M) -> ok.
