-module(input).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include("records.hrl").
-compile(export_all).

render_element(#input{title=Title, state=State, feed_state=FS}=I) ->
    Source = [State#input_state.title_id, State#input_state.body_id, State#input_state.recipients_id],
    User = wf:user(),
    Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
    Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
    {FormStyle, ExpandStyle} = if I#input.collapsed==true -> {"display:none;",""}; true -> {"", "display:none;"} end,

    wf:render(
    [
    case Title of undefined -> []; _ -> #h3{class=[blue], body= Title} end,
    #panel{id=State#input_state.alert_id},
    #panel{id=State#input_state.toolbar_id, class=["row-fluid"], body=[
        #panel{class=["btn-toolbar", I#input.class], style=ExpandStyle, body=[
            #link{class=I#input.expand_class, body=I#input.expand_btn, postback={show_input, State}, delegate=input} ]} ]},

    #panel{id=State#input_state.form_id, class=["row-fluid"], style=FormStyle, body=[#panel{class=[span9], body=[
        if State#input_state.show_recipients == true ->
            #textboxlist{id=State#input_state.recipients_id, placeholder=I#input.placeholder_rcp, delegate=input, values=I#input.recipients, role=I#input.role};true -> [] end,
        if State#input_state.show_title == true ->
            #textbox{id=State#input_state.title_id, class=[span12], placeholder= I#input.placeholder_ttl}; true -> [] end,
        #htmlbox{id=State#input_state.body_id, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, delegate_api=input, img_tool=gm, post_target=State#input_state.media_id, size=?THUMB_SIZE},
        #panel{class=["btn-toolbar"], body=[
            #link{id=State#input_state.post_id, class=?BTN_INFO, body= <<"Post">>, delegate=input, postback={post, State#input_state.entry_type, State, FS}, source=Source},
            #link{class=[btn], style=ExpandStyle, body= <<"Close">>, postback={hide_input, State}, delegate=input}
        ]},
        if State#input_state.show_media == true ->
            #panel{id=State#input_state.media_id, body=preview_medias(State#input_state.media_id, Medias, input)}; true -> [] end
      ]},
      #panel{class=[span3]}]}] ).

preview_medias(Id, Medias, Delegate)->
  L = length(Medias),
  if L > 0 ->
    #carousel{indicators=false, style="border:1px solid #eee;", items=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[close], style="position:absolute; right:10px;top:5px; color:red;",  body= <<"&times;">>, postback={remove_media, M, Id}, delegate=Delegate},
          #link{class=[thumbnail], body=[
            #image{image= case M#media.thumbnail_url of undefined -> <<"holder.js/100%x80">>;
              Th ->
                Ext = filename:extension(Th),
                filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_139x80"++Ext])
             end}
          ]} ]} || M <- lists:sublist(Medias, I, 4) ]} || I <- lists:seq(1, L, 4) ],
      caption=#panel{body= <<"Message will be sent with this medias.">>}}; true-> [] end.

control_event(Cid, Role) ->
    SearchTerm = wf:q(term),
    Entries = [{element(#iterator.id, E), case Role of user-> element(#user.display_name, E); product-> element(#product.title, E); _-> skip end}
        || E <- kvs:entries(kvs:get(feed, ?FEED(Role)), Role, undefined)],

    Data = [[list_to_binary(atom_to_list(Role)++Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
        || {Id, Name} <- Entries, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).

event({post, comment, #input_state{}=Is, #feed_state{}=Fs}) ->
    error_logger:info_msg("=>comment entry:"),
    Comment = wf:q(Is#input_state.body_id),
    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,

    Recipients = Is#input_state.recipients,
    {_,_,{Eid,_,_}} = lists:nth(1,Recipients),
    error_logger:info_msg("Recipients: ~p", [Recipients]),

    Created = now(),
    Cid = kvs:uuid(),
    C = #comment{id = {Cid, {Eid, ?FEED(entry)}},
                from = From,
                comment_id = Cid,
                entry_id = {Eid, ?FEED(entry)},
                feed_id = ?FEED(comment),
                content = wf:js_escape(Comment),
                media = Medias,
                feeds=[{comments, kvs_feed:create()}],
                created = now() },

    [msg:notify([kvs_feed, RoutingType, To, comment, Cid, add],
        [C#comment{id= {Cid, {EntryId, EntryFid}},
            entry_id = {EntryId,EntryFid},
            feed_id = CommentsFid, 
            feeds=[{comments, kvs_feed:create()}]}, Is, ?FD_STATE(CommentsFid)#feed_state{view=comment,  entry_type=comment, mode=panel, entry_id=#comment.comment_id}])
        || {RoutingType, To, {EntryId, EntryFid, {_,CommentsFid}}} <- Recipients],

    msg:notify([kvs_feed, comment, register], [C, Is, ?FD_STATE(?FEED(comment))]);

event({post, entry, #input_state{}=Is, #feed_state{}=Fs})->
    error_logger:info_msg("Post entry: ~p", [Fs]),
    EntryType = Is#input_state.entry_type,
    User = wf:user(),
    Desc = wf:q(Is#input_state.body_id),
    Title = wf:q(Is#input_state.title_id),

    R1 = lists:flatmap(fun(S) -> [begin
        Type = list_to_atom(A),
        Feed = case EntryType of review -> reviews; _ -> EntryType end,
        case kvs:get(Type, string:substr(S, length(A)+Pos)) of {error,_}-> [];
        {ok, E} -> {Type, element(#iterator.id, E), lists:keyfind(Feed, 1, element(#iterator.feeds, E))} end end
        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(wf:q(Is#input_state.recipients_id), ",")),

    R2 = [[ {group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-
        lists:flatten([case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(To)])]
        || {RouteType, To, _} <- R1, RouteType==product],

    UsrFeed = lists:keyfind(case EntryType of review -> feed; direct -> sent; reviews -> feed; _-> EntryType  end, 1, User#user.feeds),
    R3 = case User of undefined -> []; _ when UsrFeed /=false -> [{user, User#user.email, UsrFeed}]; _-> [] end,
    Recipients = lists:flatten([R1,R2,R3]),

    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,

    EntryId = kvs:uuid(),
    E = #entry{
        id = {EntryId, ?FEED(entry)},
        entry_id=EntryId,
        feed_id = ?FEED(entry),
        from=From,
        to = R1,
        type=EntryType,
        media=Medias,
        title=Title,
        description=Desc,
        shared="",
        created = now()
    },

    [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add], [E#entry{
        id={EntryId, FeedId},
        feed_id=FeedId,
        to = {RoutingType, To}}, Is, #feed_state{}]) || {RoutingType, To, {_, FeedId}} = R <- Recipients],

    if EntryType == review ->
        error_logger:info_msg("Put entry in general reviews feed ~p", [E]),
        msg:notify([kvs_feed, entry, register], [E, Is, Fs]); true -> ok end;

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:session(medias) of undefined -> []; Mi -> Mi end),
  wf:session(medias, New),
  wf:update(Id, preview_medias(Id, New, input));

event({show_input, #input_state{}=S})-> wf:wire(#jq{target=S#input_state.toolbar_id,method=[hide]}), wf:wire(#jq{target=S#input_state.form_id,    method=[fadeIn]});
event({hide_input, #input_state{}=S})-> wf:wire(#jq{target=S#input_state.form_id,   method=[hide]}), wf:wire(#jq{target=S#input_state.toolbar_id, method=[fadeIn]}).

api_event(attach_media, Args, _Tag)->
  Props = n2o_json:decode(Args),
  Target = binary_to_list(proplists:get_value(<<"preview">>, Props#struct.lst)),
  Id = proplists:get_value(<<"id">>, Props#struct.lst),
  File = binary_to_list(proplists:get_value(<<"file">>, Props#struct.lst)),
  Type = proplists:get_value(<<"type">>, Props#struct.lst),
  Thumb = binary_to_list(proplists:get_value(<<"thumb">>, Props#struct.lst)),
  Media = #media{id = Id,
    url = File,
    type = {attachment, Type},
    thumbnail_url = Thumb},
  Medias = case wf:session(medias) of undefined -> []; M -> M end,
  NewMedias = [Media | Medias],
  wf:session(medias, NewMedias),
  wf:update(Target, preview_medias(Target, NewMedias, input)).
