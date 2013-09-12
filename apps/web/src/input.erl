-module(input).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").
-compile(export_all).

render_element(#input{title=Title, state=State, feed_state=FS}=I) ->
    Source = [State#input_state.title_id, State#input_state.body_id, State#input_state.recipients_id, State#input_state.price_id, State#input_state.currency_id, State#input_state.scope_id],
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

        if State#input_state.show_price == true ->
            #panel{class=["input-append"], body=[
                #textbox{id=State#input_state.price_id, value=float_to_list(0/100, [{decimals, 2}])},
                #select{id=State#input_state.currency_id, class=[selectpicker], body=[#option{label= L, body = V} || {L,V} <- ?CURRENCY]} ]}; true -> [] end,

        if State#input_state.show_scope == true ->
            #select{id=State#input_state.scope_id, body=[
                #option{label= <<"scope">>, body = <<"scope">>, disabled=true, selected=true, style="display:none; color:gray;"},
                #option{label= <<"Public">>, value = public},
                #option{label= <<"Private">>, value = private} ]}; true -> [] end,

        #panel{class=["btn-toolbar"], body=[
            #link{id=State#input_state.post_id, class=?BTN_INFO, body= <<"Post">>, delegate=input, postback={post, State#input_state.entry_type, State, FS}, source=Source},
            #link{class=[btn], style=ExpandStyle, body= <<"Close">>, postback={hide_input, State}, delegate=input}
        ]},

        if State#input_state.show_media == true ->
            #panel{id=State#input_state.media_id, body=preview_medias(State#input_state.media_id, Medias, input)}; true -> [] end
      ]},
      #panel{class=[span3], body=if State#input_state.show_upload == true -> [
        #upload{id=State#input_state.upload_id, preview=true, root=?ROOT, dir=Dir,
            value="", delegate_query=?MODULE, post_write=attach_media, delegate_api=input, img_tool=gm, post_target=State#input_state.media_id, size=?THUMB_SIZE}
        ]; true -> [] end}]}] ).

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

control_event(_, {query_file, Root, Dir, File, MimeType, PostWrite, Target})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      Media = #media{
        id = element_upload:hash(filename:join([Root,Dir,Name])),
        url = filename:join([Root,Dir,Name]),
        type = {attachment, MimeType},
        thumbnail_url = filename:join([Dir,"thumbnail",Name])},
      wf:session(medias, [Media]),
      wf:update(Target, input:preview_medias(Target, [Media], mygames)),
      FileInfo#file_info.size;
    {error, _} -> 0 end,
  {exist, Size};
control_event(Cid, Role) ->
    SearchTerm = wf:q(term),
    Entries = [{element(#iterator.id, E), case Role of user-> element(#user.display_name, E); product-> element(#product.title, E); group-> element(#group.name, E); _ -> skip end}
        || E <- kvs:entries(kvs:get(feed, ?FEED(Role)), Role, undefined)],

    Data = [[list_to_binary(atom_to_list(Role)++Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
        || {Id, Name} <- Entries, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).

event({post, group, #input_state{}=Is, #feed_state{}=FS}) ->
    User = wf:user(),
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
    Name = wf:q(Is#input_state.title_id),
    Desc = wf:q(Is#input_state.body_id),
    Publicity = case wf:q(Is#input_state.scope_id) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
    Id = case Publicity of private -> Name; _ -> kvs:uuid() end,
    RegData = #group{id=Id, name = Name, description = Desc, scope = Publicity, creator = From, owner = From, feeds = ?GRP_CHUNK, created = now()},

    msg:notify([kvs_group, group, register], [RegData, Is, FS]);

%    case kvs:add(RegData) of
%    {ok, G} ->
%      msg:notify([kvs_group, group, init], [G#group.id, G#group.feeds]),

%      wf:wire(wf:f("$('#cats > tbody:first').append('~s');", [wf:render(
%        #tr{class=[case G#group.scope of private -> "info"; _-> "" end], cells=[
%            #td{body=#checkbox{id=G#group.id}},
%            #td{body= G#group.id},
%            #td{body=G#group.name},
%            #td{body=G#group.description},
%            #td{body=atom_to_list(G#group.scope)} ]} )])),
%      wf:wire("$('#cat_name').val('');$('#cat_desc').val('')");
%    {error, _} -> skip
%  end;

event({post, product, #input_state{}=Is, #feed_state{}=FS}) ->
    error_logger:info_msg("[input] => save product"),
    User = wf:user(),
    Title = wf:q(Is#input_state.title_id),
    Descr = wf:q(Is#input_state.body_id),
    Currency = wf:q(Is#input_state.currency_id),
    TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
    Product = #product{
        id = kvs:uuid(),
        creator = User#user.email,
        owner = User#user.email,
        title = list_to_binary(wf:js_escape(Title)),
        brief = list_to_binary(wf:js_escape(Descr)),
        cover = TitlePic,
        price = product_ui:to_price(wf:q(Is#input_state.price_id)),
        currency = Currency,
        feeds = ?PRD_CHUNK,
        created = now() },

    RawRecipients = if Is#input_state.show_recipients == true -> wf:q(Is#input_state.recipients_id); true -> Is#input_state.recipients end,

    R1 = lists:flatmap(fun(S) -> [
        case kvs:get(list_to_atom(A), string:substr(S, length(A)+Pos)) of {error,_}-> [];
        {ok, E} -> {list_to_atom(A), element(#iterator.id, E), lists:keyfind(products, 1, element(#iterator.feeds, E))} end
        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(RawRecipients, ",")),

    UsrFeed = lists:keyfind(products, 1, User#user.feeds),
    R2 = case User of undefined -> []; _ when UsrFeed /=false -> [{user, User#user.email, UsrFeed}]; _-> [] end,
    R3 = [{product, Product#product.id, {products, ?FEED(product)}}], % general feed
    Recipients = lists:flatten([R1,R2, R3]),
    Medias = case wf:session(medias) of undefined -> []; L -> L end,

    msg:notify([kvs_product, product, register], [Product, Is#input_state{
        recipients = Recipients,
        medias = Medias,
        owner = User#user.email,
        title = Product#product.title,
        description=Product#product.brief}, FS]);

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

event({post, EntryType, #input_state{}=Is, #feed_state{}=Fs})->
    error_logger:info_msg("=>post entry: ~p", [Fs]),
    User = wf:user(),
    Desc = wf:q(Is#input_state.body_id),
    Title = wf:q(Is#input_state.title_id),
    error_logger:info_msg("Show recipients: ~p", [Is#input_state.show_recipients]),
    RawRecipients = if Is#input_state.show_recipients == true -> wf:q(Is#input_state.recipients_id); true -> Is#input_state.recipients end,
    error_logger:info_msg("Entry type: ~p", [EntryType]),
    error_logger:info_msg("Raw: ~p", [RawRecipients]),
    R1 = lists:flatmap(fun(S) -> [begin
        error_logger:info_msg("s:~p a:~p ", [S, A]),
        Type = list_to_atom(A),
        Feed = case EntryType of review -> reviews; _ -> EntryType end,
        ObjId = string:tokens(string:substr(S, length(A)+Pos), "="),
        case kvs:get(Type, lists:nth(1,ObjId)) of {error,_}-> [];
        {ok, E} -> {Type, element(#iterator.id, E), lists:keyfind(Feed, 1, element(#iterator.feeds, E))} end end

        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(RawRecipients, ",")),
    error_logger:info_msg("R1: ~p", [R1]),

    R2 = [[ {group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-
        lists:flatten([case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(To)])]
        || {RouteType, To, _} <- R1, RouteType==product],
    error_logger:info_msg("R2: ~p", [R2]),

    R3 = case User of undefined -> [];
        _ -> Feed = lists:keyfind(case EntryType of review -> feed; direct -> sent; reviews -> feed; _-> EntryType  end, 1, User#user.feeds),
            if  Feed == false -> []; true -> [{user, User#user.email, Feed}] end end,
    error_logger:info_msg("R3: ~p", [R3]),
    R4 = case EntryType of
        review -> [ {user, User#user.email, {feed, ?FEED(entry)}}];
        _-> [] end,
    Recipients = lists:flatten([R1,R2,R3,R4]),
    error_logger:info_msg("Recipients: ~p", [Recipients]),
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
        to = {RoutingType, To}}, Is, ?FD_STATE(FeedId, Fs)])
    || {RoutingType, To, {_, FeedId}} <- Recipients];

%    if EntryType == review ->
%        error_logger:info_msg("Put entry in general reviews feed ~p", [E]),
%        msg:notify([kvs_feed, entry, register], [E, Is, ?FD_STATE(?FEED(entry), Fs)]); true -> ok end;

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
