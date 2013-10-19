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

render_element(#input{state=State, feed_state=FS}=I) ->
    wf:render(input(I, State, FS)).

input(I=#input{title=Title}, State=#input_state{}, FS=#feed_state{}) ->
    Source = [State#input_state.title_id,
        State#input_state.body_id,
        State#input_state.recipients_id,
        State#input_state.price_id,
        State#input_state.currency_id,
        State#input_state.scope_id],

    User = wf:user(),
    Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
    Medias = case wf:session(medias) of undefined -> State#input_state.medias; []-> State#input_state.medias; Ms -> Ms end,

    {FormStyle, ExpandStyle} = if State#input_state.collapsed==true -> {"display:none;",""}; true -> {"", "display:none;"} end,
    CancelStyle = if State#input_state.update == false -> "display:none;"; true -> "" end,
    Cover = if length(State#input_state.medias) > 0 -> (lists:nth(1,State#input_state.medias))#media.url; true -> "" end,
%    wf:wire("$('.selectpicker').each(function() {var $select = $(this); $select.selectpicker($select.data());});"),
    error_logger:info_msg("Show recipients: ~p", [State#input_state.show_recipients]),
    [
        #panel{id=State#input_state.toolbar_id, class=["row-fluid"], body=[
            #panel{class=["btn-toolbar", I#input.class], style=ExpandStyle, body=[
                #link{class=I#input.expand_class, body=I#input.expand_btn, postback={show_input, State}, delegate=input} ]} ]},

        #panel{id=State#input_state.form_id, class=[I#input.class], style=FormStyle, body=[
            #panel{id=State#input_state.alert_id},
            case Title of undefined -> []; _ -> #h4{class=[blue], body= Title} end,

            #panel{class=["row-fluid"], body=[

            #panel{class=[span9, input], body=[
                if State#input_state.show_recipients == true ->
                    #textboxlist{id=State#input_state.recipients_id,
                        placeholder=I#input.placeholder_rcp,
                        delegate=input,
                        values=I#input.recipients,
                        role=I#input.role};true -> [] end,

                if State#input_state.show_title == true ->
                    #textbox{id=State#input_state.title_id, class=[span12],placeholder= I#input.placeholder_ttl, value=State#input_state.title}; true -> [] end,

                if State#input_state.simple_body == true ->
                    #textarea{id=State#input_state.body_id, class=[span12], placeholder=I#input.placeholder_box, body=State#input_state.description};
                true ->
                    #htmlbox{id=State#input_state.body_id, class=[span12],
                        html = State#input_state.description,
                        root=?ROOT, dir=Dir,
                        post_write=attach_media,
                        delegate_api=input,
                        img_tool=gm,
                        post_target=State#input_state.media_id, size=?THUMB_SIZE} end,

                if State#input_state.show_price == true ->
                    #panel{class=["input-append"], body=[
                        #textbox{id=State#input_state.price_id, 
                            value=float_to_list(if State#input_state.price == undefined -> 0; true -> State#input_state.price end/100, [{decimals, 2}])},
                        #select{id=State#input_state.currency_id, class=[selectpicker],
                            body=[#option{label= L, body = V} || {L,V} <- ?CURRENCY]} ]}; true -> [] end,

                if State#input_state.show_scope == true ->
                    #select{id=State#input_state.scope_id, body=[
                        #option{label= <<"scope">>,   body = <<"scope">>, disabled=true, selected=true, style="display:none; color:gray;"},
                        #option{label= <<"Public">>,  value = public},
                        #option{label= <<"Private">>, value = private} ]}; true -> [] end,

                #panel{class=["btn-toolbar"], body=[
                    #link{id=State#input_state.post_id, class=I#input.post_class,
                        body=if State#input_state.update == false -> I#input.post_btn; true -> I#input.update_btn end,
                        delegate=input,
                        postback={
                            if State#input_state.update == false -> post; true -> update end,
                            State#input_state.entry_type, State, FS},
                        source=Source},
                    #link{class=I#input.close_class, style=ExpandStyle, body=I#input.close_btn,
                        delegate=input, postback={hide_input, State}},
                    #link{class=I#input.cancel_class, style=CancelStyle, body=I#input.cancel_btn,
                        delegate=input, postback={clear_input, I}} ]},

                if State#input_state.show_media == true ->
                    #panel{id=State#input_state.media_id,
                        body=preview_medias(State#input_state.media_id, Medias, input)}; true -> [] end ]},

            #panel{class=[span3], body=if State#input_state.show_upload == true -> [
                #upload{id=State#input_state.upload_id,
                    preview=true, root=?ROOT, dir=Dir,
                    value="",
                    delegate_query=?MODULE,
                    post_write=attach_media,
                    delegate_api=input,
                    img_tool=gm,
                    post_target=State#input_state.media_id, size=?THUMB_SIZE}]; true -> [] end} 
            ]} 
        ]}
    ].

preview_medias(Id, Medias, Delegate)->
  L = length(Medias),
  if L > 0 ->
    #carousel{indicators=false, style="border:1px solid #eee;", items=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[close], style="position:absolute; right:10px;top:5px; color:red;",
            body= <<"&times;">>, postback={remove_media, M, Id}, delegate=Delegate},
          #link{class=[thumbnail], body=[
            #image{image= case M#media.thumbnail_url of undefined -> <<"holder.js/100%x80">>;
              Th ->
                Ext = filename:extension(Th),
                filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_139x80"++Ext])
             end}
          ]} ]} || M <- lists:sublist(Medias, I, 4) ]} || I <- lists:seq(1, L, 4) ],
      caption=#panel{body= <<"Message will be sent with this medias.">>}}; true-> [] end.

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      Media = #media{
        id = element_upload:hash(filename:join([Root,Dir,Name])),
        url = filename:join([Root,Dir,Name]),
        type = {attachment, MimeType},
        thumbnail_url = filename:join([Dir,"thumbnail",Name])},
      wf:session(medias, [Media]),
      wf:update(Target, input:preview_medias(Target, [Media], input)),
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
    Publicity = case wf:q(Is#input_state.scope_id) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
    Id = case Publicity of private -> Name; _ -> kvs:uuid() end,
    RegData = #group{id=Id,
                    name = Name,
                    description = wf:q(Is#input_state.body_id),
                    scope = Publicity,
                    creator = From,
                    owner = From,
                    feeds = ?GRP_CHUNK,
                    created = now()},

    msg:notify([kvs_group, group, register], [RegData, Is, FS]);

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
        title = list_to_binary(Title),
        brief = list_to_binary(Descr),
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

    R1 = Is#input_state.recipients,
    {_,_,{Eid,_,_}} = lists:nth(1,R1),
%    error_logger:info_msg("[input]Recipients: ~p", [R1]),
    R2 = [ {user, From, {Eid, ?FEED(entry), {feed, ?FEED(comment)}}}],

    Recipients = lists:flatten([R1,R2]),
    error_logger:info_msg("[input] -> Recipients: ~p", [Recipients]),
    Created = now(),
    Cid = kvs:uuid(),
    C = #comment{id = {Cid, {Eid, ?FEED(entry)}},
                from = From,
                comment_id = Cid,
                entry_id = {Eid, ?FEED(entry)},
                feed_id = ?FEED(comment),
                content = list_to_binary(Comment),
                media = Medias,
                feeds=[{comments, kvs_feed:create()}],
                created = now() },
    
    [msg:notify([kvs_feed, RoutingType, To, comment, Cid, add],
        [C#comment{id= {Cid, {EntryId, EntryFid}},
            entry_id = {EntryId,EntryFid},
            feed_id = CommentsFid, 
            feeds=[{comments, kvs_feed:create()}]}, Is,
            ?FD_STATE(CommentsFid, Fs)#feed_state{recipients=Recipients}])
        || {RoutingType, To, {EntryId, EntryFid, {_,CommentsFid}}} <- Recipients];

%    msg:notify([kvs_feed, comment, register], [C, Is, ?FD_STATE(?FEED(comment))]);

event({post, EntryType, #input_state{}=Is, #feed_state{}=Fs})->
    error_logger:info_msg("=>post entry: ~p", [Fs]),
    User = wf:user(),
    {Title, Desc} = if Is#input_state.collect_msg == true ->
        {wf:q(Is#input_state.title_id), wf:q(Is#input_state.body_id)}; 
    true -> {Is#input_state.title, Is#input_state.description} end,
    error_logger:info_msg("Entry type: ~p", [EntryType]),
    R1 = if Is#input_state.show_recipients == true ->
        lists:flatmap(fun(S) -> [begin
        Type = list_to_atom(A),
        Feed = case EntryType of review -> reviews; _ -> EntryType end,
        ObjId = string:tokens(string:substr(S, length(A)+Pos), "="),
        case kvs:get(Type, lists:nth(1,ObjId)) of {error,_}-> [];
        {ok, E} -> {Type, element(#iterator.id, E), lists:keyfind(Feed, 1, element(#iterator.feeds, E))} end end
        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, 
            string:tokens(wf:q(Is#input_state.recipients_id), ","));
    true -> Is#input_state.recipients end,

    error_logger:info_msg("R1: ~p", [R1]),

    R2 = [[ {group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-
        lists:flatten([case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(To)])]
        || {RouteType, To, _} <- R1, RouteType==product, EntryType==review orelse EntryType == reviews],
    error_logger:info_msg("R2: ~p", [R2]),

    R3 = case User of undefined -> [];
        _ -> Feed = lists:keyfind(case EntryType of review -> feed; direct -> sent; reviews -> feed; _-> EntryType  end, 1, User#user.feeds),
            if  Feed == false -> []; true -> [{user, User#user.email, Feed}] end end,
    error_logger:info_msg("R3: ~p", [R3]),
    R4 = case EntryType of
        review -> [ {user, User#user.email, {feed, ?FEED(entry)}}];
        _-> [] end,
    Recipients = lists:flatten([R1,R2,R3,R4]),
    error_logger:info_msg("[input]Recipients: ~p", [Recipients]),
    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,

    EntryId = case Is#input_state.entry_id of undefined -> kvs:uuid(); Id -> Id end,
    E = #entry{
        id = {EntryId, ?FEED(entry)},
        entry_id=EntryId,
        feed_id = ?FEED(entry),
        from=From,
        to = R1,
        type=EntryType,
        media=Medias,
        title=iolist_to_binary(Title),
        description=iolist_to_binary(Desc),
        shared="",
        created = now()
    },

    [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add], [E#entry{
        id={EntryId, FeedId},
        feed_id=FeedId,
        to = {RoutingType, To}}, Is, ?FD_STATE(FeedId, Fs)])
    || {RoutingType, To, {_, FeedId}} <- Recipients];

event({update, product, #input_state{}=Is, #feed_state{}=Fs}) ->
    error_logger:info_msg("=>update product", []),
    case kvs:get(product, Is#input_state.entry_id) of {error,_}-> wf:update(Is#input_state.alert_id, index:error("no object"));
    {ok, P=#product{}} ->
        User = wf:user(),
        Title = wf:q(Is#input_state.title_id),
        Descr = wf:q(Is#input_state.body_id),
        Currency = wf:q(Is#input_state.currency_id),

        TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
        Medias = case wf:session(medias) of undefined -> []; L -> L end,
        error_logger:info_msg("Title picture: ~p~n Medias: ~p", [TitlePic, Medias]),

        Product = P#product{
            title = list_to_binary(Title),
            brief = list_to_binary(Descr),
            cover = TitlePic,
            price = product_ui:to_price(wf:q(Is#input_state.price_id)),
            currency = Currency},

        Entry = #entry{
            created=Product#product.created,
            entry_id=Product#product.id,
            from=Product#product.owner,
            type= product,
            media=Medias,
            title=Product#product.title,
            description=Product#product.brief,
            shared=""},

        kvs:put(Product),

        RawRecipients = if Is#input_state.show_recipients == true -> wf:q(Is#input_state.recipients_id); true -> Is#input_state.recipients end,

        R1 = lists:flatmap(fun(S) -> [
            case kvs:get(list_to_atom(A), string:substr(S, length(A)+Pos)) of {error,_}-> [];
            {ok, E} -> {list_to_atom(A), element(#iterator.id, E), lists:keyfind(products, 1, element(#iterator.feeds, E))} end
            || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(RawRecipients, ",")),
        error_logger:info_msg("r1: ~p ", [R1]),

        Groups = ordsets:from_list([case kvs:get(Type, S) of {error,_}->[]; {ok, G}->G end ||{Type, S, _}<-R1, Type==group]),
        Participate = ordsets:from_list([ case kvs:get(group, Where) of {ok, G}->G;_->[] end  || #group_subscription{where=Where} <- kvs_group:participate(P#product.id)]),

        Intersection = ordsets:intersection(Groups, Participate),
        OldSubs = ordsets:subtract(Participate, Intersection),
        NewSubs = ordsets:subtract(Groups, Intersection),

        % stay in group and edit entry
        Rec1 = [{user, P#product.owner, lists:keyfind(products, 1, User#user.feeds)} |
            [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- Intersection]],
        error_logger:info_msg("Edit recipients: ~p~n", [Rec1]),

        [msg:notify([kvs_feed, RouteType, To, entry, Eid, edit],
            [Entry#entry{to = {RouteType, To}, feed_id=Fid, id={Product#product.id, Fid}}, Is, ?FD_STATE(Fid, Fs)]) || {RouteType, To, {_,Fid}=Eid} <- Rec1],

        % leave group and remove entry
        Rec2 = [{group,Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- OldSubs],
        error_logger:info_msg("Remove recipients: ~p~n", [Rec2]),
        [msg:notify([kvs_feed, RouteType, To, entry, Eid, delete], [Entry, skip]) || {RouteType, To, Eid} <- Rec2],
        [kvs_group:leave(Product#product.id, G) || {_,G,_} <- Rec2],

        % join group and add entry
        Rec3 = [{group, Where, lists:keyfind(products, 1, Feeds)} || #group{id=Where, feeds=Feeds} <- NewSubs],
        error_logger:info_msg("Add recipients: ~p~n", [Rec3]),
        [kvs_group:join(Product#product.id, G) || {_,G,_} <- Rec3],
        [msg:notify([kvs_feed, RoutingType, To, entry, Product#product.id, add],
            [Entry#entry{id={Product#product.id, Fid},feed_id=Fid,to = {RoutingType, To}}, Is, ?FD_STATE(Fid,Fs)]) || {RoutingType, To, {_, Fid}} <- Rec3],

        wf:update(Is#input_state.alert_id, index:success("updated"))
    end;
event({update, Type, #input_state{}=Is, #feed_state{}=Fs}) ->
    Id = Is#input_state.entry_id,
    error_logger:info_msg("Update ~p ~p", [Type, Id]);

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:session(medias) of undefined -> []; Mi -> Mi end),
  wf:session(medias, New),
  wf:update(Id, preview_medias(Id, New, input));

event({show_input, #input_state{}=S})->
    wf:wire(#jq{target=S#input_state.toolbar_id,method=[hide]}),
    wf:wire(#jq{target=S#input_state.form_id,   method=[fadeIn]});
event({hide_input, #input_state{}=S})->
    wf:wire(#jq{target=S#input_state.form_id,   method=[hide]}),
    wf:wire(#jq{target=S#input_state.toolbar_id,method=[fadeIn]});
event({clear_input, #input{state=State, feed_state=FS}=I})->
    error_logger:info_msg("CLEAR INPUT ~p", [I#input.state#input_state.id]),
    wf:session(medias, []),
    wf:update(I#input.state#input_state.id,
        #panel{body=[input(
        I#input{recipients=[], title= <<"New game">>},
        State#input_state{update=false, price=undefined, title=undefined, description=undefined, medias=[], entry_id=undefined},
        FS)]}
    ),
%    wf:wire("$('.selectpicker').each(function() {var $select = $(this); $select.selectpicker($select.data());});"),
    ok;
event(E)-> error_logger:info_msg("INPUT:~p", [E]).

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
