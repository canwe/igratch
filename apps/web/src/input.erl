-module(input).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include("records.hrl").
-compile(export_all).

render_element(#input{title=Title}=I) ->
    MsId        = wf:temp_id(),
    TitleId     = wf:temp_id(),
    EditorId    = wf:temp_id(),
    SaveId      = wf:temp_id(),
    RecipientsId= wf:temp_id(),
    FormId      = wf:temp_id(),
    ToolbarId   = wf:temp_id(),
    AlertId     = wf:temp_id(),
    User = wf:user(),
    Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
    Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
    {FormStyle, ExpandStyle} = if I#input.collapsed==true -> {"display:none;",""}; true -> {"", "display:none;"} end,

    wf:render(dashboard:section(wf:temp_id(), [
    case Title of undefined -> []; _ -> #h3{class=[blue], body= Title} end,
    #panel{id=AlertId},
    #panel{id=ToolbarId, class=["row-fluid"], body=[
        #panel{class=["btn-toolbar"], style=ExpandStyle, body=[
            #link{class=?BTN_INFO, body=I#input.expand_btn, postback={show_input, FormId, ToolbarId}, delegate=input} ]} ]},

    #panel{id=FormId, class=["row-fluid"], style=FormStyle, body=[#panel{class=[span9], body=[
        #textboxlist{id=RecipientsId, placeholder=I#input.placeholder_rcp, delegate=input, values=I#input.recipients, role=I#input.role},
        #textbox{id=TitleId, class=[span12], placeholder= I#input.placeholder_ttl},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, delegate_api=input, img_tool=gm, post_target=MsId, size=?THUMB_SIZE},
        #panel{class=["btn-toolbar"], body=[
            #link{id=SaveId, class=?BTN_INFO, body= <<"Post">>, delegate=input,
                postback={post_entry, RecipientsId, EditorId, TitleId, I#input.type, MsId, AlertId}, source=[TitleId, EditorId, RecipientsId] },
            #link{class=[btn, "btn-large"], style=ExpandStyle, body= <<"Close">>, postback={hide_input, FormId, ToolbarId}, delegate=input}
        ]},
        #panel{id=MsId, body=preview_medias(MsId, Medias, input)}
      ]},
      #panel{class=[span3]}]}], I#input.icon, I#input.class)).

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
        || E <- kvs:entries(kvs:get(feed, ?FEED(Role)), Role)],

    Data = [[list_to_binary(atom_to_list(Role)++Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
        || {Id, Name} <- Entries, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).

event({post_entry, RecipientsId, EditorId, TitleId, EntryType, MediasId, AlertId}) ->
    User = wf:user(),
    Desc = wf:q(EditorId),
    Title = wf:q(TitleId),

    R1 = lists:flatmap(fun(S) -> [begin
        Type = list_to_atom(A),
        Feed = case EntryType of review -> reviews; _ -> EntryType end,
        case kvs:get(Type, string:substr(S, length(A)+Pos)) of {error,_}-> [];
        {ok, E} -> {Type, element(#iterator.id, E), lists:keyfind(Feed, 1, element(#iterator.feeds, E))} end end
        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(wf:q(RecipientsId), ",")),

    R2 = [[ {group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-
        lists:flatten([case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(To)])]
        || {RouteType, To, _} <- R1, RouteType==product],

    UsrFeed = lists:keyfind(case EntryType of review -> feed; direct -> sent; reviews -> feed; _-> EntryType  end, 1, User#user.feeds),
    R3 = case User of undefined -> []; _ when UsrFeed /=false -> [{user, User#user.email, UsrFeed}]; _-> [] end,

%    R4 = case UsrFeed of {feed,_} -> {user, User#user.email, {feed, undefined}}; _-> [] end,

    Recipients = lists:flatten([R1,R2,R3]),
    error_logger:info_msg("[input] Recipients: ~p", [Recipients]),

    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User-> User#user.email end,
    EntryId = kvs:uuid(),

    E = #entry{
        id = {EntryId, ?FEED(entry)},
        entry_id=EntryId,
        feed_id = ?FEED(entry),
        from=From,
        to = lists:nth(1, [{Ty,To} || {Ty,To,_} <- R1, Ty==product]),
        type=EntryType,
        media=Medias,
        title=Title,
        description=Desc,
        shared=""
    },

    [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add], [#entry{
        id={EntryId, FeedId},
        entry_id=EntryId,
        feed_id=FeedId,
        created = now(),
        to = {RoutingType, To},
        from=From,
        type=EntryType,
        media=Medias,
        title=Title,
        description=Desc,
        shared=""}, RecipientsId, TitleId, EditorId, MediasId, R]) || {RoutingType, To, {_, FeedId}} = R <- Recipients],

    msg:notify([kvs_feed, entry, register], [E, RecipientsId, TitleId, EditorId, MediasId, ok]);

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:session(medias) of undefined -> []; Mi -> Mi end),
  wf:session(medias, New),
  wf:update(Id, preview_medias(Id, New, input));

event({show_input, Form, Tools})-> wf:wire(#jq{target=Tools, method=[hide]}), wf:wire(#jq{target=Form,  method=[fadeIn]});
event({hide_input, Form, Tools})-> wf:wire(#jq{target=Form,  method=[hide]}), wf:wire(#jq{target=Tools, method=[fadeIn]}).

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
