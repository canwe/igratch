-module(controls).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include("records.hrl").
-compile(export_all).

render_element(#input{title=Title}=I) ->
    User = wf:user(),
    Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
    MsId = wf:temp_id(),
    TitleId = wf:temp_id(),
    EditorId = wf:temp_id(),
    SaveId = wf:temp_id(),
    Dir = "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end,
    RecipientsId = wf:temp_id(),
    FormId = wf:temp_id(),
    ToolbarId = wf:temp_id(),
    wf:render(dashboard:section([
    #h3{class=[blue], body= Title},
    #panel{id=ToolbarId, class=["row-fluid"], body=[
        #panel{class=["btn-toolbar"], style=if I#input.collapsed==true -> ""; true -> "display:none;" end, body=[
            #link{class=[btn, "btn-large", "btn-info"], body= <<"Write">>, postback={show_input, FormId, ToolbarId}, delegate=controls}
        ]}
    ]},
    #panel{id=FormId, class=["row-fluid"], style=if I#input.collapsed==true -> "display:none;";true-> "" end, body=[
      #panel{class=[span9], body=[
        #textboxlist{id=RecipientsId, placeholder=I#input.placeholder_rcp, delegate=controls, values=I#input.recipients, role=I#input.role},
        #textbox{id=TitleId, class=[span12], placeholder= I#input.placeholder_ttl},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, delegate_api=controls, img_tool=gm, post_target=MsId, size=?THUMB_SIZE},
        #panel{class=["btn-toolbar"], body=[
            #link{id=SaveId, class=[btn, "btn-large", "btn-info"], body= <<"Post">>,
                delegate=controls, postback={post_entry, RecipientsId, EditorId, TitleId, I#input.type, MsId}, source=[TitleId, EditorId, RecipientsId] },
            #link{class=[btn, "btn-large"], style=if I#input.collapsed==false -> "display:none;";true-> "" end,
                body= <<"Close">>, postback={hide_input, FormId, ToolbarId}, delegate=controls}
        ]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias, controls)}
      ]},
      #panel{class=[span3], body=[]}]}], I#input.icon)).

control_event(Cid, Role) ->
    SearchTerm = wf:q(term),
    Entries = [{element(#iterator.id, E), case Role of user-> element(#user.display_name, E); product-> element(#product.title, E); _-> skip end}
        || E <- kvs:entries(kvs:get(feed, ?FEED(Role)), Role)],

    Data = [[list_to_binary(atom_to_list(Role)++Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
        || {Id, Name} <- Entries, string:str(string:to_lower(wf:to_list(Name)), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).


% - entry type 
%  
event({post_entry, RecipientsId, EditorId, TitleId, EntryType, MediasId}) ->
    User = wf:user(),
    Desc = wf:q(EditorId),
    Title = wf:q(TitleId),
    error_logger:info_msg("Entry type: ~p", [EntryType]),
    error_logger:info_msg("Recipients line: ~p ", [wf:q(RecipientsId)]),

    R1 = lists:flatmap(fun(S) -> [begin
        Type = list_to_atom(A),
        Feed = case EntryType of
            review -> reviews;
            _ -> EntryType
        end,
        case kvs:get(Type, string:substr(S, length(A)+Pos)) of {error,_}-> [];
        {ok, E} -> {Type, element(#iterator.id, E), lists:keyfind(Feed, 1, element(#iterator.feeds, E))} end end

        || {A, Pos} <- [{A, string:str(S, A)} || A <- ["user", "group", "product"]], Pos == 1] end, string:tokens(wf:q(RecipientsId), ",")),

    R2 = [[ {group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-
        lists:flatten([case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end || #group_subscription{where=Where} <- kvs_group:participate(To)])]
        || {RouteType, To, _} <- R1, RouteType==product],

    error_logger:info_msg("Route2: ~p", [R2]),

    UsrFeed = case EntryType of review -> feed; direct -> sent; reviews-> feed; _-> EntryType  end,
    error_logger:info_msg("In user -> ~p", [UsrFeed]),
    R3 = [{user, User#user.email, lists:keyfind(UsrFeed, 1, User#user.feeds)}],

    Recipients = lists:flatten([R1,R2,R3]),
    error_logger:info_msg("[control]Recipients: ~p", [Recipients]),

    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User-> User#user.email end,
    EntryId = kvs:uuid(),

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
        shared=""}, RecipientsId, TitleId, EditorId, MediasId, R]) || {RoutingType, To, {_, FeedId}} = R <- Recipients];

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:session(medias) of undefined -> []; Mi -> Mi end),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New, controls));

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
  wf:update(Target, product_ui:preview_medias(Target, NewMedias, controls)).

