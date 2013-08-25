-module(controls).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
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
    wf:render(dashboard:section([
    #h3{class=[blue], body= Title},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textboxlist{id=RecipientsId, placeholder=I#input.placeholder_rcp, delegate=controls},
        #textbox{id=TitleId, class=[span12], placeholder= I#input.placeholder_ttl},
        #htmlbox{id=EditorId, class=[span12], root=?ROOT, dir=Dir, post_write=attach_media, delegate_api=controls, img_tool=gm, post_target=MsId, size=?THUMB_SIZE},
        #panel{class=["btn-toolbar"], body=[#link{id=SaveId, class=[btn, "btn-large", "btn-success"], body= <<" Post">>,
          delegate=controls, postback={post_entry, RecipientsId, EditorId, TitleId, MsId}, source=[TitleId, EditorId, RecipientsId] }]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias, controls)}
      ]},
      #panel{class=[span3], body=[]}]}], I#input.icon)).

control_event(Cid, _) ->
    SearchTerm = wf:q(term),
    Data = [[list_to_binary(Id++"="++wf:to_list(Name)), list_to_binary(wf:to_list(Name))]
    || #user{email=Id, display_name=Name} <- kvs:entries(kvs:get(feed, ?USR_FEED), user), string:str(string:to_lower(wf:to_list(Id)), string:to_lower(SearchTerm)) > 0],
    element_textboxlist:process_autocomplete(Cid, Data, SearchTerm).

event({post_entry, RecipientsId, EditorId, TitleId, MediasId}) ->
    User = wf:user(),
    Desc = wf:q(EditorId),
    Title = wf:q(TitleId),
    Users = [case kvs:get(user,S) of {error,_} -> []; {ok,G} ->G end || S <- string:tokens(wf:q(RecipientsId), ",")],

     Recipients = lists:append([
        [{user, Id, lists:keyfind(direct,1,Feeds)} || #user{email=Id, feeds=Feeds} <- Users, User#user.id /= Id],
        [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}] ]),

    error_logger:info_msg("Recipients: ~p", [Recipients]),

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
        type=direct,
        media=Medias,
        title=Title,
        description=Desc,
        shared=""}, TitleId, EditorId, MediasId, direct]) || {RoutingType, To, {_, FeedId}} <- Recipients];

event({remove_media, M, Id}) ->
  New = lists:filter(fun(E)-> E/=M end, case wf:session(medias) of undefined -> []; Mi -> Mi end),
  wf:session(medias, New),
  wf:update(Id, product_ui:preview_medias(Id, New, controls)).

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

