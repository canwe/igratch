-module(profile).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-include("states.hrl").

main() -> [#dtl{file = "prod",  ext="dtl", bindings=[
    {title,<<"Profile">>},{body,body()},{css,?PROFILE_CSS},{less,?LESS},{bootstrap, ?PROFILE_BOOTSTRAP}]}].

body() ->
    Who = case wf:user() of undefined -> #user{}; U -> U end,
    What = case wf:qs(<<"id">>) of undefined -> Who;
        Val -> case kvs:get(user, binary_to_list(Val)) of {error, not_found} -> #user{}; {ok, Usr1} -> Usr1 end end,
    Nav = {What, profile, []},

    index:header() ++ dashboard:page(Nav, [
        if What#user.email == undefined -> index:error("There is no user "++wf:to_list(wf:qs(<<"id">>))++"!");
        true -> [
            dashboard:section(profile, profile_info(Who, What, "icon-2x"), "icon-user"),
            if Who == What -> payments(What);
            true -> [
                case lists:keyfind(direct, 1,What#user.feeds) of false -> [];
                {_,Fid} ->
                    InputState = case wf:cache({?FD_INPUT(Fid),?CTX#context.module}) of undefined ->
                        Is = ?DIRECT_INPUT(Fid)#input_state{
                            show_recipients = false,
                            expand_btn= <<"Write message">> ,
                            recipients=[{user, What#user.email, {direct, Fid}}]},
                        wf:cache({?FD_INPUT(Fid),?CTX#context.module}, Is), Is; IS->IS end,
                    #input{state=InputState, icon=""}
                end,
                case lists:keyfind(feed, 1, element(#iterator.feeds, What)) of false -> [];
                {_, Fid} ->
                    FeedState = case wf:cache({Fid,?CTX#context.module}) of undefined -> 
                        Fs = ?REVIEWS_FEED(Fid), wf:cache({Fid,?CTX#context.module}, Fs), Fs; FS->FS end,
                    #feed_ui{title= <<"Recent activity">>, icon="icon-list", state=FeedState} end ] end ] end ])  ++ index:footer().

profile_info(Who, #user{} = What, Size) -> 
%    error_logger:info_msg("Avatar: ~p", [Who#user.avatar]),
    [
    #h3{class=[blue], body=[<<"Profile ">>,  if Who==What ->
        #span{id=profile_ctl, body=[ #link{body=[#i{class=["icon-edit", "icon-large"]}], title="edit", postback={edit_profile}} ]}; true -> [] end]},

    #panel{class=["row-fluid"], body=[
        #panel{class=[span4, "dashboard-img-wrapper"], body=[
            #panel{id=profile_img, class=["dashboard-img"], body=[
                #image{image = case What#user.avatar of undefined ->  "/holder.js/180x180";
                    Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
                        ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>}]},
            #panel{id=img_ctl, class=["dashboard-img-ctl"], body=[]} ]},

        #panel{class= [span8, "profile-info-wrapper"], body=
            #panel{class=["form-inline", "profile-info"], body=[
                #panel{body=[#label{body= <<"Name:">>}, #b{id=displayname, body= What#user.display_name}]},
                #panel{body=[#label{body= <<"Mail:">>},
                    #link{url= if What#user.email==undefined -> []; true-> iolist_to_binary(["mailto:", What#user.email]) end,
                        body=#strong{body= What#user.email}}]},
                #panel{body=[#label{body= <<"Member since ">>}, #strong{body= index:to_date(What#user.register_date)}]},
                #b{class=[if What#user.status==ok -> "text-success";true ->"text-error" end], body=
                    if What#user.status==ok -> <<"Active">>; true-> wf:to_list(What#user.status) end},
                features(Who, What, Size),
                #p{id=alerts} ]}}]} ];
profile_info(Who, What, Size) -> case kvs:get(user, What) of {ok, U}-> profile_info(Who,U,Size); _-> [] end.

features(Who, What, Size) ->
    Blocked = kvs_acl:check_access(What#user.email, {feature,login}) == disable,
    Writer =  kvs_acl:check_access(What#user.email, {feature,reviewer}) =:= allow,
    Dev =     kvs_acl:check_access(What#user.email, {feature,developer}) =:= allow,
    Admin =   kvs_acl:check_access(What#user.email, {feature,admin}) =:= allow,
    AmIAdmin= kvs_acl:check_access(case Who of undefined -> undefined; #user{} -> Who#user.email; S -> S end,  {feature, admin}) == allow,
  [#p{body=[
    if Blocked andalso AmIAdmin ->
        #link{body=[#span{class=["icon-stack", Size], body=[
                #i{class=?STACK_BASE++["text-error"]},#i{class=["icon-user", "icon-muted"]}]}],
            title= <<"unblock account">>,
            postback={unblock, What},
            delegate=admin};
    Blocked -> #link{body=[#span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE++["text-error"]},#i{class=["icon-user", "icon-muted"]}]}]};
    true ->
    #link{class=["text-warning"],
        data_fields=?TOOLTIP,
        title = if AmIAdmin -> <<"disable user">>; true -> <<"user">> end,
        postback = if AmIAdmin -> {disable,What}; true -> undefined end,
        delegate = admin,
        body=#span{class=["icon-stack", Size], body=[
            #i{class=?STACK_BASE},#i{class=["icon-user"]},
            if AmIAdmin ->  #i{class=["icon-ban-circle", "text-error"]}; true -> [] end
        ]}} end,

  if AmIAdmin andalso Writer -> #link{class=["text-success"],
    data_fields=?TOOLTIP, title= <<"revoke reviewer">>,
    postback={revoke, reviewer, What#user.email},
    body=#span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE},#i{class=["icon-pencil", "icon-light"]}, #i{class=["icon-ban-circle", "text-error"]} ]}};
  Writer -> #link{class=["text-success"],
    data_fields=?TOOLTIP, title= <<"reviewer">>,
    body=#span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE},#i{class=["icon-pencil", "icon-light"]}]}}; 
  Who==What -> #link{
    data_fields=?TOOLTIP, title= <<"request reviewer role">>,
    body=#span{class=["icon-stack", Size], body=[#i{class=["icon-circle", "icon-stack-base", "icon-muted"]},#i{class=["icon-pencil"]}, #i{class=["icon-large", "icon-question", "text-error"]}]},
    postback={request, reviewer}};
  true -> []
  end,

  if AmIAdmin andalso Dev -> #link{
    postback={revoke, developer, What#user.email},
    data_fields=?TOOLTIP, title= <<"revoke developer">>,
    body=#span{class=["icon-stack", Size], body=[#i{class=["icon-circle", "icon-stack-base", "icon-2x"]},#i{class=["icon-barcode", "icon-light"]}, #i{class=["icon-ban-circle", "text-error"]}  ]}};
  Dev -> #link{class=[""],
    data_fields=?TOOLTIP, title= <<"developer">>,
    body=#span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE},#i{class=["icon-barcode", "icon-light"]}]}}; 
  Who == What -> #link{
    data_fields=?TOOLTIP, title= <<"request developer role">>,
    body=#span{class=["icon-stack", Size], body=[#i{class=["icon-circle", "icon-stack-base", "icon-muted"]},#i{class=["icon-barcode"]}, #i{class=["icon-large","icon-question", "text-error"]}]},
    postback={request, developer}};
  true -> []
  end,
  if Admin ->
     #link{data_fields=?TOOLTIP, title= <<"administrator">>,
            body=#span{class=["icon-stack", Size, blue],
            body=[#i{class=?STACK_BASE},#i{class=["icon-wrench", "icon-light"]}]}}; true->[] end
  ]}].

payments(What) ->
  dashboard:section([
    #h3{class=[blue], body= <<"Payments">>},
    #table{class=[table, "table-hover", payments],
      header=[#tr{class=["feed-table-header"],
                cells=[#th{body= <<"Date">>}, #th{body= <<"Status">>}, #th{body= <<"Price">>}, #th{body= <<"Game">>}]}],
      body=[[begin
        #tr{cells= [
          #td{body= [index:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end, float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{url=?URL_PRODUCT(Id),body= Title}} ]} 
      end || #payment{product=#product{id=Id, title=Title, price=Price, currency=Cur}} = Py <-kvs_payment:payments(What#user.email) ]]}], "icon-list").

preview_medias(#media{} = M)-> [
    #image{image = case M#media.thumbnail_url of undefined -> <<"/holder.js/180x180">>;
        Th ->  Ext = filename:extension(Th),
            filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_180x180"++Ext]) end},
    #panel{id=apply_ctl, class=["btn-toolbar", "text-center"],body=[
        #link{class=[btn],body= <<"apply">>, postback={apply_image, M}},
        #link{class=[btn],body= <<"cancel">>, postback={close_image}} ]}].

% Event

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
  wf:update(Target, preview_medias(Media));

api_event(Name,Tag,Term) -> error_logger:info_msg("dashboard Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
  Name = binary_to_list(File),
  Size = case file:read_file_info(filename:join([Root,Dir,Name])) of 
    {ok, FileInfo} ->
      Media = #media{
        id = element_upload:hash(filename:join([Root,Dir,Name])),
        url = filename:join([Root,Dir,Name]),
        type = {attachment, MimeType},
        thumbnail_url = filename:join([Dir,"thumbnail",Name])},
      wf:update(Target, preview_medias(Media)),
      FileInfo#file_info.size;
    {error, _} -> 0 end,
  {exist, Size}.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({edit_profile}) ->
    Who = wf:user(),
    wf:update(profile_ctl, [#link{body=#i{class=["icon-reply", "icon-large", "text-warning"]},
        title= <<"close">>, postback={cancel}}]),
%    wf:update(displayname, #panel{class=["input-append"], body=[
%        #textbox{id=display_name,value=[Who#user.display_name]},
%        #button{class=[btn], body=#i{class=["icon-refresh", "icon-large"]},
%            title= <<"update">>, source=[display_name], postback={apply_name}} ]}),
    wf:update(img_ctl,
        #upload{id=profile_upload,
                preview=false,
                root=?ROOT,
                dir="static/"++ case Who of undefined-> "anonymous"; User -> User#user.email end,
                value="",
                delegate_query=?MODULE,
                post_write=attach_media,
                delegate_api=?MODULE,
                img_tool=gm,
                post_target=profile_img,
                size=?THUMB_SIZE});
event({cancel})->
    Who = wf:user(),
    wf:update(profile_ctl, [#link{body=#i{class=["icon-edit", "icon-large"]}, title= <<"edit">>, postback={edit_profile}}]),
%    wf:update(displayname, Who#user.display_name),
    wf:update(img_ctl, []);
event({apply_name}) ->
    User = wf:user(),
    DisplayName = wf:q(display_name),
    case kvs:put(User#user{display_name=DisplayName}) of ok ->
        wf:user(User#user{display_name=DisplayName}),
        wf:update(alerts, index:success(<<"Display name updated.">>));
    _-> wf:update(alerts, index:error(<<"Failed to update display name. Please try again.">>)) end;
event({apply_image, #media{thumbnail_url=Th}}) ->
    User = wf:user(),
    Ext = filename:extension(Th),
    Ava = filename:join([filename:dirname(Th), filename:basename(Th, Ext)++"_180x180"++Ext]),
    case kvs:put(User#user{avatar=Ava}) of ok ->
        wf:user(User#user{avatar=Ava}),
        wf:update(alerts, index:success(<<"Profile image updated.">>)),
        wf:remove(apply_ctl);
    _-> wf:update(alerts, index:error(<<"Failed to update profile image. Please try again.">>)) end;
event({close_image}) ->
    User =  wf:user(),
    wf:update(profile_img, #image{image = case User#user.avatar of undefined ->  "/holder.js/180x180";
        Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
            ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>});
event({request, Feature}) ->
    User =wf:user(),
    case kvs:get(acl, {feature, admin}) of {error, not_found} -> wf:update(alerts,index:error("system has no administrators yet"));
    {ok,#acl{}=Acl} ->
        Recipients = lists:flatten([case kvs:get(user, Accessor) of {error,_} -> [];
            {ok, U} -> {Type, Accessor, lists:keyfind(direct, 1, U#user.feeds)} end
            || #acl_entry{accessor={Type,Accessor}, action=Action} <- kvs:entries(Acl, acl_entry, undefined), Action =:= allow]),

        case lists:keyfind(direct, 1, User#user.feeds) of false -> skip;
        {_,Fid} ->
            Is = ?DIRECT_INPUT(Fid)#input_state{
                entry_type = {feature, Feature},
                collect_msg = false,
                show_recipients = false,
                title = "Feature <b>"++ wf:to_list(Feature)++"</b> request",
                recipients = Recipients,
                description = wf:to_list(Feature) ++ " requested!"},

            input:event({post, {feature, Feature}, Is}),

            wf:update(alerts, index:error(wf:to_list(Feature) ++" requested")) end end;
event(Event) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    if IsAdmin -> admin:event(Event); true -> ok end.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), profile, []})),
    feed_ui:process_delivery(R,M).
