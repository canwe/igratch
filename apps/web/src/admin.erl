-module(admin).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

-jsmacro([on_shown/0,show/1]).

on_shown() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("shown", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main()-> #dtl{file="prod",
              bindings=[{title,<<"admin">>},{body,body()},{css,?ADMIN_CSS},{less,?LESS},{bootstrap,?ADMIN_BOOTSTRAP}]}.

body() ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    wf:wire(#api{name=tabshow}),
    wf:wire(on_shown()),

    Nav = {User, admin, subnav()},
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    index:header() ++ dashboard:page(Nav, [
        #panel{class=[span9, "tab-content"], style="min-height:400px;",
            body = if IsAdmin -> [
                #panel{id=categories, class=["tab-pane", active], body=tab(categories)},
                [#panel{id=Id, class=["tab-pane"]} || Id <-[acl, users, products]] 
            ];true -> [] end} ]) ++ index:footer().

subnav() -> [
    {categories, "categories"},
    {acl, "acl"},
    {users, "users"},
    {products, "products"}
  ].

tab(categories) ->
    GroupsFeed = ?FEED(group),
    InputState = case wf:session(?FD_INPUT(GroupsFeed)) of undefined -> 
        Is = ?GROUPS_INPUT, wf:session(?FD_INPUT(GroupsFeed), ?GROUPS_INPUT), Is; IS -> IS end,
    FeedState = case wf:session(GroupsFeed) of undefined ->
        Fs = ?GROUPS_FEED, wf:session(GroupsFeed, Fs), Fs; FS -> FS end,

    [#input{state=InputState, icon="icon-tags"},

    #feed_ui{title= <<"Categories ">>,
        icon="icon-list", state=FeedState,
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"">>},
            #th{body= <<"id">>},
            #th{body= <<"name">>},
            #th{body= <<"description">>},
            #th{body= <<"scope">>} ]} ]}];

tab(acl)->
    {AclEn, Acl} = lists:mapfoldl(fun(#acl{id={R,N}=Aid}, Ain) ->
        State = case wf:session(Aid) of undefined ->
            S = ?ACL_FEED(Aid), wf:session(Aid, S), S; Fs -> Fs end,

        B = #panel{id=wf:to_list(R)++wf:to_list(N), class=["tab-pane"], body=[
            #feed_ui{title=wf:to_list(Aid)++" entries",
                icon="icon-list",
                state=State,
                header=[#tr{class=["feed-table-header"], cells=[
                    #th{body= <<"id">>},
                    #th{body= <<"accessor">>},
                    #th{body= <<"action">>}]} ]}]},
        Ao = [#tr{cells=[
            #td{body=#link{url="#"++wf:to_list(R)++wf:to_list(N), body=wf:to_list(Aid),
                data_fields=[{<<"data-toggle">>, <<"tab">>}]}},
            #td{body=wf:to_list(Aid)}]}|Ain],
        {B , Ao} end, [], kvs:all(acl)),

    [dashboard:section([
        #h3{class=[blue], body= <<"ACL">>},
        #table{class=[table, "table-hover"], header=[#tr{class=["feed-table-header"],
            cells=[#th{body= <<"id">>}, #th{body= <<"resourse">>}]}], body=[Acl]}],
        "icon-male"),
    #panel{class=["tab-content"], body=[AclEn]} ];

tab(users)->
    UsrFeed = ?FEED(user),
    State = case wf:session(UsrFeed) of undefined ->
        S = ?USERS_FEED, wf:session(UsrFeed, S), S; FS -> FS end,

    #feed_ui{title= <<"Users ">>, icon="icon-user", state=State,
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"email">>},
            #th{body= <<"roles">>},
            #th{body= <<"last login">>}]} ]};

tab(products)->
    State = ?PRODUCTS_VIEW_FEED,
    wf:session(?FEED(product), State),

    #feed_ui{title= <<"Products">>, icon="icon-gamepad", state=State, header=[
        #tr{class=["feed-table-header"], cells=[#th{body= <<"">>},#th{body= <<"title">>}, #th{}]}]};
tab(_)-> [].

feature_reply(#user{}=Whom, Feature, Msg, Eid) ->
    case lists:keyfind(direct, 1, Whom#user.feeds) of false -> skip;
        {_,Fid}=Feed ->
            Is = ?DIRECT_INPUT(Fid)#input_state{
                collect_msg = false,
                show_recipients = false,
                title = "Re: Feature <b>"++ wf:to_list(Feature)++"</b> request",
                recipients = [{user, Whom#user.email, Feed}],
                description = Msg},
            input:event({post, direct, Is}),
            error_logger:info_msg("entry reply to: ~p", [Eid]),
            case Eid of undefined -> ok;
            _ -> User = wf:user(),
                case lists:keyfind(direct, 1, User#user.feeds) of false -> ok;
                {_,Did}->
                    error_logger:info_msg("Entry delete: ~p", [Eid]),
                    msg:notify( [kvs_feed, User#user.email, entry, delete],
                                [#entry{id={Eid, Did}, entry_id=Eid, feed_id=Did}, Is]) end end end.

% Events

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case list_to_atom(Id) of categories -> ok; Tab -> wf:update(Tab, tab(Tab)) end;
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({view, Id}) -> error_logger:info_msg("redirect"), wf:redirect("/profile?id="++Id);
event({disable, What})-> error_logger:info_msg("ban user ~p", [What]);
event({allow, Whom, Eid, Feature}) ->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, Whom}, Feature, allow),
        feature_reply(U, Feature, <<"Your request accepted!">>, Eid) end;
event({cancel, Whom, Eid, Feature}) ->
    error_logger:info_msg("Cancel ~p", [Eid]),
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} -> feature_reply(U, Feature, <<"Your request is prohibited!">>, Eid) end;
event({revoke, Feature, Whom})->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, U#user.email}, {feature, Feature}, disable),
        feature_reply(U, Feature, <<"Your role disabled!">>, undefined) end;
event(_) -> ok.

process_delivery(R,M) ->
    feed_ui:process_delivery(R,M).
