-module(admin).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").

-include("records.hrl").
-include("states.hrl").

-jsmacro([on_shown/0,show/1]).

on_shown() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("shown", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

feed_states() ->
    [{?FEED(group), ?GROUPS_FEED}, {?FEED(user), ?USERS_FEED}, {?FEED(product), ?PRODUCTS_VIEW_FEED}] ++
    [{Id, ?ACL_FEED(Id)} || #acl{id=Id} <- kvs:all(acl)].

input_states() -> [{?FEED(group), ?GROUPS_INPUT}].

main()-> #dtl{file="prod", bindings=[{title,<<"admin">>},{body, body()}]}.

body() ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    wf:wire(#api{name=tabshow}),
    wf:wire(on_shown()),
    wf:wire(show(case wf:qs(<<"tab">>) of undefined -> "'categories'"; T ->  "'"++wf:to_list(T)++"'" end)),

    Nav = {User, admin, subnav()},
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    index:header() ++ dashboard:page(Nav, [
        #panel{class=[span9, "tab-content"], style="min-height:400px;",
            body = if IsAdmin -> [#panel{id=Id, class=["tab-pane"]} 
                || Id <-[categories, acl, users, products] ];true -> [] end} ]) ++ index:footer().

subnav() -> [
    {categories, "categories"},
    {acl, "acl"},
    {users, "users"},
    {products, "products"}
  ].

tab(categories) -> [
    #input{state=proplists:get_value(?FEED(group), input_states()), icon="icon-tags"},

    #feed_ui{title= <<"Categories ">>,
        icon="icon-list", state=proplists:get_value(?FEED(group), feed_states()),
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"">>},
            #th{body= <<"id">>},
            #th{body= <<"name">>},
            #th{body= <<"description">>},
            #th{body= <<"scope">>} ]} ]}];

tab(acl)->
    {AclEn, Acl} = lists:mapfoldl(fun(#acl{id={R,N}=Aid}, Ain) ->
        State = proplists:get_value(Aid, feed_states()),

        B = #panel{id=wf:to_list(R)++wf:to_list(N), class=["tab-pane"], body=[
            #feed_ui{title=wf:to_list(Aid)++" entries",
                icon="icon-list",
                state=State,
                header=[#tr{class=["feed-table-header"], cells=[
                    #th{body= <<"id">>},
                    #th{body= <<"accessor">>},
                    #th{body= <<"action">>}]} ]}]},
        Ao = [#tr{cells=[
            #td{body=#link{url="#"++atom_to_list(R)++atom_to_list(N), body=wf:to_list(Aid),
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
    State = proplists:get_value(?FEED(user), feed_states()),

    #feed_ui{title= <<"Users ">>, icon="icon-user", state=State,
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"email">>},
            #th{body= <<"roles">>},
            #th{body= <<"last login">>}]} ]};

tab(products)->
    State = proplists:get_value(?FEED(product), feed_states()),

    #feed_ui{title= <<"Products">>, icon="icon-gamepad", state=State, header=[
        #tr{class=["feed-table-header"], cells=[#th{body= <<"">>},#th{body= <<"title">>}, #th{}]}]};

tab(_)-> [].

feature_reply(#user{}=Whom, Feature, Msg, Eid, #feed_state{}=S) ->
    case lists:keyfind(direct, 1, Whom#user.feeds) of false -> skip;
        {_,Id}=Feed ->
            Type = direct,
            Is = #input_state{
                collect_msg = false,
                show_recipients = false,
                entry_type = direct,
                recipients = [{user, Whom#user.email, Feed}],
                title = "Re: Feature <b>"++ wf:to_list(Feature)++"</b> request",
                description = Msg},
            input:event({post, Type, Is, ?DIRECT_STATE(Id)}),

            User = wf:user(),
            Recipients = [{user, User#user.email, lists:keyfind(direct,1, User#user.feeds)}],
            [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete],
                        [#entry{id={Eid, Fid}, entry_id=Eid, feed_id=Fid}, Is, ?FD_STATE(Fid,S)])
                || {RouteType, To, {_,Fid}} <- Recipients] end.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), tab(list_to_atom(Id)));
api_event(_,_,_) -> ok.

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({view, Id}) -> error_logger:info_msg("redirect"), wf:redirect("/profile?id="++Id);
event({disable, What})-> error_logger:info_msg("ban user ~p", [What]);
event({allow, Whom, Eid, Feature, #feed_state{}=S}) ->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, Whom}, Feature, allow),
        feature_reply(U, Feature, <<"Your request accepted!">>, Eid, S) end;
event({cancel, Whom, Eid, Feature, #feed_state{}=S}) ->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} -> feature_reply(U, Feature, <<"Your request is prohibited!">>, Eid, S) end;
event({revoke, Feature, Whom})->
    case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
        kvs_acl:define_access({user, U#user.email}, {feature, Feature}, disable),
        feature_reply(U, Feature, <<"Your role disabled!">>, undefined, #feed_state{entry_id=#entry.entry_id}) end;
event(_) -> ok.

process_delivery(R,M) ->
    feed_ui:process_delivery(R,M).
