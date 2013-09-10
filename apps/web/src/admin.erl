-module(admin).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"admin">>},{body, body()}]}.

body() ->
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){tabshow($(e.target).attr('href'));});"),
    Tab = case wf:qs(<<"tab">>) of undefined -> <<"categories">>; T ->  T end,
    wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').tab('show');});",[Tab])),

    Nav = {wf:user(), admin, subnav()},
    index:header() ++ dashboard:page(Nav, [
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
            #panel{id=Id, class=["tab-pane"]} || Id <-[categories, acl, users, products] ]} ]) ++ index:footer().

tab(categories) -> 
    error_logger:info_msg("Show categories"),
    State = #feed_state{container_id=?GRP_FEED, entry_type=group},
    [
    dashboard:section(input(), "icon-tags"),
    #feed2{title= <<"Categories ">>, icon="icon-list", selection=true, state=State,
        header=[#tr{class=["feed-table-header"], cells=[
            #th{body= <<"">>},
            #th{body= <<"id">>},
            #th{body= <<"name">>},
            #th{body= <<"description">>},
            #th{body= <<"scope">>} ]} ]} ];

tab(acl)-> {AclEn, Acl} = acls(), [
    dashboard:section(acl(Acl), "icon-male"),
    #panel{class=["tab-content"], body=[AclEn]}
    ];
tab(users) ->
    State = #feed_state{container_id=?USR_FEED, entry_type=user, entry_id=#user.username},
    #feed2{title= <<"Users ">>, icon="icon-user", state=State,
        header=[#tr{class=["feed-table-header"], cells=[
        #th{body= <<"email">>},
        #th{body= <<"roles">>},
        #th{body= <<"last login">>}]} ]};
tab(products)->
    State = #feed_state{container_id=?PRD_FEED, entry_type=product},
    #feed2{title= <<"Products">>, icon="icon-gamepad", selection=true, state=State,
        header=[#tr{class=["feed-table-header"], cells=[#th{body= <<"">>},#th{body= <<"title">>}]}]};

tab(_)-> [].

subnav() -> [
    {categories, "categories"},
    {acl, "acl"},
    {users, "users"},
    {products, "products"}
  ].

input()-> [
  #h3{body= <<"Add category">>},
    #panel{class=["row-fluid"], body=[#panel{class=[span8], body=[
    #textbox{id=cat_name, class=[span12], placeholder= <<"name">>},
    #textarea{id=cat_desc, class=[span12], placeholder= <<"description">>},
    #select{id=cat_scope, class=[], body=[
      #option{label= <<"scope">>, body = <<"scope">>, disabled=true, selected=true, style="display:none; color:gray;"},
      #option{label= <<"Public">>, value = public},
      #option{label= <<"Private">>, value = private}
    ]},
    #link{id=save_cat, class=[btn, "btn-large"], body=[#i{class=["icon-tags"]}, <<" Create">>], postback=save_cat, source=[cat_name, cat_desc, cat_scope]} 
    ]} ]} ].

resources()->[
  #h3{class=[blue], body= <<"Resources">>},
  #table{class=[table, "table-hover"], body=[[
      #tr{cells=[#td{body= <<"category">>}]},
      #tr{cells=[#td{body= <<"user">>}]},
      #tr{cells=[#td{body= <<"product">>}]},
      #tr{cells=[#td{body= <<"feed">>}]}
      #tr{cells=[#td{body= <<"feature">>}]}
    ]]}
  ].

acl(Rows)->[
  #h3{class=[blue], body= <<"ACL">>},
  #table{class=[table, "table-hover"], header=[#tr{cells=[#th{body= <<"id">>}, #th{body= <<"resourse">>}]}], body=[Rows]}].

acls()->
  lists:mapfoldl(fun(#acl{id={R,N}=Aid}, Ain) ->
    Id = io_lib:format("~p", [Aid]),
    State = #feed_state{container=acl, container_id=Aid, entry_type=acl_entry},
    B = #panel{id=atom_to_list(R)++atom_to_list(N), class=["tab-pane"], body=[
        #feed2{title=Id++" entries", icon="icon-list", selection=false, state=State,
            header=[#tr{class=["feed-table-header"], cells=[
%                #th{body= <<"">>},
                #th{body= <<"id">>},
                #th{body= <<"accessor">>},
                #th{body= <<"action">>}]} ]}
    ]},
    Ao = [#tr{cells=[#td{body=#link{url="#"++atom_to_list(R)++atom_to_list(N), body=Id, data_fields=[{<<"data-toggle">>, <<"tab">>}]}}, #td{body=io_lib:format("~p", [Aid])}]}|Ain],
   {B , Ao}
  end, [], kvs:all(acl)).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(save_cat) ->
  Name = wf:q(cat_name),
  Desc = wf:q(cat_desc),
  Publicity = case wf:q(cat_scope) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
  Creator = (wf:user())#user.email,
  Id = case Publicity of private -> Name; _ -> kvs:uuid() end,
  RegData = #group{id=Id, name = Name, description = Desc, scope = Publicity, creator = Creator, owner = Creator, feeds = ?GRP_CHUNK, created = now()},

  case kvs:add(RegData) of
    {ok, G} ->
      msg:notify([kvs_group, group, init], [G#group.id, G#group.feeds]),

      wf:wire(wf:f("$('#cats > tbody:first').append('~s');", [wf:render(
        #tr{class=[case G#group.scope of private -> "info"; _-> "" end], cells=[
            #td{body=#checkbox{id=G#group.id}},
            #td{body= G#group.id},
            #td{body=G#group.name},
            #td{body=G#group.description},
            #td{body=atom_to_list(G#group.scope)} ]} )])),
      wf:wire("$('#cat_name').val('');$('#cat_desc').val('')");
    {error, _} -> skip
  end;
event({view, Id}) -> error_logger:info_msg("redirect"), wf:redirect("/profile?id="++Id);
event({disable, What})-> error_logger:info_msg("ban user ~p", [What]);
event({revoke, Feature, Whom})->
  error_logger:info_msg("Disable ~p : ~p", [Whom, Feature]),
  User = wf:user(),
  case kvs:get(user, Whom) of {error, not_found} -> skip;
    {ok, U} ->
      kvs_acl:define_access({user, U#user.email}, {feature, Feature}, disable),

      ReplyRecipients = [{user, U#user.email, lists:keyfind(direct, 1, U#user.feeds)}],
      error_logger:info_msg("Reply recipients ~p", [ReplyRecipients]),
      EntryId = kvs:uuid(),
      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
                  [#entry{id={EntryId, FeedId},
                          entry_id=EntryId,
                          feed_id=FeedId,
                          created = now(),
                          to = {RoutingType, To},
                          from=User#user.email,
                          type=reply,
                          media=[],
                          title= <<"Feature disabled">>,
                          description= "You role "++ io_lib:format("~p", [Feature])++" has been disabled!",
                          shared=""}, skip, skip, skip, direct]) || {RoutingType, To, {_, FeedId}} <- ReplyRecipients] end;

event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), tab(list_to_atom(Id)));
api_event(_,_,_) -> ok.

process_delivery(R,M) ->
    feed2:process_delivery(R,M).
