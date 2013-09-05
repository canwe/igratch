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

tab(categories)-> [
    dashboard:section(input(), "icon-tags"),
%    dashboard:section(categories(), "icon-list") 
    categories()
    ];
tab(acl)-> {AclEn, Acl} = acls(), [
    dashboard:section(acl(Acl), "icon-male"),
    dashboard:section(acl_entry(AclEn), "icon-list") ];
tab(users)-> [
    dashboard:section(users(), "icon-user") ];
tab(products)-> [
    dashboard:section(products(), "icon-gamepad") ];
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

-record(state, {sel_all, sel_toolbar, feed_toolbar, close, feed_title, full}).
categories()->
    {ok,Feed} = kvs:get(feed, ?GRP_FEED),
    Entries = kvs:entries(Feed, group, undefined, ?PAGE_SIZE),
    SelTb = wf:temp_id(),
    FeedTb = wf:temp_id(),
    Close = wf:temp_id(),
    SAll = wf:temp_id(),
    Del = wf:temp_id(),
    FeedTitle = wf:temp_id(),
    ChangeFeed = wf:temp_id(),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    SelAllIds = [ Id++"|"|| #group{id=Id}<- Entries ],
    wf:session(selected,[]),
    State = #state{sel_all=SAll, sel_toolbar=SelTb, feed_toolbar=FeedTb, close=Close, feed_title=FeedTitle, full=SelAllIds},
    [
    #panel{id=FeedTitle, class=["row-fluid"], style="border-bottom:1px solid white; box-shadow: 1px 1px #eeeeee;", body= [
        #panel{class=[span1], style="padding:0 5px;", body=#h3{body=[
           #i{class=["icon-list", blue]},
           #checkbox{id=SAll, class=[checkbox, inline],postback={select, SAll, State}, source=[SAll], value=SelAllIds}
        ]}},
        #panel{class=[span11], style="padding-right:5px;", body=[
            #h3{body=[<<"Categories ">>,
                #span{id=SelTb, style="display:none;", body=[
                    #link{id=Del, class=[btn], body=[<<"delete">>]},
                    #link{id=ChangeFeed, class=[btn], body=[<<"archive">>]}
                ]},

                #span{class=["pull-right"], body=[
                    #panel{id=FeedTb, body=[
                        #small{body=["1-", integer_to_list(?PAGE_SIZE), " of ", integer_to_list(Feed#feed.entries_count)]},
                        #link{class=[btn], body=[<<"<">>], postback={prev, Last}},
                        #link{class=[btn], body=[<<">">>], postback={next, Last}}
                    ]},
                    #link{id=Close, class=[close, "text-error"], style="padding:2px 12px; margin-top:7px;display:none;",
                        postback={cancel_select, State}, body= <<"&times;">>}
                ]}
            ]}
        ]}
    ]},

    #table{id=cats, class=[table, "table-hover"],
        header=[#tr{style="background:white;border-bottom: 2px solid #079ebd; color:black;",
            cells=[#th{body= <<"">>},#th{body= <<"id">>}, #th{body= <<"name">>}, #th{body= <<"description">>}, #th{body= <<"scope">>}]}],
        body=[[#tr{id=Id++"tr", class=[case Scope of private -> "info"; _-> "" end], cells=[
            #td{body= [#checkbox{id=Id, postback={select, Id, State}, source=[Id], value=Id}]}, #td{body=Id}, #td{body=Name}, #td{body=Desc}, #td{body=atom_to_list(Scope)}]} 
            || #group{id=Id, name=Name, description=Desc, scope=Scope} <- Entries ]]}
].

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

acl_entry(Panes)-> [#panel{class=["tab-content"], body=[Panes]}].

acls()->
  lists:mapfoldl(fun(#acl{id={R,N}=Aid}, Ain) ->
    Id = io_lib:format("~p", [Aid]),
    B = #panel{id=atom_to_list(R)++atom_to_list(N), class=["tab-pane"], body=[
      #h3{class=[blue], body=[Id, " entries"]},
      #table{class=[table, "table-hover"], header=[#tr{cells=[#th{body= <<"id">>}, #th{body= <<"accessor">>}, #th{body= <<"action">>}]}], body=[[
        #tr{cells=[#td{body=io_lib:format("~p", [Ai])}, #td{body= Accessor}, #td{body= atom_to_list(Action)}]} || #acl_entry{id=Ai, accessor={user, Accessor}, action=Action} <- kvs:entries(acl, Aid, acl_entry, undefined)
      ]]}
    ]},
    Ao = [#tr{cells=[#td{body=#link{url="#"++atom_to_list(R)++atom_to_list(N), body=Id, data_fields=[{<<"data-toggle">>, <<"tab">>}]}}, #td{body=io_lib:format("~p", [Aid])}]}|Ain],
   {B , Ao}
  end, [], kvs:all(acl)).

users()-> [
  #h3{body= <<"Users">>},
  #table{class=[table, "table-hover"],
    header=[#tr{cells=[#th{body= <<"email">>}, #th{body= <<"roles">>}, #th{body= <<"last login">>}]}],
    body=[[
      begin
        #tr{cells=[
          #td{body=#link{body=U#user.email, postback={view, U#user.email}}},
          #td{body=[profile:features(wf:user(), U, "icon-2x")]},
          #td{body=case kvs:get(user_status, U#user.email) of {ok,Status} -> product_ui:to_date(Status#user_status.last_login); {error, not_found}-> "" end}
        ]}
      end|| #user{} = U <- kvs:entries(kvs:get(feed, ?USR_FEED), user)
    ]]}].

products()->[
  #h3{body= <<"Products">>},
  #table{class=[table, "table-hover"],
    header=[#tr{cells=[#th{body= <<"title">>}]}],
    body=[[
      begin
        #tr{cells=[#td{body=U#product.title} ]}
      end|| U <- kvs:entries(kvs:get(feed, ?PRD_FEED), product)
    ]]}].

event({prev, Last}) ->
    error_logger:info_msg("Prev ~p",[Last]),
    ok;
event({next, Last}) ->
    error_logger:info_msg("Next ~p", [Last]),
    ok;

event({cancel_select, #state{}=State}) ->
    [wf:wire(#jq{target=C, method=["prop"], args=["'checked', false"]}) || C <- wf:session(selected)],
    wf:wire(#jq{target=State#state.sel_all, method=["prop"], args=["'checked', false"]}),
    wf:wire(#jq{target=State#state.sel_toolbar, method=["hide"]}),
    wf:wire(#jq{target=State#state.feed_toolbar, method=["show"]}),
    wf:wire(#jq{target=State#state.close, method=["hide"]}),
    wf:wire(#jq{target=State#state.feed_title, method=["attr"], args=["'style', 'background-color:none;'"]}),
    wf:session(selected, []);

event({select, CurrSel, #state{}=State})->
    Selection = wf:session(selected),
    NewSel = case wf:q(CurrSel) of "undefined" -> if CurrSel == State#state.sel_all -> sets:new(); true -> sets:from_list(Selection--[CurrSel]) end;
    Val -> Vals = string:tokens(Val,"|"),
        [wf:wire(#jq{target=C, method=["prop"], args=["'checked', 'checked'"]}) || C <- Vals],
        sets:from_list(Vals++Selection)
    end,
    case sets:size(NewSel) of 0 -> event({cancel_select, State});
    S ->
        wf:wire(#jq{target=State#state.sel_toolbar, method=["show"]}),
        wf:wire(#jq{target=State#state.close, method=["show"]}),
        wf:wire(#jq{target=State#state.feed_title, method=["attr"], args=["'style', 'background-color:lightblue'"]}),
        wf:wire(#jq{target=State#state.feed_toolbar, method=["hide"]}),
        error_logger:info_msg("Size: ~p", [S]),
        if S == ?PAGE_SIZE -> wf:wire(#jq{target=State#state.sel_all, method=["prop"], args=["'checked', 'checked'"]});
        true -> wf:wire(#jq{target=State#state.sel_all, method=["prop"], args=["'checked', false"]}) end
    end,
    wf:session(selected, sets:to_list(NewSel));

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
          #td{body= G#group.id}, #td{body=G#group.name}, #td{body=G#group.description}, #td{body=atom_to_list(G#group.scope)} ]} )])),
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

process_delivery([create],
                 [{Creator, Id, Name, Desc, Publicity}]) ->
  error_logger:info_msg("responce to create group"),
  ok;
process_delivery(R,M) -> feed:process_delivery(R,M).
