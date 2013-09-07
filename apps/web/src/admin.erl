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

-record(state, {feed_id, prev, next, page_label, sel_all, sel_all_ctl, del_ctl, sel_toolbar, feed_toolbar, close, feed_title, full, start, total, current, start_element, last_element}).
categories()->
    {ok,Feed} = kvs:get(feed, ?GRP_FEED),
    Entries = kvs:entries(Feed, group, ?PAGE_SIZE),
    SelTb = wf:temp_id(),
    FeedTb = wf:temp_id(),
    Close = wf:temp_id(),
    SAll = wf:temp_id(),
    SW = wf:temp_id(),
    Del = wf:temp_id(),
    FeedTitle = wf:temp_id(),
    ChangeFeed = wf:temp_id(),
    Last = case Entries of []-> #iterator{}; E-> lists:last(E) end,
    First = case Entries of []-> #iterator{}; E1-> lists:nth(1,E1) end,
    SelAllIds = [ Id++"|"|| #group{id=Id}<- Entries ],
    wf:session(selected,[]),
    FeedId = wf:temp_id(),
    PrevId = wf:temp_id(),
    NextId = wf:temp_id(),
    PageLbl = wf:temp_id(),
    TotalCount = Feed#feed.entries_count,
    CurrentCount = length(Entries),
    State = #state{feed_id=FeedId, 
        sel_all=SAll,
        sel_toolbar=SelTb,
        feed_toolbar=FeedTb,
        prev=PrevId,
        next=NextId,
        page_label=PageLbl,
        close=Close,
        feed_title=FeedTitle,
%        full=SelAllIds,
        sel_all_ctl = SW,
        del_ctl = Del,
        start_element = First,
        last_element = Last,
        start = 1,
        total = TotalCount,
        current = CurrentCount},
    error_logger:info_msg("FIRST: ~p", [First]),
%    error_logger:info_msg("Entries: ~p", [Entries]),
    
    [
    #panel{id=FeedTitle, class=["row-fluid"], style="border-bottom:1px solid white; box-shadow: 1px 1px #eeeeee;", body= [
        #panel{class=[span1], style="padding:0 5px;", body=#h3{body=[
           #i{class=["icon-list", blue]},
            #span{id=SW,body=[
                #checkbox{id=SAll, class=[checkbox, inline],postback={select, SAll, State}, source=[SAll], value=SelAllIds,
                    style="padding-top:0; padding-left:23px;" ++ if TotalCount > 0 -> [] ; true-> "display:none;" end} 
            ]}
        ]}},
        #panel{class=[span11], style="padding-right:5px;", body=[
            #h3{body=[<<"Categories ">>,
                #span{id=SelTb, style="display:none;", body=[
                    #link{id=Del, class=[btn], body=[<<"delete">>], postback={delete, State}},
                    #link{id=ChangeFeed, class=[btn], body=[<<"archive">>]}
                ]},

                #span{class=["pull-right"], body=[
                    #panel{id=FeedTb, body=[
                        if TotalCount > 0 -> [
                        #small{id=PageLbl, body=[integer_to_list(State#state.start), "-", integer_to_list(CurrentCount), " of ", integer_to_list(TotalCount)]},
                        #link{id=PrevId,class=[btn, case element(#iterator.next, First) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>],
                            postback={traverse, #iterator.next, First, State}},
                        #link{id=NextId,class=[btn, case element(#iterator.prev, Last)  of undefined -> "disabled"; _ -> "" end], body=[<<">">>],
                            postback={traverse, #iterator.prev, Last, State}}]; true -> [] end
    
                    ]},
                    #link{id=Close, class=[close, "text-error"], style="padding:2px 12px; margin-top:7px;display:none;",
                        postback={cancel_select, State}, body= <<"&times;">>}
                ]}
            ]}
        ]}
    ]},

    #table{id=FeedId, class=[table, "table-hover"],
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

event({delete, State})->
    Selection = wf:session(selected),
    [begin
        error_logger:info_msg("Delete selection: ~p", [Id]),
        {ok, Obj} = kvs:get(group, Id),
        msg:notify([kvs_feed, group, unregister], [Obj, State])
    end
    ||Id<-Selection],
    ok;
event({traverse, Direction, Start, State})->
    error_logger:info_msg("=> Traverse ~p from ~p~n", [Direction ,Start]),
    Prev = element(Direction, Start),
    error_logger:info_msg("Really next element ~p dir:~p~n", [Prev, Direction]),
    Entries = case Prev of undefined when Direction == #iterator.prev ->
        {ok,Feed} = kvs:get(feed, ?GRP_FEED),
        kvs:entries(Feed, group, ?PAGE_SIZE);
    undefined when Direction == #iterator.next ->
        {ok,Feed} = kvs:get(feed, ?GRP_FEED),
        kvs:entries(Feed, group, ?PAGE_SIZE);
%    undefined -> [];
        _ ->
        error_logger:info_msg("Read 4 from ~p in ~p", [Prev, Direction]),
        kvs:entries(group, Prev, ?PAGE_SIZE, Direction)
    end,
%    Entries = kvs:entries(group, Prev, ?PAGE_SIZE, Direction),

    SelAllIds = [ Id++"|"|| #group{id=Id}<- Entries ],
%    error_logger:info_msg("Entries: ~p~n", [Entries]),

    NewLast  = case Entries of [] -> #iterator{}; E  -> lists:last(E) end,
    NewFirst = case Entries of [] -> #iterator{}; E1 -> lists:nth(1,E1) end,

    error_logger:info_msg("Entries count: ~p", [length(Entries)]),
    TotalCount = State#state.total,
    CurrentCount = length(Entries),
    NewStart = case Direction of
        #iterator.prev -> State#state.start+?PAGE_SIZE;
        #iterator.next -> State#state.start-?PAGE_SIZE
    end,
    error_logger:info_msg("Entries count: ~p", [CurrentCount]),

    wf:update(State#state.feed_id,
    [
    wf_tags:emit_tag(<<"thead">>, wf:render([#tr{style="background:white;border-bottom: 2px solid #079ebd; color:black;",
            cells=[#th{body= <<"">>},#th{body= <<"id">>}, #th{body= <<"name">>}, #th{body= <<"description">>}, #th{body= <<"scope">>}]}]), []),

    [#tr{id=Id++"tr", class=[case Scope of private -> "info"; _-> "" end], cells=[
            #td{body= [#checkbox{id=Id, postback={select, Id, State#state{current=length(Entries)}}, source=[Id], value=Id}]}, #td{body=Id}, #td{body=Name}, #td{body=Desc}, #td{body=atom_to_list(Scope)}]} 
            || #group{id=Id, name=Name, description=Desc, scope=Scope} <- Entries ]]),

    wf:replace(State#state.prev, #link{id=State#state.prev, class=[btn, case element(#iterator.next, NewFirst) of undefined -> "disabled"; _ -> "" end], body=[<<"<">>], 
        postback={traverse, #iterator.next, NewFirst, State#state{start=NewStart, start_element=NewFirst, last_element=NewLast}}}),
    wf:replace(State#state.next, #link{id=State#state.next, class=[btn, case element(#iterator.prev, NewLast) of undefined -> "disabled"; _ -> "" end], body=[<<">">>], 
        postback={traverse, #iterator.prev, NewLast, State#state{start=NewStart, start_element=NewFirst, last_element=NewLast}}}),

    wf:update(State#state.page_label, [integer_to_list(NewStart), "-", integer_to_list(NewStart+CurrentCount-1), " of ", integer_to_list(TotalCount)]),

    wf:update(State#state.sel_all_ctl,
        #checkbox{id=State#state.sel_all, class=[checkbox, inline],postback={select, State#state.sel_all, State#state{current=length(Entries)}},
            source=[State#state.sel_all], value=SelAllIds,
            style="padding-top:0; padding-left:23px;" ++ if TotalCount > 0 -> [] ; true-> "display:none;" end}
    ),
    wf:replace(State#state.del_ctl, #link{id=State#state.del_ctl, class=[btn], body=[<<"delete">>], postback={delete, 
        State#state{start_element=NewFirst, last_element=NewLast}}});

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
    error_logger:info_msg("CurrSel: ~p", [CurrSel]),
    NewSel = case wf:q(CurrSel) of "undefined" -> if CurrSel == State#state.sel_all -> sets:new(); true -> sets:from_list(Selection--[CurrSel]) end;
    Val -> Vals = string:tokens(Val,"|"),
        [wf:wire(#jq{target=C, method=["prop"], args=["'checked', 'checked'"]}) || C <- Vals],
        sets:from_list(Vals++Selection)
    end,
%    error_logger:info_msg("-~p", [NewSel]),
%    error_logger:info_msg("Select: ~p  |~p", [CurrSel, State]),
    error_logger:info_msg("New selection: ~p", [NewSel]),
    error_logger:info_msg("Size: ~p", [sets:size(NewSel)]),
    error_logger:info_msg("Current: ~p", [State#state.current]),

    case sets:size(NewSel) of 0 -> event({cancel_select, State});
    S ->
        wf:wire(#jq{target=State#state.sel_toolbar, method=["show"]}),
        wf:wire(#jq{target=State#state.close, method=["show"]}),
        wf:wire(#jq{target=State#state.feed_title, method=["attr"], args=["'style', 'background-color:lightblue'"]}),
        wf:wire(#jq{target=State#state.feed_toolbar, method=["hide"]}),
        if S == ?PAGE_SIZE orelse S == State#state.current -> wf:wire(#jq{target=State#state.sel_all, method=["prop"], args=["'checked', 'checked'"]});
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

process_delivery([create],
                 [{_Creator, _Id, _Name, _Desc, _Publicity}]) ->
  error_logger:info_msg("responce to create group"),
  ok;
process_delivery([group, unregistered], {{ok, Id}, [State]})->
    error_logger:info_msg("=>>Group unregistered: ~p", [Id]),
    event({cancel_select, State}),
    Start = State#state.start_element,
%    Last = State#state.last_element,
%    error_logger:info_msg("Start element: ~p",[Start]),
    StartId = element(#iterator.id, Start),
    error_logger:info_msg("Start element: ~p", [Start]),
    error_logger:info_msg("Start deleted? ~p", [StartId == Id]),
    case element(#iterator.next, Start) of
        undefined ->
            error_logger:info_msg("No element before start"),
            error_logger:info_msg("After start: ~p", [element(#iterator.prev, Start)]),
            event({traverse, #iterator.next, #iterator{}, State});
        N when Id == StartId ->
            error_logger:info_msg("start element of page deleted"),
            case element(#iterator.prev, Start) of
                undefined -> 
                    error_logger:info_msg("no prev of start so read next [~p]", [N]),
                    event({traverse, #iterator.next, Start, State});
                _ ->
                    error_logger:info_msg("some element prev the start, so just read prev of next of start. or just start :) "),
                    case kvs:get(group,N) of {error,E} -> error_logger:info_msg("No N in DB", [N]);
                    {ok, G} ->event({traverse, #iterator.prev, G, State}) end
            end;
        N ->
            error_logger:info_msg("Befor start is ~p",[N]),
            error_logger:info_msg("After start: ~p", [element(#iterator.prev, Start)]),
            case kvs:get(group,N) of {error,E} -> error_logger:info_msg("No N in DB", [N]);
            {ok, G} -> event({traverse, #iterator.prev, G, State}) end
        end,
    ok;
process_delivery(R,M) -> feed:process_delivery(R,M).
