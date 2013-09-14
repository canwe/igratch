-module(profile).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> [#dtl{file = "prod",  ext="dtl", bindings=[{title,<<"Profile">>},{body,body()}]}].

body() ->
    Who = case wf:user() of undefined -> #user{}; U -> U end,
    What = case wf:qs(<<"id">>) of undefined -> Who;
        Val -> case kvs:get(user, binary_to_list(Val)) of {error, not_found} -> #user{}; {ok, Usr1} -> Usr1 end end,
    Nav = {What, profile, []},
    State = case lists:keyfind(feed, 1, element(#iterator.feeds, What)) of
        {_, Id}->?FD_STATE(Id)#feed_state{view=review}; false -> #feed_state{} end,
    index:header() ++ dashboard:page(Nav, [
        if What#user.email == undefined -> index:error("There is no user "++wf:to_list(wf:qs(<<"id">>))++"!");
        true -> [
            dashboard:section(profile, profile_info(Who, What, "icon-2x"), "icon-user"),
            if Who == What -> payments(What);
            true -> [
                #input{%title= <<"Write message">>,
                    state=#input_state{collapsed=true, post_collapse= true, show_recipients=false},
                    placeholder_rcp= <<"E-mail/User">>,
                    placeholder_ttl= <<"Subject">>,
                    class="alt",
                    role=user,
                    expand_btn= <<"Write message">>,
                    recipients="user"++wf:to_list(What#user.email)++"="++wf:to_list(What#user.display_name)},

                #feed_ui{title= <<"Recent activity">>, icon="icon-list", state=State}
    ] end ] end ])  ++ index:footer().

profile_info(Who, #user{} = What, Size) ->
%    error_logger:info_msg("What: ~p", [What]),
    RegDate = product_ui:to_date(What#user.register_date),
    Mailto = if What#user.email==undefined -> []; true-> iolist_to_binary(["mailto:", What#user.email]) end,
    Large = Size == "icon-2x",
    [#h3{class=[blue], body= if Large -> <<"Profile">>; true -> <<"&nbsp;&nbsp;&nbsp;Author">> end},
    #panel{class=["row-fluid"], body=[

        #panel{class=[if Large -> span4; true -> span12 end, "dashboard-img-wrapper"], body=
        #panel{class=["dashboard-img"], body=
          #image{class=[], alt="",
            image = case What#user.avatar of undefined ->  "/holder.js/" ++ if Large -> "180x180"; true -> "135x135" end;
            Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}]) ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>}}},

        #panel{class=if Large -> [span8, "profile-info-wrapper"]; true -> [span12] end, body=
        #panel{class=["form-inline", "profile-info"], body=[
          #panel{body=[if Large -> #label{body= <<"Name:">>};true -> [] end, #b{body= What#user.display_name}]},
          if Large -> #panel{body=[if Large -> #label{body= <<"Mail:">>}; true -> [] end, #link{url= Mailto, body=#strong{body= What#user.email}}]}; true -> [] end,
          #panel{body=[if Large -> #label{body= <<"Member since ">>}; true -> [] end, #strong{body= RegDate}]},
          #b{class=["text-success"], body= if What#user.status==ok -> <<"Active">>; true-> atom_to_list(What#user.status) end},
          features(Who, What, Size),
          #p{id=alerts}
        ]}}]} ];
profile_info(Who, What, Size) -> case kvs:get(user, What) of {ok, U}-> profile_info(Who,U,Size); _-> [] end.

features(Who, What, Size) ->
  Writer =  kvs_acl:check_access(What#user.email, {feature,reviewer}) =:= allow,
  Dev =     kvs_acl:check_access(What#user.email, {feature,developer}) =:= allow,
  Admin =   kvs_acl:check_access(What#user.email, {feature,admin}) =:= allow,
  AmIAdmin= kvs_acl:check_access(case Who of undefined -> undefined; #user{} -> Who#user.email; S -> S end,  {feature, admin}) =:= allow,
  [#p{body=[
  if AmIAdmin -> #link{class=["text-warning"], 
      data_fields=?TOOLTIP, title= <<"disable user">>,
      postback={disable, What},
      body = #span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE},#i{class=["icon-user"]}, #i{class=["icon-ban-circle", "text-error"]} ]} };
  true ->  #link{class=["text-warning"],
      data_fields=?TOOLTIP, title= <<"user">>,
      body=#span{class=["icon-stack", Size], body=[#i{class=?STACK_BASE},#i{class=["icon-user"]}]}} end,
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
  if AmIAdmin andalso Dev -> #link{class=[],
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
  #link{
    data_fields=?TOOLTIP, title= <<"administrator">>,
    body=#span{class=["icon-stack", Size, blue], body=[#i{class=?STACK_BASE},#i{class=["icon-wrench icon-light"]}]}}; true->[] end
  ]}].

payments(What) ->
  dashboard:section([
    #h3{class=[blue], body= <<"Payments">>},
    #table{class=[table, "table-hover", payments],
      header=[#tr{cells=[#th{body= <<"Date">>}, #th{body= <<"Status">>}, #th{body= <<"Price">>}, #th{body= <<"Game">>}]}],
      body=[[begin
        #tr{cells= [
          #td{body= [product_ui:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end, float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{url=?URL_PRODUCT(Id),body= Title}} ]} 
      end || #payment{product=#product{id=Id, title=Title, price=Price, currency=Cur}} = Py <-kvs_payment:payments(What#user.email) ]]}], "icon-list").

api_event(Name,Tag,Term) -> error_logger:info_msg("dashboard Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({request, Feature}) -> 
    User =wf:user(),
    case kvs:get(acl, {feature, admin}) of {error, not_found} -> wf:update(alerts,index:error("system has no administrators yet"));
    {ok,#acl{}=Acl} ->
        Recipients = lists:flatten([case kvs:get(user, Accessor) of {error,_} -> [];
            {ok, U} -> {Type, Accessor, lists:keyfind(direct, 1, U#user.feeds)} end
            || #acl_entry{accessor={Type,Accessor}, action=Action} <- kvs:entries(Acl, acl_entry, undefined), Action =:= allow]),

        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = {feature, Feature},
            recipients = Recipients,
            title = "Feature <b>"++ wf:to_list(Feature)++"</b> request",
            description = wf:to_list(Feature) ++ " requested!"},
        input:event({post, {feature, Feature}, Is, ?DIRECT_STATE(?FEED(entry))}),
        wf:update(alerts, index:error(wf:to_list(Feature) ++" requested")) end;
event({read,_, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,
    IsAdmin = kvs_acl:check_access(User#user.email, {feature, admin})==allow,
    if IsAdmin -> admin:event(Event); true -> ok end.

process_delivery(R,M) ->
    wf:update(sidenav, dashboard:sidenav({wf:user(), profile, []})),
    feed_ui:process_delivery(R,M).
