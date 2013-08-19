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

main() -> [#dtl{file = "prod",  ext="dtl", bindings=[{title,<<"Account">>},{body,body()}]}].

body() ->
  Who = case wf:user() of undefined -> #user{}; U -> U end,
  What = case wf:qs(<<"id">>) of undefined -> Who;
    Val -> case kvs:get(user, binary_to_list(Val)) of {error, not_found} -> #user{}; {ok, Usr1} -> Usr1 end
  end,

  index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        if What#user.email == undefined -> index:error("There is no user "++binary_to_list(wf:qs(<<"id">>))++"!");
          true -> [
          #panel{id=side_menu, class=[span3], body=dashboard:sidebar_menu(Who, What, profile, [])},
          #panel{class=[span9], body=[
            dashboard:section(profile_info(Who, What), "icon-user"),
            dashboard:section(payments(Who, What), "icon-list")
          ]}] end
      ]}}}
  ] ++ index:footer().

profile_info(Who, What) ->
      RegDate = product_ui:to_date(What#user.register_date),
      Mailto = if What#user.email==undefined -> []; true-> iolist_to_binary(["mailto:", What#user.email]) end,
      [
      #h3{class=[blue], body= <<"Profile">>},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span4, "dashboard-img-wrapper"], body=
        #panel{class=["dashboard-img"], body=
          #image{class=[], alt="",
            image = case What#user.avatar of undefined ->  "/holder.js/180x180";
              Av -> 
              re:replace(Av, <<"_normal">>, <<"">>, [{return, list}]) 
                ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>}} },
        #panel{class=[span8, "profile-info-wrapper"], body=
        #panel{class=["form-inline", "profile-info"], body=[
          #panel{body=[#label{body= <<"Name:">>}, #b{body= What#user.display_name}]},
          #panel{show_if=What#user.email=/=undefined, body=[#label{body= <<"Mail:">>}, #link{url= Mailto, body=#strong{body= What#user.email}}]},
          #panel{body=[#label{body= <<"Member since ">>}, #strong{body= RegDate}]},
          #b{class=["text-success"], body= if What#user.status==ok -> <<"Active">>; true-> atom_to_list(What#user.status) end},
          features(Who, What),
          #p{id=alerts}
        ]}}]} ].

features(Who, What) ->
  Writer =  kvs_acl:check_access(What#user.email, {feature,reviewer}) =:= allow,
  Dev =     kvs_acl:check_access(What#user.email, {feature,developer}) =:= allow,
  Admin =   kvs_acl:check_access(What#user.email, {feature,admin}) =:= allow,
  [#p{body=[
  #link{class=["text-warning"],
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"user">>,
    body=#span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-stack-base", "icon-circle"]},#i{class=["icon-user"]}]}},
  if Writer -> #link{class=["text-success"],
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"reviewer">>,
    body=#span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-pencil icon-light"]}]}}; 
  Who==What -> #link{
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"request reviewer role">>,
    body=#span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-circle", "icon-stack-base", "icon-muted"]},#i{class=["icon-pencil"]}, #i{class=["icon-large", "icon-question", "text-error"]}]},
    postback={request, reviewer}};
  true -> []
  end,
  if Dev -> #link{class=[""],
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"developer">>,
    body=#span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-barcode", "icon-light"]}]}}; 
  Who == What -> #link{
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"request developer role">>,
    body=#span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-circle", "icon-stack-base", "icon-muted"]},#i{class=["icon-barcode"]}, #i{class=["icon-large","icon-question", "text-error"]}]},
    postback={request, developer}};
  true -> []
  end,
  if Admin ->
  #link{
    data_fields=[{<<"data-toggle">>,<<"tooltip">>}], title= <<"administrator">>,
    body=#span{class=["icon-stack", "icon-2x", blue], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-wrench icon-light"]}]}}; true->[] end
  ]}].

payments(Who, What) -> [
  if Who == What ->[
    #h3{class=[blue], body= <<"Payments">>},
    #table{class=[table, "table-hover", payments],
      header=[#tr{cells=[#th{body= <<"Date">>}, #th{body= <<"Status">>}, #th{body= <<"Price">>}, #th{body= <<"Game">>}]}],
      body=[[begin
        {{Y, M, D}, _} = calendar:now_to_datetime(Py#payment.start_time),
        Date = io_lib:format("~p ~s ~p", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
        #tr{cells= [
          #td{body= [Date]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end, float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{url="/product?id="++Id,body= Title}} ]} 
      end || #payment{product=#product{id=Id, title=Title, price=Price, currency=Cur}} = Py <-kvs_payment:payments(What#user.email) ]]}];
  true -> [
    #h3{class=[blue], body= <<"Recent activity">>},
      myreviews:reviews(What)
  ] end ].

api_event(Name,Tag,Term) -> error_logger:info_msg("dashboard Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({request, Feature}) -> 
  error_logger:info_msg("request feature ~p", [Feature]),
  User =wf:user(),
  case kvs:get(acl, {feature, admin}) of {error, not_found} -> wf:update(alerts,index:error("system has no administrators yet"));
    {ok,#acl{id=Id}} ->
      Recipients = [{user, User#user.email, lists:keyfind(direct, 1, User#user.feeds)} | lists:flatten([
        case kvs:get(user, Accessor) of {error, not_found} -> []; {ok, U} -> {Type, Accessor, lists:keyfind(direct, 1, U#user.feeds)} end
      || #acl_entry{accessor={Type,Accessor}, action=Action} <- kvs_acl:entries(Id), Action =:= allow])],

      EntryId = kvs:uuid(),
      From = case wf:user() of undefined -> "anonymous"; User-> User#user.email end,
      [msg:notify([kvs_feed, RoutingType, To, entry, EntryId, add],
                  [#entry{id={EntryId, FeedId},
                          entry_id=EntryId,
                          feed_id=FeedId,
                          created = now(),
                          to = {RoutingType, To},
                          from=From,
                          type={feature, Feature},
                          media=[],
                          title= <<"Request">>,
                          description= <<"">>,
                          shared=""}, skip, skip, skip, direct]) || {RoutingType, To, {_, FeedId}} <- Recipients],

      error_logger:info_msg("Recipients ~p", [Recipients]) end,
  ok;
event({read, reviews, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

process_delivery([user,To,entry,_,add],
                 [#entry{type=T},Tid, Eid, MsId, TabId])->
  User = wf:user(),
  What = case kvs:get(user,To) of {error, not_found}-> #user{}; {ok, U} -> U end,
  error_logger:info_msg("[profile]~p receive ADD entry from ~p", [User#user.email, What#user.email]),
  if What#user.email == User#user.email ->
    error_logger:info_msg("update ui "),
    wf:update(alerts, index:error(io_lib:format("~p", [T]) ++" requested")),
    wf:update(side_menu, dashboard:sidebar_menu(User, What , profile, [])); true -> ok end;
process_delivery(_R, _M) -> skip.
