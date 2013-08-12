-module(profile).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include("records.hrl").

main() -> 
case wf:user() of undefined -> wf:redirect("/login"); _ -> [#dtl{file = "prod",  ext="dtl",bindings=[{title,<<"Account">>},{body,body()}]}] end.

body() -> index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(profile)},
        #panel{class=[span9], body=[
          dashboard:section(profile_info(wf:user()), "icon-user"),
          dashboard:section(payments(wf:user()), "icon-list")
        ]} ]} } }
  ] ++ index:footer().

profile_info(U) -> 
      {{Y, M, D}, _} = calendar:now_to_datetime(U#user.register_date),
      RegDate = io_lib:format("~p ~s ~p", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
      Mailto = if U#user.email==undefined -> []; true-> iolist_to_binary(["mailto:", U#user.email]) end,
      [
      #h3{class=[blue], body= <<"Profile">>},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span4, "dashboard-img-wrapper"], body=
        #panel{class=["dashboard-img"], body=
          #image{class=[], alt="",
            image = re:replace(U#user.avatar, <<"_normal">>, <<"">>, [{return, list}]) ++"?sz=180&width=180&height=180&s=180", width= <<"180px">>, height= <<"180px">> }} },
      #panel{class=[span8, "profile-info-wrapper"], body=
        #panel{class=["form-inline", "profile-info"], body=[
        #panel{body=[#label{body= <<"Name:">>}, #b{body= U#user.display_name}]},
        #panel{show_if=U#user.email=/=undefined, body=[#label{body= <<"Mail:">>}, #link{url= Mailto, body=#strong{body= U#user.email}}]},
        #panel{body=[#label{body= <<"Member since ">>}, #strong{body= RegDate}]},
        #b{class=["text-success"], body= <<"Active">>},
        #p{body=[
          #span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-stack-base", "icon-circle"]},#i{class=["icon-user icon-light"]}]},
          #span{class=["icon-stack", "icon-2x"], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-list-alt icon-light"]}]},
          #span{class=["icon-stack", "icon-2x", blue], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-wrench icon-light"]}]}
        ]},
        #panel{class=["btn-toolbar", "control-buttons"], body=[
          #link{url= <<"#">>, class=[btn, "btn-warning", "btn-large", disabled], body=[#i{class=["icon-share"]}, <<" Upgrade">>]}
        ]} ]}}]} ].

payments(User) -> [
  #h3{class=[blue], body= <<"Payments">>},
  #table{class=[table, "table-hover", payments],
    header=[#tr{cells=[
      #th{body= <<"Date">>},
      #th{body= <<"Status">>},
      #th{body= <<"Price">>},
      #th{body= <<"Game">>}
    ]}],
    body=[[
    begin
      {{Y, M, D}, _} = calendar:now_to_datetime(Py#payment.start_time),
      Date = io_lib:format("~p ~s ~p", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
    #tr{cells= [
      #td{body= [Date]},
      #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],body= [atom_to_list(Py#payment.state)]},
      #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end, float_to_list(Price/100, [{decimals, 2}])]},
      #td{body=#link{url="/product?id="++Id,body= Title}} ]} 
    end || #payment{product=#product{id=Id, title=Title, price=Price, currency=Cur}} = Py <-kvs_payment:payments(User#user.email)

  ]]} ].

api_event(Name,Tag,Term) -> error_logger:info_msg("dashboard Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).
event(init) -> [].
