-module(checkout).
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"buy mobile">>},{body, body()}]}.

body() ->
  % payment confirmation data PID/Order
  % PId = wf:qs(<<"pid">>),
  % OrderGUID = wf:qs(<<"order">>),

  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[ #h1{class=[], body= [<<"Checkout: ">>, #small{body= <<"review order">>}]} ]},
      case wf:qs(<<"product_id">>) of undefined -> index:error(<<"no product to checkout">>);
        Id -> error_logger:info_msg("id ~p", [Id]),
          case kvs:get(product, binary_to_list(Id)) of
            {error, not_found} -> index:error(<<"not_found">>);
            {ok, P} -> 
              PurchaseId = kvs_payment:payment_id(),
              #panel{class=["row-fluid", dashboard], body=[
                #panel{class=[span9], body=[
                  dashboard:section(address(), "icon-home"),
                  dashboard:section(payment(), "icon-usd"),
                  dashboard:section(shipping(),"icon-truck")
                ]},

                #panel{class=[span3], body=[
                  #panel{class=["well"], body=[
                    #h3{body= <<"Total: ">>},
                    #span{class=["icon-usd"]},float_to_list(P#product.price/100, [{decimals, 2}]),
                    #blockquote{body= [#span{body= <<"continue to PayPal">>}]},
                    #panel{class=["btn-toolbar", "text-center"], body=[
                      #link{class=[btn, "btn-large", "btn-warning"], body= <<"continue">>, postback={buy, PurchaseId, P}}
                    ]}
                  ]}
                ]}
              ]} end end
  ]} ]} ] ++ index:footer().

address() -> [
  #h3{body= <<"Ship address">>},
  #address{body=[
    #strong{body= <<"Synrc Research Center s.r.o.">>}, #br{},
    <<"Roháčova 141/18, Praha 3">>, #br{},
    <<"13000, Сzech Republic">>, #br{},
    #abbr{title="Phone", body= <<"P:">>}, <<"(044) 361-3263">>]},
    #link{class=[muted],body= <<"change shipping address">>}
  ].

payment() -> [
  #h3{body= <<"Pay with">>},
  #select{class=[selectpicker], body=[
    #option{body= <<"PayPal">>, value="paypal"} 
  ]} ].
shipping()-> [
  #h3{body= <<"Shipping">>},
  #panel{body= <<"+ shipping information">>} 
  ].

process_result(success) ->
%  case kvs_membership:get_purchase(PurchaseId) of
%    {ok, Purchase} -> msg:notify(["purchase", "user", wf:user(), "set_purchase_state"], {element(2,Purchase), done, mobile});
%      _ -> "Purchase Not Found"
%  end;
  ok;
process_result(failure) -> not_found.

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({buy, PurchaseId, #product{}=Product}) ->
  User = wf:user(),
  error_logger:info_msg("Continue checkout ~p, Product ~p", [PurchaseId,Product#product.id]),

  Pm = #payment{id = PurchaseId,
    user_id = User#user.email,
    product = Product,
    info = paypal
  },
  msg:notify([kvs_payment, user, User#user.email, add], {Pm});
  % submit form\redirect;

event(Event) -> error_logger:info_msg("[buy_mobile]Page event: ~p", [Event]), ok.
%event(Event)-> buy:event(Event).

process_delivery([user, UserId, add], {Pm}) ->
  error_logger:info_msg("Payment added: ~p", [Pm]),
  ok;
process_delivery(_R, _M) -> skip.


