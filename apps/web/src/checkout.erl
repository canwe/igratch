-module(checkout).
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").

-include("records.hrl").
-include("states.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"buy mobile">>},{body, body()}]}.

body() ->
  % payment confirmation data PID/Order
  % PId = wf:qs(<<"pid">>),
  % OrderGUID = wf:qs(<<"order">>),
    User = wf:user(),

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[

            case wf:qs(<<"sid">>) of undefined ->
                Order = case wf:session(?USR_ORDER(User#user.email)) of undefined -> []; O->O end,
                Status = case wf:qs(<<"status">>) of <<"approve">> -> done; _ -> failed end,
                {_, Cid} = lists:keyfind(cart, 1, element(#iterator.feeds, User)),
                [case kvs:get(payment, Id) of {error,_}-> ok;
                {ok,_} ->
                    msg:notify([kvs_payment, user, User#user.email, set_state], {Id, Status, paypal}),

                    if Status == done ->
                        msg:notify([kvs_feed, user, Email, entry, {P#product.id, Cid}, delete], []);true -> ok end
                 end || #payment{id=Id, product=P, user_id=Email} <- Order],

                case Status of done -> wf:redirect("/profile");
                _-> [index:error(<<"payment was rejected, try again">>),
                    #panel{class=["btn-toolbar", "text-center"], body=[
                        #link{class=?BTN_INFO, body= <<"try again">>, url="/shopping_cart"} ]}] end;

            Id ->
                SelectionKey = binary_to_list(Id),
                Visible = case wf:qs(<<"vid">>) of undefined -> "";Vid -> binary_to_list(Vid) end,
                Selection = sets:from_list(wf:session(SelectionKey)),

                Products = lists:flatten([case kvs:get(product, Eid) of undefined -> []; {ok, P} -> P end
                    || {Eid,_}=Pid <- wf:session(Visible), sets:is_element(wf:to_list(erlang:phash2(Pid)), Selection)]),

                Total = lists:foldl(fun(#product{}=P, Sum)-> P#product.price+Sum end, 0, Products),
                Order = [#payment{id=kvs_payment:payment_id(),
                                  user_id=User#user.email,
                                  product_id = P#product.id,
                                  product=P,
                                  info=paypal} || #product{}=P <- Products],
                if length(Order) > 0 ->
                    #panel{class=["row-fluid", dashboard], body=[
                        #panel{class=[span9], body=[
                            #h3{class=["feed-title"], body= [<<"Checkout: ">>, #small{body= <<"review order">>}]},
                            dashboard:section(payment(), "icon-usd")
                        ]},

                        #panel{class=[span3], body=[
                            #panel{class=["well"], body=[
                                #h3{body= <<"Total: ">>},
                                #span{class=["icon-usd"]},float_to_list(Total/100, [{decimals, 2}]),
                                #blockquote{body= [#span{body= <<"continue to PayPal">>}]},
                                #panel{class=["btn-toolbar", "text-center"], body=[
                                    #link{class=[btn, "btn-warning"],
                                            body= <<"continue">>,
                                            postback={buy, Order}} ]} ]} 
                        ]} ]};
                true -> index:error(<<"order is empty">>) end end ]} ]}] ++ index:footer().

payment() -> [
  #h3{body= <<"Pay with">>},
  #select{class=[selectpicker], body=[
    #option{body= <<"PayPal">>, value="paypal"} 
  ]} ].

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
event({buy, Order}) ->
  case wf:user() of undefined -> wf:redirect("/login"); #user{email=Email} ->
    [msg:notify([kvs_payment, user, Email, add], {Pm}) || #payment{} = Pm <- Order],
    wf:session(?USR_ORDER(Email), Order),
    wf:redirect("/fakepp")
  end;

event(Event) -> error_logger:info_msg("[buy_mobile]Page event: ~p", [Event]), ok.
process_delivery(_R, _M) -> skip.


