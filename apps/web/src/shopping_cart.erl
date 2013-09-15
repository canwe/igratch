-module(shopping_cart).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"shopping cart">>},{body, body()}]}.

body()->
    User = wf:user(),
    ShoppingCart = case lists:keyfind(cart, 1, element(#iterator.feeds, User)) of false -> [];
        {_, CId} ->  #feed_ui{title= <<"shopping cart">>, icon="icon-shopping-cart", state=?FD_STATE(CId), header=[]} end,
    WhishList  = case lists:keyfind(whishlist, 1, element(#iterator.feeds, User)) of false -> [];
        {_, WId} -> #feed_ui{title= <<"whish list">>, icon="icon-list", state=?FD_STATE(WId), header=[]} end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #panel{class=["page-header"], body=[
                #panel{class=["btn-toolbar"], body=[
                    #link{class=[btn, "btn-info"], body= <<"Continue shopping">>, url="/store"} ]} ]},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9], body=[
                    ShoppingCart,
                    #panel{class=["hero-unit", "text-center"], body=[#p{body= <<"">>}]},
                    WhishList]},

                #panel{class=[span3], body=[
                    #panel{class=["well","pricing-table", "product-price", "text-center"], body=[
                        #h3{body=#span{class=["text-info"],body= <<"Order Summary">>}},
                        case [] of [] -> []; P -> [
                            #h3{class=["pricing-table-price", "product-price-price"], body=[
                                #span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},

                            #list{class=["pricing-table-list", "product-price-list", unstyled], body=[
                                #li{body= <<"Promotional discount: -$0.00">>},
                                #li{body= <<"Estimated shipping:    $0.00">>},
                                #li{body= <<"Estimated tax:         $0.00">>},
                                #li{body= <<"Remaining ballance:    $0.00">>}]},

                            #h3{body= <<"Estimated total: ">>},
                            #h3{class=["pricing-table-price", "product-price-price"], body=[
                                #span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},

                            #link{class=[btn, "btn-large", "btn-warning"], body= <<"checkout">>,
                                url="/checkout?product_id="++P#product.id} ] end ]} ]} ]} ]} ]} ] ++ index:footer().

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, {Id,_}})-> wf:redirect("/product?id="++Id);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).

