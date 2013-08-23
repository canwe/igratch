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
%  P = #product{price=99.99, id=1},
  [P1|_]=Prods= case wf:session(shopping_cart) of undefined-> [undefined]; Ps-> Ps end,
  error_logger:info_msg("P1~p", [P1]),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
        #panel{class=["btn-toolbar"], body=[
          #link{class=[btn, "btn-info", "btn-large"], body= <<"Continue shopping">>, url="/store"},
          case P1 of indefined -> []; P->#link{class=[btn, "btn-warning", "btn-large"], body= <<"Checkout">>, url="/checkout?product_id="++P#product.id } end
        ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9], body=[
          dashboard:section(cart(Prods), "icon-shopping-cart"),
          #panel{class=["hero-unit", "text-center"], body=[
            #h1{body= <<"Got a question?">>},
            #p{body= <<"want to work with us to move your bussines to the next level? Well, dont be afraid">>},
            #link{class=[btn, "btn-large", "btn-info"], body= <<"contact us">>}
          ]},

          dashboard:section(whishlist(), "icon-list")
        ]},

        #panel{class=[span3], body=[
          #panel{class=["well","pricing-table", "product-price", "text-center"], body=[
            #h3{body=#span{class=["text-info"],body= <<"Order Summary">>}},
            case P1 of undefined -> []; #product{} = P->  [
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

              #link{class=[btn, "btn-large", "btn-warning"], body= <<"checkout">>, url="/checkout?product_id="++P#product.id} ]
            end
          ]}
        ]}
      ]}
    ]}
  ]}
  ] ++ index:footer().


whishlist()->[
  #h3{body= <<"Whish list">>},
  [[#product_entry{entry=E, mode=line, category=Name, controls=product:controls(E)} || E <- kvs:entries(lists:keyfind(products,1,Feeds), undefined, ?PAGE_SIZE)]
  || #group{feeds=Feeds, name=Name} <- kvs:all(group)]
].
cart(Products)->[
  #h3{body= <<"Shopping cart">>},
  [#product_cart{product=P} || P <- Products] ].


event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, {Id,_}})-> wf:redirect("/product?id="++Id);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

process_delivery(R,M) -> product:process_delivery(R,M).

