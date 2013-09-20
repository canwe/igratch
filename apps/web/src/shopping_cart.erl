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

body()-> index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #panel{class=["page-header"], body=[
                #panel{class=["btn-toolbar"], body=[
                    #link{class=[btn, "btn-info"], body= <<"Continue shopping">>, url="/store"} ]} ]},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9], body=[
                    case wf:user() of undefined -> [];
                    User -> [case lists:keyfind(cart, 1, element(#iterator.feeds, User)) of false -> [];
                        {_, CId} ->
                            State = ?CART_STATE(CId),
                            #feed_ui{
                                title= <<"shopping cart">>,
                                icon="icon-shopping-cart icon-large blue",
                                selection_ctl=checkout_ctl(State),
                                state=State,
                                header=[]} end,
                            #panel{
                                class=["hero-unit", "text-center"],
                                body=[#p{body= <<"">>}]},
                        case lists:keyfind(whishlist, 1, element(#iterator.feeds, User)) of false -> [];
                        {_, WId} ->
                            #feed_ui{
                                title= <<"whish list">>,
                                icon="icon-list icon-large blue",
                                state=?CART_STATE(WId),
                                header=[]} end] end ]},

                #panel{class=[span3], body=[
                    #panel{class=["well","pricing-table", "product-price", "text-center"], body=[
                        case [] of [] -> []; P -> [
                            #h3{body=#span{class=["text-info"],body= <<"Order Summary">>}},
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

%% Render shopping cart elements

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=cart}=State}) ->
    case kvs:get(product, E#entry.entry_id) of
        {ok, P} -> cart_item(P, State);
        {error,_}-> <<"item not available">> end;
render_element(#div_entry{entry=#product{}=P, state=#feed_state{view=cart}=State}) ->
    cart_item(P, State);
render_element(E)-> feed_ui:render_element(E).

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, product, Id})-> wf:redirect("/product?id="++Id);
event({checkout, #feed_state{}=S}) -> ok;
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).

cart_item(P, State) ->
    Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
    wf:render([
        #panel{id=?EN_MEDIA(Id), class=[span4, "media-pic"], style="margin:0;",
            body=#entry_media{media=media(P#product.cover), mode=store}},

        #panel{class=[span5, "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, postback={read, product, P#product.id}}}},

            #p{id=?EN_DESC(Id), body=product_ui:shorten(P#product.brief)} ]},

        #panel{class=[span3, "text-center"], body=[
            #h3{style="",
                body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},
                #link{class=?BTN_INFO, body= <<"to wishlist">>, postback={move_to_list, Id, State}}
        ]} ]).

checkout_ctl(State) ->
    error_logger:info_msg("State: ~p", [State#feed_state.container_id]),
    [
    #link{id=?FD_CHKOUT(State#feed_state.container_id), class=[btn, "btn-warning"], body= <<"checkout">>,
        data_fields=?TOOLTIP, title= <<"checkout">>, postback={checkout, State}, delegate=shopping_cart}
    ].

media(undefined)-> #media{};
media(File)-> #media{url = File,
    thumbnail_url = filename:join([filename:dirname(File),"thumbnail",filename:basename(File)])}.

short_date(undefined) -> short_date(now());
short_date(Date) ->
    {{Y, M, D}, {_,_,_}} = calendar:now_to_datetime(Date),
    io_lib:format("~s ~p, ~p", [?MONTH(M), D, Y]).
