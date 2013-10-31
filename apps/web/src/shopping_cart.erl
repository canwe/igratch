-module(shopping_cart).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").
-include("paypal.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"shopping cart">>},
                                      {body, body()},{css,?CART_CSS},{less,?LESS},{bootstrap, ?CART_BOOTSTRAP}]}.

body()->
    case wf:user() of undefined -> wf:redirect("/login");
    User ->
    State = case lists:keyfind(cart, 1, element(#iterator.feeds, User)) of false -> undefined;
        {_, Cid} -> case wf:session({Cid, ?CTX#context.module}) of undefined ->
            CS = ?CART_STATE(Cid), wf:session({Cid, ?CTX#context.module},CS), CS; Cs-> Cs end end,

    WishState = case lists:keyfind(wishlist, 1, element(#iterator.feeds, User)) of false -> undefined;
        {_, Wid} -> case wf:session({Wid, ?CTX#context.module}) of undefined ->
            Ws = ?CART_STATE(Wid)#feed_state{view=store}, wf:session({Wid, ?CTX#context.module}, Ws), Ws; WS-> WS end end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #h4{class=["row-fluid", "page-header-sm"], body=[
                #link{class=?BTN_INFO, body= <<"continune shopping">>, url="/store"},
                #small{id=alert, body=case wf:qs(<<"token">>) of undefined -> <<"">>; Tk ->
                    case paypal:get_express_details([{"TOKEN", Tk}]) of {error,E} ->
                        alert("payment " ++ proplists:get_value(?PP_TRANSACTION, E) 
                            ++" "++ proplists:get_value(?PP_ACK, E)
                            ++ " "++ proplists:get_value(?PP_ERROR_MSG,E));
                    Details ->
                        CorrelationId = proplists:get_value(?PP_TRANSACTION, Details),
                        CheckoutStatus = proplists:get_value(?PP_STATUS, Details),
                        alert("payment " ++ CorrelationId ++ " status:" ++CheckoutStatus) end end}]},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9], body=[
                    #feed_ui{title= <<"shopping cart">>,
                            icon="icon-shopping-cart icon-large blue",
                            state=State},

                    #panel{class=["hero-unit", "clearfix"], body= <<"">>},
                        #feed_ui{title= <<"whish list">>,
                            icon="icon-list blue",
                            state=WishState,
                            header=[]}]},

                #panel{id=?USR_ORDER(State#feed_state.container_id), class=[span3], body=order_summary(State)} ]}]}]}
    ] ++ index:footer() end.

order_summary(#feed_state{visible_key=Visible}=S) ->
    case wf:session(Visible) of [] ->
        case kvs:get(S#feed_state.container, S#feed_state.container_id) of {error,_} -> ok;
            {ok, Feed} -> Entries = kvs:entries(Feed, S#feed_state.entry_type, S#feed_state.page_size),
                wf:session(Visible, [element(S#feed_state.entry_id, E)|| E<-Entries]) end; _-> ok end,

    {Items, Total} = lists:mapfoldl(fun({Id,_}, In)->
        case kvs:get(product,Id) of {error,_} -> {[], In};
            {ok, #product{price=Price, title=Title}} -> {
                [#panel{body=[#b{body=Title}, #span{class=["pull-right"], body=[
                    #span{class=["icon-usd"]},
                    float_to_list(Price/100, [{decimals,2}]) ]}]}
                ], In+Price} end end,
        0, [Pid || Pid <- ordsets:from_list(case wf:session(Visible) of undefined->[];I->I end)]),

    #panel{class=[well, "pricing-table", "affix-top"],
           style="width:230px",
           data_fields=[{<<"data-spy">>, <<"affix">>}],
           body=[
        #h4{class=["text-warning", "text-center"], body= <<"Order Summary">>},
        #hr{},
        Items,
        if Total > 0 -> #hr{}; true -> [] end,
        #panel{body=[
            #b{body= <<"Estimated total: ">>},
            #span{class=["pull-right"],
                  body=[#i{class=["icon-usd"]}, float_to_list(Total/100, [{decimals,2}])]} ]},
        #hr{},
        #link{class=[btn, "btn-block", if Total == 0 -> disabled; true -> "" end],
            postback={checkout, Visible},
            body=[#image{image="https://www.paypal.com/en_US/i/btn/btn_xpressCheckout.gif"}]} 
    ]};
order_summary(undefined)->[].

alert(Msg) ->
    #span{class=[alert, fade, in, "alert-danger"],
        style ="margin-left:10px;",body=[
        #span{body= Msg},
        #link{class=[close], url="#", 
              data_fields=[{<<"data-dismiss">>,<<"alert">>}],
              style="float:none;top:0;",
              body= <<"&times;">>}]}.

%% Render elements

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=cart}=State}) ->
    wf:render(case kvs:get(product, E#entry.entry_id) of
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
        error_logger:info_msg("Id: ~p", [Id]),
        [#panel{id=?EN_MEDIA(Id), class=[span4, "media-pic"], style="margin:0;",
            body=#entry_media{media=store:media(P#product.cover), mode=store}},

        #panel{class=[span5, "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, postback={read, product, P#product.id}}}},

            #p{id=?EN_DESC(Id), body=product_ui:shorten(P#product.brief)} ]},

        #panel{class=[span3, "text-center"], body=[
            #h3{style="",
                body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},
                #link{class=?BTN_INFO, body= <<"to wishlist">>, postback={to_wishlist, P, State}} ]}];

    {error,_} -> <<"item not available">> end);
render_element(E)-> store:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, product, Id})-> wf:redirect("/product?id="++Id);

event({to_wishlist, #product{}=P, #feed_state{}=S})->
    User = wf:user(),
    Fid = S#feed_state.container_id,
    case kvs:get(entry, {P#product.id, Fid}) of {error,_}-> ok;
    {ok, E} ->
        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = wishlist,
            entry_id = P#product.id,
            title = P#product.title,
            description = P#product.brief,
            medias=[store:media(P#product.cover)]},

            error_logger:info_msg("Input ~p ~p", [P#product.id, Fid]),

            input:event({post, wishlist, Is}),

            msg:notify( [kvs_feed, User#user.email, entry, delete], [E, Is]) end;

event({to_wishlist, #feed_state{selected_key=Selected, visible_key=Visible}})->
    Selection = sets:from_list(wf:session(Selected)),
    User = wf:user(),
    case lists:keyfind(wishlist, 1, User#user.feeds) of false -> ok;
    {_,Fid} ->
        [case kvs:get(entry, Id) of {error,_} -> ok; 
        {ok, E} ->
            msg:notify( [kvs_feed, user, User#user.email, entry, Eid, add],
                        [E#entry{id={Eid, Fid}, feed_id=Fid}]),

            msg:notify( [kvs_feed, User#user.email, entry, delete], [E])

        end || {Eid,_}=Id <- wf:session(Visible), sets:is_element(wf:to_list(erlang:phash2(Id)), Selection)] end;

event({add_cart, #product{}=P}=M) ->
    store:event(M),
    User = wf:user(),
    case lists:keyfind(wishlist, 1, User#user.feeds) of false -> ok;
    {_,Fid} ->
        case kvs:get(entry, {P#product.id, Fid}) of {error,_}-> ok;
        {ok, E} -> msg:notify([kvs_feed, User#user.email, entry, delete], [E]) end end;

event({checkout, Visible}) ->
    User = wf:user(),
    {Req, {Total,_}} = lists:mapfoldl(fun({Id,_}, {T, In})->
        case kvs:get(product,Id) of {error,_} -> {[], {T, In}};
            {ok, #product{price=Price}=P} ->
                Index = integer_to_list(In),
                PmId = kvs_payment:payment_id(),
                Pm = #payment{id=PmId,
                        user_id = User#user.email,
                        product_id=P#product.id,
                        product = P,
                        info = paypal},
                msg:notify([kvs_payment, user, User#user.email, add], {Pm}),

                {paypal:product_request(Index,PmId,P), {T+Price,In+1}} end end,
        {0,0}, [I || I <- ordsets:from_list(wf:session(Visible))]),

    case paypal:set_express_checkout(lists:flatten(?PP_PAYMENTREQUEST(Total)++Req)) of
        {error,E} -> wf:update(alert, alert(E));_-> ok end;

event(_) -> ok.

process_delivery([entry, {_,Fid}, _]=R, [#entry{}|_]=M)->
    User = wf:user(),
    feed_ui:process_delivery(R,M),
    case feed_ui:feed_state(Fid) of false -> ok;
    State ->
        wf:update(?USR_ORDER(State#feed_state.container_id), order_summary(State)),
        case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
        {_,CFid} when Fid == CFid ->
            case kvs:get(feed,Fid) of
                {ok, #feed{entries_count=C}} when C == 0 -> wf:update(?USR_CART(User#user.id), "");
                {ok, #feed{entries_count=C}} -> wf:update(?USR_CART(User#user.id), integer_to_list(C));
                _ -> ok end;
        _ -> ok end end;

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
