-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

main()->#dtl{file="prod", bindings=[{title,<<"Store">>},{body,body()},
                                    {css,?STORE_CSS},{less,?LESS},{bootstrap, ?STORE_BOOTSTRAP}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(index:on_shown()),

    Groups = lists:flatmap(fun(#group{scope=Scope, feeds=Feeds, name=Name})->
        case lists:keyfind(products,1, Feeds) of
        {_,Fid} when Scope==public ->
            case wf:cache({Fid,?CTX#context.module}) of undefined -> wf:cache({Fid,?CTX#context.module}, ?STORE_FEED(Fid)), [{Name, Fid}];
                _-> [{Name,Fid}] end; _ -> [] end end, kvs:all(group)),

    All = case wf:cache({?FEED(product),?CTX#context.module}) of undefined ->
        FS = ?PRODUCTS_FEED, wf:cache({?FEED(product),?CTX#context.module},FS), FS; F->F end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #h4{class=[span12, "page-header-sm"], body=[
                #link{url="#all", body=[#i{class=["icon-home", "icon-large", "text-warning"]}], data_fields=?DATA_TAB},
                #small{body= string:join([wf:to_list(wf:render(
                    #link{url="#"++wf:to_list(Fid),body=[#i{class=["icon-asterisk"]}, Name], data_fields=?DATA_TAB})) 
                        || {Name,Fid} <- Groups], " / ")} ]},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9, "tab-content"], body=[
                    #panel{id=all, class=["tab-pane", active], body=[
                        #feed_ui{icon=["icon-home ", "icon-large ", "text-warning"],
                                icon_url="#all",
                                class=articles,
                                title=[],
                                state=All}]},
                    [#panel{id=wf:to_list(Fid), class=["tab-pane"]}|| {_,Fid} <- Groups]]},
                #panel{class=[span3]}]} ]} ]} ] ++ index:footer().

%% Render store elements
render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=store}=State}) ->
    case kvs:get(product, E#entry.entry_id) of {error, _} -> wf:render(#panel{body= <<"error displaying item">>});
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
        store_element(Id, P) end;

render_element(#div_entry{entry=#product{}=P, state=#feed_state{view=store}}) ->
    Id = wf:to_list(erlang:phash2(element(#product.id, P))),
    store_element(Id, P);

render_element(E)-> error_logger:info_msg("[store] render -> feed_ui"),feed_ui:render_element(E).

store_element(Id, P) ->
    Media = input:media(P#product.cover),

    {FromId, From} = case kvs:get(user, P#product.owner) of
        {ok, U} -> {P#product.owner, U#user.display_name};
        {error, _} -> {P#product.owner,P#product.owner} end,

    wf:render([
        #panel{class=[span2, "article-meta"], body=[
            #panel{body=[
                #link{body=From, url= "/profile?id="++wf:to_list(FromId)},
                #panel{body= short_date(P#product.created)} ]},
            #p{body=[#link{body=[#i{class=["icon-windows", "icon-large"]}]}]},
            #p{body=[#link{url="#",body=[
                #span{class=[?EN_CM_COUNT(Id)], body=
                    integer_to_list(kvs_feed:comments_count(product, P#product.id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]} ]},

        #panel{id=?EN_MEDIA(Id), class=[span3, "media-pic"], body=#entry_media{media=Media, mode=store}},

        #panel{class=[span5, "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, url=?URL_PRODUCT(P#product.id)}}},

            #p{id=?EN_DESC(Id), body=index:shorten(P#product.brief)} ]},

        #panel{class=[span2, "text-center"], body=[
            #h3{style="",
                body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},
            #link{class=[btn, "btn-warning"], body=[<<"add to cart">>], postback={add_cart, P}} ]} ]).


% Events

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case Id of "all" -> []; _ -> wf:update(Id, index:feed(list_to_integer(Id),articles)) end,
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({add_cart, #product{}=P}) ->
    case wf:user() of undefined -> wf:redirect("/login");
    _->
        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = cart,
            entry_id = P#product.id,
            title = P#product.title,
            description = P#product.brief,
            medias=[input:media(P#product.cover)]},

        input:event({post, cart, Is}) end;

event(Event) -> error_logger:info_msg("[store]Page event: ~p", [Event]), ok.

process_delivery(R,M) ->
    User = wf:user(),
    case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
    {_, Id} -> case kvs:get(feed,Id) of {error,_}-> ok;
        {ok, #feed{entries_count=C}} when C==0 -> ok;
        {ok, #feed{entries_count=C}}-> wf:update(?USR_CART(User#user.id), integer_to_list(C)) end end,
    feed_ui:process_delivery(R,M).

short_date(undefined) -> short_date(now());
short_date(Date) ->
    {{Y, M, D}, {_,_,_}} = calendar:now_to_datetime(Date),
    io_lib:format("~s ~p, ~p", [?MONTH(M), D, Y]).
