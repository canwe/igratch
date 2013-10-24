-module(store).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").

-include("records.hrl").
-include("states.hrl").

-jsmacro([on_show/0,show/1]).

on_show() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) -> jq(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

feed_states()-> [
    {?FEED(product), ?PRODUCTS_FEED} |
    [case lists:keyfind(products, 1, Feeds) of false -> {ok, ok};
    {_,Id} -> {Id, ?STORE_FEED(Id)} end || #group{scope=Scope, feeds=Feeds} <- kvs:all(group), Scope==public] ].

main() -> #dtl{file="prod", bindings=[{title,<<"Store">>},{body, body()}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(on_show()),
    wf:wire(show(case wf:qs(<<"id">>) of undefined -> "'all'"; T ->  "'"++wf:to_list(T)++"'" end)),
    Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #link{url="#all", data_fields=?DATA_TAB},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9, "tab-content"], body=[
                    #panel{id=all, class=["tab-pane"]},
                    [#panel{id=Id, class=["tab-pane"]} || #group{id=Id} <- Groups] ]},
                #panel{class=[span3]}]} ]} ]} ] ++ index:footer().

header(Groups, Current) -> [
    lists:dropwhile(fun(E)-> E== <<" / ">> end, [begin [ <<" / ">>,
        #link{url="#"++Id, body=[#span{class=["icon-asterisk"]}, Name],
            data_fields=?DATA_TAB,
            class=[if Current==Id-> "text-warning"; true -> "" end]}]
    end || #group{id=Id, name=Name} <- Groups])].

feed(Group) ->
    Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],
    State = case kvs:get(group, Group) of {error,_} -> ?PRODUCTS_FEED;
        {ok, G} -> case lists:keyfind(products, 1, element(#iterator.feeds, G)) of false -> false;
            {_, Id} -> ?STORE_FEED(Id) end end,

    #feed_ui{icon=["icon-home ", "icon-large ", if Group =="all"-> "text-warning"; true-> "" end],
        icon_url="#all",
        title=[header(Groups, Group)],
        state=State}.

%% Render store elements
render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=store}=State}) ->
    case kvs:get(product, E#entry.entry_id) of {error, _} -> wf:render(#panel{body= <<"error displaying item">>});
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
        store_element(Id, P, State) end;

render_element(#div_entry{entry=#product{}=P, state=#feed_state{view=store}=State}) ->
    Id = wf:to_list(erlang:phash2(element(#product.id, P))),
    store_element(Id, P, State);
render_element(E)-> error_logger:info_msg("[store] render -> feed_ui"),feed_ui:render_element(E).

store_element(Id, P,State) ->
    Media = media(P#product.cover),

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
                #link{style="color:#9b9c9e;", body=P#product.title, postback={read, product, P#product.id}}}},

            #p{id=?EN_DESC(Id), body=product_ui:shorten(P#product.brief)} ]},

        #panel{class=[span2, "text-center"], body=[
            #h3{style="",
                body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]},
            #link{class=[btn, "btn-warning"], body=[<<"add to cart">>], postback={add_cart, P, State}} ]} ]).


% Events

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(Id)),
    wf:wire(on_show()),
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event({checkout, Pid}) -> wf:redirect("/checkout?product_id="++Pid);
event({add_cart, #product{}=P, #feed_state{}=S}) ->
    case wf:user() of undefined -> wf:redirect("/login");
    _-> Fs = ?FD_STATE(S)#feed_state{
            enable_selection=true,
            view=cart,
            delegate=shopping_cart},
        Is = #input_state{
            collect_msg = false,
            show_recipients = false,
            entry_type = cart,
            entry_id = P#product.id,
            title = P#product.title,
            description = P#product.brief,
            medias=[media(P#product.cover)]},
        input:event({post, cart, Is, Fs}) end;

event(Event) -> error_logger:info_msg("[store]Page event: ~p", [Event]), ok.

process_delivery(R,M) ->
    User = wf:user(),
    case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
    {_, Id} -> case kvs:get(feed,Id) of {error,_}-> ok;
        {ok, #feed{entries_count=C}} when C==0 -> ok;
        {ok, #feed{entries_count=C}}-> wf:update(?USR_CART(User#user.id), integer_to_list(C)) end end,
    feed_ui:process_delivery(R,M).


media(undefined)-> undefined;
media(File)-> #media{url = File,
    thumbnail_url = filename:join([filename:dirname(File),"thumbnail",filename:basename(File)])}.

short_date(undefined) -> short_date(now());
short_date(Date) ->
    {{Y, M, D}, {_,_,_}} = calendar:now_to_datetime(Date),
    io_lib:format("~s ~p, ~p", [?MONTH(M), D, Y]).
