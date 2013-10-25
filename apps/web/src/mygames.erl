-module(mygames).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include_lib("feed_server/include/records.hrl").

-include("records.hrl").
-include("states.hrl").

feed_states() ->
    User = wf:user(),
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,
    case lists:keyfind(products, 1, Feeds) of false -> []; {_,Id} -> [{Id, ?MYGAMES_FEED(Id)}] end.

input_states() ->
    User = wf:user(),
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,
    case lists:keyfind(products, 1, Feeds) of false -> []; {_,Id} -> [{Id, ?MYGAMES_INPUT(Id)}] end.

main()-> #dtl{file="prod", bindings=[{title,<<"my games">>},{body, body()}]}.

body()->
    User = wf:user(),
    Nav = {User, mygames, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    index:header() ++ dashboard:page(Nav,
        case lists:keyfind(products, 1, Feeds) of false -> [];
        {_, Id} ->
            FeedState = proplists:get_value(Id, feed_states()),
            InputState = proplists:get_value(Id, input_states()),

            #feed_ui{title= <<"my games">>,
                     icon="icon-gamepad",
                     state=FeedState,
                     header=[ #input{state=InputState} ]} end) ++ index:footer().

%% Render feed items

render_element(#div_entry{entry=#entry{id={Eid,_}}=E, state=#feed_state{view=product}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    case kvs:get(product, Eid) of {error,_}-> wf:render(["no product"]);
    {ok, P} ->
        Fid = State#feed_state.container_id,
        UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
        From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
        InputState = (proplists:get_value(Fid, input_states()))#input_state{update=true},

        wf:render([#panel{class=[span3, "article-meta"], body=[
            #h4{class=[blue], body= <<"">>},
            #p{body=#link{body=From, url= "/profile?id="++wf:to_list(E#entry.from)}},
            #p{body=[#span{body= product_ui:to_date(E#entry.created)} ]},
            #p{body=[#link{url="#",body=[
                #span{class=[?ID_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]},
            #h4{body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}]) ]} ]},

            #panel{id=?EN_MEDIA(UiId), class=[span4,"media-pic"], body=#entry_media{media=E#entry.media, mode=reviews}},

            #panel{class=[span4, "article-text"], body=[
                #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
                    #link{style="color:#9b9c9e;", postback={read, product, Id}, body=E#entry.title}}},
                #p{id=?EN_DESC(UiId), body=E#entry.description}
            ]},
            #panel{class=[span1], body=[
                #link{body= <<"edit">>, class=[btn, "btn-block"], delegate=input, postback={edit, P, InputState}},
                #link{body= <<"more">>, class=[btn, "btn-block"], postback={read, product, Id}} ]} ]) end;
render_element(E)-> feed_ui:render_element(E).

%% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
