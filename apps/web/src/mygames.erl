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

-define(IN_STATE(Id), #input_state{
    id=?FD_INPUT(Id),
    fid = Id,
    role=group,
    entry_type=product,
    show_upload=true,
    show_price=true,
    simple_body=true,
    class= "feed-table-header",
    control_title = <<"Create">>,
    placeholder_rcp= <<"Categories">>,
    placeholder_ttl= <<"Game title">>,
    placeholder_box= <<"Brief description">> }).

feed_states() ->
    User = wf:user(),
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    case lists:keyfind(products, 1, Feeds) of false -> [];
        {_, Id} -> [
            {Id, ?FD_STATE(Id)#feed_state{
                view=product,
                html_tag=panel,
                entry_id=#entry.entry_id,
                enable_selection=true,
                delegate=mygames}}] end.

main()-> #dtl{file="prod", bindings=[{title,<<"my games">>},{body, body()}]}.

body()->
    User = wf:user(),
    Nav = {User, mygames, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    index:header() ++ dashboard:page(Nav,
        case lists:keyfind(products, 1, Feeds) of false -> [];
        {_, Id} ->
            #feed_ui{title= <<"My games">>,
                icon="icon-gamepad",
                state=proplists:get_value(Id, feed_states()),
                header=[ #input{state=?IN_STATE(Id), post_btn= <<"create">>} ]} end ) ++index:footer().

%% Render products

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=product}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    case kvs:get(product, Id) of {error,_}-> wf:render([]);
    {ok, P} ->
        Fid = State#feed_state.container_id,
        UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
        From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,

        wf:render([#panel{class=[span3, "article-meta"], body=[
            #h3{class=[blue], body= <<"">>},
            #p{body=#link{body=From, url= "/profile?id="++wf:to_list(E#entry.from)}},
            #p{body=[#span{body= product_ui:to_date(E#entry.created)} ]},
            #p{body=[#link{url="#",body=[
                #span{class=[?ID_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]} ]},

            #panel{id=?EN_MEDIA(UiId), class=[span4, "media-pic"], body =#entry_media{media=E#entry.media, mode=reviews}},

            #panel{class=[span4, "article-text"], body=[
                #h3{body=#link{postback={read, product, Id}, body=
                    #span{id=?EN_TITLE(UiId), class=[title], body=E#entry.title}}},

                #p{id=?EN_DESC(UiId), body=product_ui:shorten(E#entry.description)}
            ]},
            #panel{class=[span1], body=[
                #link{body= <<"edit">>, class=[btn], delegate=input,
                    postback={edit, P, State, ?IN_STATE(Fid)#input_state{update=true}}} ]} ]) end;
render_element(E)-> feed_ui:render_element(E).

%% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
