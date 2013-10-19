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

main()-> #dtl{file="prod", bindings=[{title,<<"my games">>},{body, body()}]}.

body()->
    User = wf:user(),
    Nav = {User, mygames, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    index:header() ++ dashboard:page(Nav,
        case lists:keyfind(products, 1, Feeds) of false -> [];
        {_, Id} ->
            Fs = ?FD_STATE(Id)#feed_state{
                view=product,
                html_tag=panel,
                entry_id=#entry.entry_id,
                enable_selection=true,
                delegate=mygames
            },
            Is = #input_state{id=?FD_INPUT(Id), show_upload=true, entry_type=product, show_price=true, simple_body=true},
            #feed_ui{title= <<"My games">>, icon="icon-gamepad", state=Fs, header=[
                #panel{id=Is#input_state.id, body= input(Is,Fs,<<"New Game">>,[])} ]} end ) ++index:footer().

%% Render products

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=product}=State}) ->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    error_logger:info_msg("render element in MYGAMES ~p ~p", [Id, UiId]),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,
    {ok, P} = kvs:get(product, Id),
    Ctl = [
        #link{body= <<"edit">>, postback={edit, P, State}}
    ],
    wf:render(feed_ui:article(product, {Id, UiId}, From, E#entry.created, E#entry.media, E#entry.title, E#entry.description, Ctl));
render_element(E)-> feed_ui:render_element(E).

input(#input_state{} = Is, #feed_state{}=Fs, Title, Recipients) ->
    #input{ title= Title,
            placeholder_rcp= <<"Categories">>,
            placeholder_ttl= <<"Game title">>,
            placeholder_box= <<"Brief description">>,
            post_btn= <<"create">>,
            role=group,
            state=Is,
            feed_state=Fs,
            recipients = Recipients,
            class=["feed-table-header"]}.

media(undefined)-> [];
media(File)-> [#media{url = File,
    thumbnail_url = filename:join([filename:dirname(File),"thumbnail",filename:basename(File)])}].

%% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({edit, P=#product{}, Fs=#feed_state{}}) ->
    Groups = string:join([case kvs:get(group, G) of {error,_}-> "";
        {ok, #group{id=Id, name=Name}}-> "group"++wf:to_list(Id)++"="++wf:to_list(Name) end
        || #group_subscription{where=G} <- kvs_group:participate(P#product.id)], ","),

    Is = #input_state{
        id = ?FD_INPUT(Fs#feed_state.container_id),
        entry_id = P#product.id,
        update=true,
        title = P#product.title,
        description = P#product.brief,
        price = P#product.price,
        medias = media(P#product.cover),
        show_upload=true,
        entry_type=product,
        show_recipients=true,
        show_price=true,
        simple_body=true},

    error_logger:info_msg("Initial value: ~p", [Groups]),
    error_logger:info_msg("Update ~p", [Is#input_state.id]),
    error_logger:info_msg("Cover ~p", [P#product.cover]),

    wf:update(Is#input_state.id, input(Is, Fs, <<"Update game">>, Groups));

event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event(Event) -> error_logger:info_msg("[mygames]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
