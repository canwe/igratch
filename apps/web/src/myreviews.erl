-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").
-include("states.hrl").

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()},
                                     {css,?MYREVIEW_CSS},{less,?LESS},{bootstrap, ?MYREVIEW_BOOTSTRAP}]}.

body()->
    User = wf:user(),
    Nav = {User, myreviews, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    index:header() ++ dashboard:page(Nav,
        case lists:keyfind(feed, 1, Feeds) of false -> [];
        {_,Id}->
            FeedState = case wf:cache({Id,?CTX#context.module}) of undefined ->
                Fs = ?MYREVIEWS_FEED(Id), wf:cache({Id,?CTX#context.module}, Fs),Fs; FS -> FS end,
            InputState = case wf:cache({?FD_INPUT(Id),?CTX#context.module}) of undefined -> 
                Is = ?MYREVIEWS_INPUT(Id), wf:cache({?FD_INPUT(Id),?CTX#context.module}, Is), Is; IS-> IS end,
            #feed_ui{title= title(),
                icon="icon-list",
                state=FeedState,
                header=[#input{state=InputState} ]} end ) ++ index:footer().

%% Render review elements

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State})->
    wf:info("ok"),
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    {FromId, From} = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,
    error_logger:info_msg("media:~p", [E#entry.media]),
    wf:render([#panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue], body= <<"">>},
        #p{class=[username], body= #link{body=From, url= "/profile?id="++wf:to_list(FromId)}},
        #panel{body= product_ui:to_date(E#entry.created)},
        #p{body=[
            #link{url="#",body=[#span{class=[?EN_CM_COUNT(UiId)],
                body= integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]}]},

        #panel{id=?EN_MEDIA(UiId), class=[span4, "media-pic"],
            body=#entry_media{media=E#entry.media, mode=reviews}},

        #panel{class=[span5, "article-text"], body=[
            #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
                #link{style="color:#9b9c9e;", body=E#entry.title, url="/review?id="++wf:to_list(Eid)}}},

            #p{id=?EN_DESC(UiId), body=product_ui:shorten(E#entry.description)},
            #panel{id=?EN_TOOL(UiId), class=[more], body=[
                #link{body=[<<"read more">>], url=?URL_REVIEW(Eid)} ]}]}]);

render_element(E)-> feed_ui:render_element(E).


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed_ui:process_delivery(Route, Msg);
event(Event) -> error_logger:info_msg("[my reviews] event: ~p", [Event]).
