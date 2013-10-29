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
            FeedState = case wf:session({Id,?CTX#context.module}) of undefined ->
                Fs = ?MYREVIEWS_FEED(Id), wf:session({Id,?CTX#context.module}, Fs),Fs; FS -> FS end,
            InputState = case wf:session({?FD_INPUT(Id),?CTX#context.module}) of undefined -> 
                Is = ?MYREVIEWS_INPUT(Id), wf:session({?FD_INPUT(Id),?CTX#context.module}, Is), Is; IS-> IS end,
            #feed_ui{title= title(),
                icon="icon-list",
                state=FeedState,
                header=[#input{state=InputState} ]} end ) ++ index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed_ui:process_delivery(Route, Msg);
event(Event) -> error_logger:info_msg("[my reviews] event: ~p", [Event]).
