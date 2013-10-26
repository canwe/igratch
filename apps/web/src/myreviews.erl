-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("feed_server/include/records.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").

-include("records.hrl").
-include("states.hrl").

feed_states()->
    User = wf:user(),
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,
    case lists:keyfind(feed, 1, Feeds) of false -> []; {_, Id} -> [{Id, ?MYREVIEWS_FEED(Id)}] end.

input_states()-> 
    User = wf:user(),
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,
    case lists:keyfind(feed, 1, Feeds) of false -> []; {_, Id} -> [{Id, ?MYREVIEWS_INPUT(Id)}] end.

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()},{css,?CSS},{less,?LESS},{bootstrap, ?BOOTSTRAP}]}.

body()->
    User = wf:user(),
    Nav = {User, myreviews, []},
    Feeds = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    index:header() ++ dashboard:page(Nav,
        case lists:keyfind(feed, 1, Feeds) of false -> [];
        {_,Id}->
            FeedState = proplists:get_value(Id, feed_states()),
            InputState = proplists:get_value(Id, input_states()),

            #feed_ui{title= title(),
                icon="icon-list",
                state=FeedState,
                header=[#input{state=InputState} ]} end ) ++ index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed_ui:process_delivery(Route, Msg);
event(Event) -> error_logger:info_msg("[my reviews] event: ~p", [Event]).
