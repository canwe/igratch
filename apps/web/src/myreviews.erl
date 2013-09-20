-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("feed_server/include/records.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()}]}.

body()->
    User = wf:user(),
    Nav = {User, myreviews, []},
    List = case User of undefined -> []; _-> element(#iterator.feeds, User) end,

    State = case lists:keyfind(feed, 1, List) of 
        false -> #feed_state{};
        {_, Id} -> ?FD_STATE(Id)#feed_state{
                        entry_id=#entry.entry_id,
                        view=review,
                        enable_selection=true,
                        delegate=reviews} end,

    index:header() ++
    dashboard:page(Nav, [
        #feed_ui{title=title(), icon="icon-list", state=State, header=[
            #input{ title= <<"Submit review">>, placeholder_rcp= <<"Games">>, 
                    role=product, state=#input_state{entry_type=review},
                    feed_state=State, class=["feed-table-header"]} ]} ]) ++
    index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed_ui:process_delivery(Route, Msg);
event(Event) -> error_logger:info_msg("[account] event: ~p", [Event]).
