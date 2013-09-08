-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("feed_server/include/records.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()}]}.

body()-> Nav = {wf:user(), myreviews, []},
    User = wf:user(),
    {_, Id} = lists:keyfind(feed, 1, element(#iterator.feeds, User)),
    index:header() ++ dashboard:page(Nav, [
        #feed2{title=title(), icon="icon-list", entry_type=entry, container=feed, container_id=Id, selection=true, entry_view=review, table_mode=false, header=[
            #input{title= <<"Submit review">>, placeholder_rcp= <<"Games">>, role=product, type=review}
        ]}
    ]) ++ index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed:process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id="++Id);
event({read, review, {Id,_}})-> wf:redirect("/review?id="++Id);
event({read, review, Id})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[account] event: ~p", [Event]).
