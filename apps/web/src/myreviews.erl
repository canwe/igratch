-module(myreviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

title() -> <<"My reviews">>.

main()-> #dtl{file="prod", bindings=[{title, title()},{body, body()}]}.

body()-> Nav = {wf:user(), myreviews, []},
    index:header() ++ dashboard:page(Nav, [
        #input{title= <<"Submit review">>, placeholder_rcp= <<"Games">>, role=product, type=review},
        #feed_view{owner=wf:user(), feed=feed, title=title()} ]) ++ index:footer().

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> feed:process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id="++Id);
event({read, review, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[account] event: ~p", [Event]).
