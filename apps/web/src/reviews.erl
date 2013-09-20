-module(reviews).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

-jsmacro([on_show/0,show/1]).

on_show() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) -> jq(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
    Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],
    wf:wire(#api{name=tabshow}),
    wf:wire(on_show()),
    wf:wire(show(case wf:qs(<<"id">>) of undefined -> "'all'"; T ->  "'"++wf:to_list(T)++"'" end)),

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #link{url="#all", data_fields=?DATA_TAB},
            #panel{class=["row-fluid"], body=[
                #panel{class=[span9, "tab-content"], body=[
                    #panel{id=all, class=["tab-pane"]},
                    [#panel{id=Id, class=["tab-pane"]} || #group{id=Id} <- Groups] ]},
                #panel{class=[span3], body=[<<"">>]} ]} ]}]} ] ++ index:footer().

header(Groups, Current) -> [
    lists:dropwhile(fun(E)-> E== <<" / ">> end, [begin [ <<" / ">>,
        #link{url="#"++Id, body=[#span{class=["icon-asterisk"]}, Name],
            data_fields=?DATA_TAB,
            class=[if Current==Id-> "text-warning"; true -> "" end]}]
    end || #group{id=Id, name=Name} <- Groups])].

feed(Group) ->
    Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],
    State = case kvs:get(group, Group) of
        {error,_} -> ?FD_STATE(?FEED(entry))#feed_state{
                        view=review,
                        entry_id=#entry.entry_id,
                        delegate=reviews};
        {ok, G} -> case lists:keyfind(feed, 1, element(#iterator.feeds, G)) of
            false -> ?FD_STATE(?FEED(entry))#feed_state{
                        view=review,
                        entry_id=#entry.entry_id,
                        delegate=reviews};
            {_, Id} -> ?FD_STATE(Id)#feed_state{
                        view=review,
                        entry_id=#entry.entry_id,
                        delegate=reviews} end end,

    #feed_ui{icon=["icon-tags ", "icon-large ", if Group =="all"-> "text-warning"; true-> "" end],
        icon_url="#all",
        title=[header(Groups, Group)], state=State}.

%% Render store elements

render_element(E)-> feed_ui:render_element(E).

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(Id)),
    wf:wire(on_show()),
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, _, Id})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[reviews]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
