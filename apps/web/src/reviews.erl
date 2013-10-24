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
-include("states.hrl").

-jsmacro([on_show/0,show/1]).

on_show() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) -> jq(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

feed_states()-> [
    {?FEED(entry), ?ENTRIES_FEED} |
    [case lists:keyfind(feed, 1, Feeds) of false -> {ok, ok};
    {_,Id} -> {Id, ?REVIEWS_FEED(Id)} end || #group{scope=Scope, feeds=Feeds} <- kvs:all(group), Scope==public] ].

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(on_show()),
    wf:wire(show(case wf:qs(<<"id">>) of undefined -> "'all'"; T ->  "'"++wf:to_list(T)++"'" end)),

    Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],

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
    State = case kvs:get(group, Group) of {error,_} -> ?ENTRIES_FEED;
        {ok, G} -> case lists:keyfind(feed, 1, element(#iterator.feeds, G)) of false -> false;
            {_, Id} -> ?REVIEWS_FEED(Id) end end,

    #feed_ui{icon=["icon-tags ", "icon-large ", if Group =="all"-> "text-warning"; true-> "" end],
        icon_url="#all",
        title=[header(Groups, Group)], state=State}.

%% Render review elements

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),

    {FromId, From} = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,
    Media = E#entry.media,
    Title = E#entry.title,
    Date = E#entry.created,

    wf:render([
    #panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue], body= <<"">>},
        #p{class=[username], body= #link{body=From, url= "/profile?id="++wf:to_list(FromId)}},
        #p{class=[datestamp], body=[ #span{body= product_ui:to_date(Date)} ]},
        #p{body=[
            #link{url="#",body=[#span{class=[?EN_CM_COUNT(UiId)],
                body= integer_to_list(kvs_feed:comments_count(entry, Id))},
                #i{class=["icon-comment-alt", "icon-2x"]} ]} ]}]},

    #panel{id=?EN_MEDIA(UiId), class=[span4, "media-pic"],
        body=#entry_media{media=Media, mode=reviews}},

    #panel{class=[span5, "article-text"], body=[
        #h3{body=#span{id=?EN_TITLE(UiId), class=[title], body=
            #link{style="color:#9b9c9e;", body=wf:js_escape(Title), url="/review?id="++wf:to_list(Eid)}}},

        #p{id=?EN_DESC(UiId), body=product_ui:shorten(wf:js_escape(E#entry.description))},
        #panel{id=?EN_TOOL(UiId), class=[more], body=[
            #link{body=[<<"read more">>], url="/review?id="++wf:to_list(Eid)} ]}]}]);

render_element(E)-> feed_ui:render_element(E).

% Events 

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
