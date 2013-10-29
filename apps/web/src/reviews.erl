-module(reviews).
-compile(export_all).
-compile({parse_transform, shen}).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").
-include("states.hrl").

-jsmacro([on_show/0,show/1]).

on_show() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("show", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) -> jq(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>}, {body, body()},
                                    {css, ?REVIEWS_CSS},{less, ?REVIEWS_LESS},{bootstrap, ?REVIEWS_BOOTSTRAP}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire(on_show()),

    Groups = lists:flatmap(fun(#group{scope=Scope, feeds=Feeds, name=Name})->
        case lists:keyfind(feed,1, Feeds) of
        {_,Fid} when Scope==public ->
            case wf:session(Fid) of undefined -> wf:session(Fid, ?REVIEWS_FEED(Fid)), [{Name, Fid}];
                _-> [{Name,Fid}] end; _ -> [] end end, kvs:all(group)),

    All = case wf:session(?FEED(entry)) of undefined ->
        FS = ?ENTRIES_FEED, wf:session(?FEED(entry),FS), FS; F->F end,

    index:header() ++ [
    #section{class=[section], body=[
        #panel{class=[container], body=[
            #panel{class=["row-fluid"], body=[
                #panel{class=[span12, "page-header-sm"], body=[
                    #link{url="#all", body=[#i{class=["icon-home"]}], data_fields=?DATA_TAB},
                    [[<<" / ">>, #link{url="#"++wf:to_list(Fid), body=[
                        #i{class=["icon-asterisk"]}, Name], data_fields=?DATA_TAB}] || {Name,Fid} <- Groups] ]} ]},

            #panel{class=["row-fluid"], body=[
                #panel{class=[span9, "tab-content"], body=[
                    #panel{id=all, class=["tab-pane", active], body=[
                        #feed_ui{icon=["icon-tags ", "icon-large ", "text-warning"], state=All} ]},

                    [#panel{id=wf:to_list(Fid), class=["tab-pane"]}|| {_,Fid} <- Groups] ]},
                #panel{class=[span3], body=[<<"">>]} ]} ]}]} ] ++ index:footer().

feed(Fid) -> #feed_ui{icon=["icon-tags ", "icon-large "], state=wf:session(Fid)}.

%% Render review elements

render_element(#div_entry{entry=#entry{entry_id=Eid}=E, state=#feed_state{view=review}=State})->
    Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    {FromId, From} = case kvs:get(user, E#entry.from) of {ok, User} -> {E#entry.from, User#user.display_name}; {error, _} -> {E#entry.from,E#entry.from} end,

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

            #p{id=?EN_DESC(UiId), body=product_ui:shorten(wf:js_escape(E#entry.description))},
            #panel{id=?EN_TOOL(UiId), class=[more], body=[
                #link{body=[<<"read more">>], url="/review?id="++wf:to_list(Eid)} ]}]}]);

render_element(E)-> feed_ui:render_element(E).

% Events 

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    case Id of "all" -> []; _ -> wf:update(Id, feed(list_to_integer(Id))) end,
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, _, Id})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[reviews]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
