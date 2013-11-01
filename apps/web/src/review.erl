-module(review).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include("records.hrl").
-include("states.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"review">>},
                                      {body, body()},{css,?CSS},{less,?LESS},{bootstrap, ?BOOTSTRAP}]}.

feed_states() -> [].
input_states()-> [].

body() ->
    Id = case wf:qs(<<"id">>) of undefined -> undefined; V -> binary_to_list(V) end,
    Entries = lists:filter(fun(#entry{to=To})-> case To of {product, _}-> true; _-> false end end,
        kvs:all_by_index(entry, entry_id, Id)),

    index:header()++[
        #section{class=[section], body=#panel{class=[container], body=
            case Entries of [E=#entry{id=Eid, feed_id=Fid, to={product, Prid}}|_] ->
                Product = case kvs:get(product, Prid) of {error,_}-> #product{}; {ok, P}-> P end,
                FeedState = ?DETACHED_FEED(Fid),

                #panel{class=["row-fluid", dashboard], body=[
                    #panel{class=[span2], body=[
                        dashboard:section(case kvs:get(user, E#entry.from) of {error,_}->[];
                            {ok, U} -> [#panel{class=["dashboard-img-wrapper"], body=[
                                #panel{class=["dashboard-img"], body=[
                                    #image{image = case U#user.avatar of undefined ->  "/holder.js/180x180";
                                        Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
                                        ++"?sz=180&width=180&height=180&s=180" end, width= <<"180px">>, height= <<"180px">>}]}]},

                                    #panel{body=[#label{body= <<"Name:">>}, #b{id=displayname, body= U#user.display_name}]},
                                    #panel{body=[#label{body= <<"Mail:">>},
                                        #link{url= if U#user.email==undefined -> [];
                                            true-> iolist_to_binary(["mailto:", U#user.email]) end,
                                            body=#small{body=U#user.email}}]},
                                    #panel{body=[#label{body= <<"Member since ">>}, product_ui:to_date(U#user.register_date)]},
                                    #b{class=["text-success"], body=
                                        if U#user.status==ok -> <<"Active">>; true-> atom_to_list(U#user.status) end}
                                    ] end, "icon-user"),

                        dashboard:section([
                            #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Acticle">>},
                            #panel{body=[product_ui:to_date(E#entry.created)]},
                            #p{body=[#link{url="#",body=[
                                #span{class=[?EN_CM_COUNT(Eid)], body=
                                    integer_to_list(kvs_feed:comments_count(entry, Eid))},
                                    #i{class=["icon-comment-alt", "icon-2x"]} ]} ]} ], "icon-eye-open"),

                        dashboard:section([
                            #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Game">>},
                            #h4{body=[Product#product.title]},
                            #p{body=[Product#product.brief]},

                            #panel{class=["btn-toolbar", "text-center"], body=[
                                #button{class=[btn, "btn-warning"], body= [#span{class=["icon-shopping-cart"]}, <<" add to cart ">>], 
                                    postback={add_cart, Product}}
                            ]}
                        ], "icon-gamepad") ]},

                    #panel{class=[span10], body=[
                        dashboard:section(#feed_entry{entry=E, state=FeedState}, "icon-file-text-alt") ]}
            ]}; [] -> index:error(<<"not_found">>) end }},
        #section{class=[section], body=#panel{class=[container, "text-center"], body=[]}} ]++index:footer().

% Render view 

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=detached}=State})->
    Eid = element(State#feed_state.entry_id, E),
   {_, Fid} = lists:keyfind(comments, 1, E#entry.feeds),
    Recipients = [{RoutingType, To, {Eid, FeedId, lists:keyfind(comments, 1, Feeds)}}
        || #entry{to={RoutingType, To}, feed_id=FeedId, feeds=Feeds} <- kvs:all_by_index(entry,entry_id, Eid)],

    Is = ?COMMENTS_INPUT(Fid, Recipients),
    CmState = ?COMMENTS_FEED(Fid)#feed_state{recipients=Recipients},
    wf:cache({Fid,?CTX#context.module}, CmState),
    wf:cache({?FD_INPUT(Fid),?CTX#context.module}, Is),

    Entry = #panel{class=["blog-post"], body=[
        #h3{class=[blue], body=[#span{id=?EN_TITLE(Eid), body=E#entry.title, data_fields=[{<<"data-html">>, true}]} ]},
        #panel{id=?EN_DESC(Eid), body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

        #panel{class=[comments, "row-fluid"], body=[
            #feed_ui{icon="icon-comments-alt",
                title=[#span{class=[?EN_CM_COUNT(Eid)], body=[integer_to_list(kvs_feed:comments_count(entry, Eid))]}, <<" comments">>],
                state=CmState},
            #input{state=Is}
       ]}
    ]},
    element_panel:render_element(Entry);

% Comment

render_element(#div_entry{entry=#comment{}=C, state=#feed_state{}=State})->
    Id = element(State#feed_state.entry_id, C),
    {Author, Avatar} = case kvs:get(user, C#comment.from) of 
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,

    Date = product_ui:to_date(C#comment.created),
    {_, Fid} = lists:keyfind(comments, 1, C#comment.feeds),

    Recipients = lists:flatten([case kvs:get(comment, {Id, {E,F}}) of {error,_}-> [];
        {ok, #comment{feeds=Feeds}} ->
            Cs = lists:keyfind(comments, 1, Feeds),
            {R, T, {E, F, Cs}} end ||{R,T,{E,F,_}}<-State#feed_state.recipients]),

    CmState = ?FD_STATE(Fid, State)#feed_state{recipients=Recipients,
        show_title=State#feed_state.show_title andalso not State#feed_state.flat_mode,
        show_header=State#feed_state.show_header andalso not State#feed_state.flat_mode},
    wf:cache({Fid,?CTX#context.module}, CmState),

    Is = ?REPLY_INPUT(Fid, Recipients),
    wf:cache({?FD_INPUT(Fid),?CTX#context.module}, Is),

    InnerFeed = #feed_ui{state=CmState, class="comments",  header=[
        #input{state=Is, class=["comment-reply"]}]},

    wf:render([
        #panel{class=[media, "media-comment"], body=[
            #link{class=["pull-left"], body=[Avatar]},
            #panel{class=["media-body"], body=[
                #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
                #p{body=C#comment.content},
                if State#feed_state.flat_mode == true -> []; true -> #p{class=["media-heading"], body=[InnerFeed]} end ]} ]},
        if State#feed_state.flat_mode == true -> InnerFeed; true -> [] end]);

render_element(E)-> feed_ui:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, _, {Id,_}})-> wf:redirect("/review?id="++Id);
event({read, _, Id})-> wf:redirect("/review?id="++Id);
event({add_cart, P}) -> store:event({add_cart, P});
event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event-> ~p, Tag ~p, Term ~p",[Name, Tag, Term]).
process_delivery(R,M) -> feed_ui:process_delivery(R,M).
