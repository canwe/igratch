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

body() ->
    Id = case wf:qs(<<"id">>) of undefined -> undefined; V -> binary_to_list(V) end,
    index:header()++[
    #section{class=[section], body=#panel{class=[container], body=
        case kvs:get(entry, {Id, ?FEED(entry)}) of {error,E} -> index:error(E);
        {ok, #entry{id=Eid, to={product, Prid}}=E} ->
            Fid = ?FEED(entry),
            FeedState = case wf:cache({Fid,?CTX#context.module}) of undefined ->
                Fs = ?DETACHED_FEED(Fid), wf:cache({Fid,?CTX#context.module}, Fs), Fs; FS->FS end,

            #panel{class=["row-fluid", dashboard], body=[
                #panel{class=[span2], body=[
                    dashboard:section(case kvs:get(user, E#entry.from) of {error,_}->[];
                    {ok, #user{email=Email, status=Status}=U} -> [
                        #panel{class=["dashboard-img-wrapper"], body=[
                            #panel{class=["dashboard-img"], body=[
                                #image{image = case U#user.avatar of undefined -> "/holder.js/180x180";
                                    Av -> re:replace(Av, <<"_normal">>, <<"">>, [{return, list}])
                                    ++"?sz=180&width=180&height=180&s=180" end,
                                    width= <<"180px">>, height= <<"180px">>}]}]},

                        #panel{body=[#label{body= <<"Name:">>}, #b{id=displayname, body= U#user.display_name}]},
                        #panel{body=[
                            #label{body= <<"Mail:">>},
                            #link{url= if Email==undefined -> [];
                                true-> iolist_to_binary(["mailto:", Email]) end, body=#small{body=Email}}]},
                        #panel{body=[#label{body= <<"Member since ">>}, product_ui:to_date(U#user.register_date)]},
                        #b{class=["text-success"],
                            body=if Status==ok-> <<"Active">>;true -> atom_to_list(Status) end}] end,"icon-user"),

                    dashboard:section([
                        #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Acticle">>},
                        #panel{body=product_ui:to_date(E#entry.created)},
                        #p{body=#link{url="#",body=[
                            #span{class=[?EN_CM_COUNT(Eid)], body=integer_to_list(kvs_feed:comments_count(entry, Eid))},
                            #i{class=["icon-comment-alt", "icon-2x"]} ]}}], "icon-eye-open"),

                    case kvs:get(product, Prid) of {error,_} -> [];
                    {ok, P} -> dashboard:section([
                        #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Game">>},
                        #h4{body=P#product.title},
                        #p{body=P#product.brief},
                        #p{body=[#span{class=["icon-usd"]}, float_to_list(P#product.price/100, [{decimals, 2}])]},

                        #panel{class=["btn-toolbar", "text-center"], body=#button{class=[btn, "btn-warning"],
                            body=[#span{class=["icon-shopping-cart"]},<<" add to cart">>],postback={add_cart, P}}}],
                        "icon-gamepad") end ]},

                #panel{class=[span10], body=[
                     dashboard:section(#feed_entry{entry=E, state=FeedState}, "icon-file-text-alt") ]} ]} end }},
    #section{class=[section], body=#panel{class=[container, "text-center"]}} ]++index:footer().

% Render view 

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=detached}=State})->
    Eid = element(State#feed_state.entry_id, E),

    Entry = #panel{class=["blog-post"], body=[
        #h3{class=[blue], body=[#span{id=?EN_TITLE(Eid), body=E#entry.title, data_fields=[{<<"data-html">>, true}]}]},
        #panel{id=?EN_DESC(Eid), body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

        case lists:keyfind(comments, 1, E#entry.feeds) of false -> [];
        {_, Fid} ->
            Recipients = lists:flatmap(fun(#entry{id=EntryId, feeds=Feeds, to={Route,Type}})->
                case lists:keyfind(comments, 1, Feeds) of false -> [];
                {_, ECFid} -> [{Route,Type, {EntryId, ECFid}}] end end, kvs:all_by_index(entry, entry_id, Eid)),

            InputState = case wf:cache({?FD_INPUT(Fid), ?CTX#context.module}) of undefined ->
                Is = ?COMMENTS_INPUT(Fid, Recipients)#input_state{entry_id=Eid},
                wf:cache({?FD_INPUT(Fid), ?CTX#context.module},Is),Is; IS -> IS end,

            CmState = case wf:cache({Fid,?CTX#context.module}) of undefined ->
                S = ?COMMENTS_FEED(Fid)#feed_state{recipients=Recipients},
                wf:cache({Fid,?CTX#context.module},S), S; FS -> FS end,

            #panel{class=[comments, "row-fluid"], body=[
                #feed_ui{icon="icon-comments-alt",
                    state=CmState,
                    title=[
                        #span{class=[?EN_CM_COUNT(Eid)],
                            body=[integer_to_list(kvs_feed:comments_count(entry, Eid)), <<" comments">>]}
                    ]},
                #input{state=InputState} ]} end]},

    element_panel:render_element(Entry);

% Comment

render_element(#div_entry{entry=#comment{entry_id={Eid,_}}=C, state=#feed_state{}=State})->
    Id = element(State#feed_state.entry_id, C),
    {Author, Avatar} = case kvs:get(user, C#comment.from) of
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], 
                    image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]),
                    width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,

    Date = product_ui:to_date(C#comment.created),

    InnerFeed = case lists:keyfind(comments,1,C#comment.feeds) of false -> [];
    {_, Fid} ->
        Recipients = lists:flatmap(fun({Route,Type,{Ei,ECfid}}) ->
            case kvs:get(comment, {Id, Ei, ECfid}) of {error,_} -> [];
            {ok, #comment{feeds=Feeds}} ->
                case lists:keyfind(comments,1,Feeds) of false -> [];
                {_,CCFid} -> [{Route, Type, {Ei, CCFid}}] end end end, State#feed_state.recipients),

        InputState = ?REPLY_INPUT(Fid, Recipients)#input_state{entry_id=Eid},
        wf:cache({?FD_INPUT(Fid),?CTX#context.module}, InputState),

        CmState = ?FD_STATE(Fid, State)#feed_state{
            js_escape = false,
            recipients = Recipients,
            show_title = false,
            show_header= State#feed_state.show_header andalso not State#feed_state.flat_mode},
        wf:cache({Fid, ?CTX#context.module}, CmState),

        #feed_ui{
            class="comments",
            state=CmState,
            header=[#input{state=InputState, class=["comment-reply"]}]} end,

    wf:render([#panel{class=[media, "media-comment"], body=[
        #link{class=["pull-left"], body=[Avatar]},
        #panel{class=["media-body"], body=[
            #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
            #p{body=C#comment.content},
            if State#feed_state.flat_mode == true -> [];
                true -> #p{class=["media-heading"], body=[InnerFeed]} end ]} ]},

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

process_delivery(R,M) ->
    User = wf:user(),
    case lists:keyfind(cart, 1, User#user.feeds) of false -> ok;
    {_, Id} -> case kvs:get(feed,Id) of {error,_}-> ok;
        {ok, #feed{entries_count=C}} when C==0 -> ok;
        {ok, #feed{entries_count=C}}-> wf:update(?USR_CART(User#user.id), integer_to_list(C)) end end,
    feed_ui:process_delivery(R,M).
