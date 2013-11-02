-module(product).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-include("states.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"product">>}, {body, body()},
                                      {css,?PRODUCT_CSS}, {less,?LESS}, {bootstrap, ?PRODUCT_BOOTSTRAP}]}.

-define(FILE_DIR, "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end ++"/files").

body() ->
    Id = case wf:qs(<<"id">>) of undefined -> <<"no">>; I-> I end,
    index:header()++[
    #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, binary_to_list(Id)) of
    {ok, P} ->
        wf:wire(#api{name=tabshow}),
        wf:wire(index:on_shown()),

        wf:session(product, P),
        Owner = case wf:user() of undefined -> false; U -> P#product.owner == U#user.email end,
        Groups = lists:flatmap(fun(#group_subscription{where=G})->
            case kvs:get(group,G) of
                {error,_}-> [];
                {ok, #group{scope=Scope}} when Scope == private -> [];
                {ok, #group{id=Gid, name=Name, feeds=Feeds}} -> [{Gid, Name, lists:keyfind(products, 1, Feeds)}] end end,
            kvs_group:participate(P#product.id)),
        UsrFeeds = [{Feed,Fid}||{Feed, Fid} <- P#product.feeds, Feed /= comments andalso Feed /= bundles],

        [#panel{class=["row-fluid", "page-header-sm"], style="padding:0", body=[
            #h4{class=[span9], style="line-height:30px;margin-left:5px;", body= [
                #link{url= <<"/reviews">>, body= <<"Categories ">>, style="color:#999"},
                #small{body=[
                    [<<" | ">>, #link{url="/store?id="++Gid, body=[#span{class=["icon-asterisk"]},Name]}]
                 || {Gid, Name, _} <- Groups]} ]},

            #panel{class=[span3, "input-append", "pull-right"], style="margin:10px 0", body=[
                #textbox{class=[disabled], placeholder= <<"Search">>, disabled=true},
                #button{class=[btn, disabled], body= <<"Go!">>} ]} ]},

        #section{class=[section, alt], body=#panel{class=[container], body=[
            #product_hero{product=P},
            #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
                [#li{class= if Feed == reviews -> ["active"]; true -> [] end,
                    body=#link{data_fields=?DATA_TAB,
                        url= "#"++wf:to_list(Feed), body=wf:to_list(Feed)}} || {Feed, _} <- UsrFeeds],

                if Owner -> [
                    #li{body=#link{data_fields=?DATA_TAB, url="#files",   body= <<"files">> }},
                    #li{body=#link{data_fields=?DATA_TAB, url="#finance", body= <<"finance">> }}]; true -> [] end
            ]} ]}},

        #section{class=[section], body=
            #panel{class=[container], body=
                #panel{class=["row-fluid"], body=[
                    #panel{class=[span9], body=
                        #panel{class=["tab-content"], body=[
                            [if Feed == reviews ->
                                #panel{id=reviews, class=["tab-pane", active], body=feed(P,Fid,false)};
                             true ->
                                #panel{id=Feed, class=["tab-pane"], body=feed(P,Fid)} end || {Feed,_}=Fid <- UsrFeeds],
                            if Owner -> [
                                #panel{id=files,   class=["tab-pane"], body=files(P)},
                                #panel{id=finance, class=["tab-pane"], body=payments(P)}]; true -> [] end ]}},
                    #panel{class=[span3], body=aside(P, Groups)} ]}}}
        ];
    {error, E} -> #panel{class=[alert, "alert-danger","alert-block"], body=[
        #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>},
        #strong{body= atom_to_list(E)} ]} end }}]++index:footer().

feed(#product{} = P, {Tab, Id}) -> feed(#product{} = P, {Tab, Id}, true).
feed(#product{} = P, {Tab, Id},Escape) ->
    User = case wf:user() of undefined -> #user{}; U -> U end,

    State = ?BLOG_STATE(Id)#feed_state{enable_selection=User#user.email == P#product.owner},
    Is = ?BLOG_INPUT(Id)#input_state{
        entry_type=case Tab of reviews -> review; _-> Tab end,
        recipients=[{product, P#product.id, {Tab,Id}}],
        expand_btn= "Write "++atom_to_list(Tab)},

    wf:cache({Id,?CTX#context.module}, State),
    wf:cache({?FD_INPUT(Id),?CTX#context.module}, Is),

    #feed_ui{title=wf:to_list(Tab), icon="icon-circle", state=State#feed_state{js_escape=Escape}, header=[
        if User#user.email == P#product.owner orelse Tab == reviews -> #input{state = Is}; true -> [] end]}.

payments(#product{} = P)-> [
    #table{class=[table, "table-hover", payments],
      header=[#tr{cells=[#th{body= <<"Date">>}, #th{body= <<"Status">>},#th{body= <<"Price">>},#th{body= <<"User">>}]}],
      body=[[begin
        #tr{cells=[
          #td{body=[product_ui:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],
                body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end,
                float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{body=User, url= ?URL_PROFILE(User)}}]}
      end || #payment{user_id=User, product=#product{price=Price, currency=Cur}} = Py
        <- kvs:all_by_index(payment, product_id, P#product.id) ]]}].

files(#product{} = P)->
    case lists:keyfind(bundles, 1, P#product.feeds) of false-> [];
    {_,Fid}->
        Fs = case wf:cache({Fid,?CTX#context.module}) of undefined ->
            State = ?FILE_STATE(Fid), wf:cache({Fid,?CTX#context.module}, State), State; S -> S end,

        Is = case wf:cache({?FD_INPUT(Fid),?CTX#context.module}) of undefined ->
            In = ?FILE_INPUT(Fid)#input_state{upload_dir= ?FILE_DIR,recipients=[{product,P#product.id,{bundles, Fid}}]},
            wf:cache({?FD_INPUT(Fid),?CTX#context.module}, In), In; S1 -> S1 end,

        #feed_ui{title="product files", state=Fs, header=[#input{state=Is}]} end.

aside(#product{} = P, Groups)->
    ActiveFeed = case lists:keyfind(comments, 1, P#product.feeds) of false -> [];
    {_,Fid} ->
        AS = case wf:cache({Fid,?CTX#context.module}) of undefined ->
            Fs = ?FD_STATE(Fid)#feed_state{
                flat_mode=true,
                view=comment,
                entry_type=comment,
                entry_id=#comment.comment_id},
            wf:cache({Fid,?CTX#context.module}, Fs), Fs;
        AF -> AF end,
        #feed_ui{title= <<"Product discussion">>, 
                icon="icon-comments-alt", 
                class="comments-flat",
                state=AS} end,

    GroupFeeds = [begin
        {_,Gfid} = Fd,
        GS = case wf:cache({Gfid,?CTX#context.module}) of undefined ->
            Gfs = ?FD_STATE(Gfid)#feed_state{view=group, flat_mode = true, entry_id = #entry.entry_id, delegate=product},
            wf:cache({Gfid,?CTX#context.module}, Gfs), Gfs; GF -> GF end,

            #feed_ui{title= "More " ++ wf:to_list(Name),
                icon = "icon-list",
                class= "entries-flat",
                state=GS}
    end || {_,Name,Fd} <- Groups, Fd /= false],

    #aside{class=[sidebar], body=[
       [#panel{class=["sidebar-widget"], body=F} || F <- GroupFeeds],
      #panel{class=["sidebar-widget"], body=ActiveFeed} ]}.

controls(#entry{type=Type} =  E) -> [
  #link{body=[case Type of product -> <<"view ">>; _-> <<"read more ">> end, 
    #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, Type, E#entry.id}} ].


% Render elements of the page feeds

render_element(#div_entry{entry=E, state=#feed_state{view=files}=S})->
    Uid = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    Panel =#panel{class=["file-entry"],body=[
        #p{id=?EN_DESC(Uid), body=E#entry.description},
        [#entry_media{media=M, mode=input} ||M <- E#entry.media]
    ]},
    element_panel:render_element(Panel);

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=group, flat_mode=true}=State}) ->
    case kvs:get(product, E#entry.entry_id) of {error, _} -> wf:render(#panel{body= <<"error displaying item">>});
    {ok, P} ->
        Id = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, P))),
        Media = store:media(P#product.cover),

        wf:render([
            #panel{id=?EN_MEDIA(Id), class=[span5, "media-pic"], body=#entry_media{media=Media, mode=store}},

            #panel{class=[span6, "article-text"], style="height:5em",  body=[
                #h4{body=#span{id=?EN_TITLE(Id), class=[title], body=
                #link{style="color:#9b9c9e;", body=P#product.title, url=?URL_PRODUCT(P#product.id)}}},

            #p{id=?EN_DESC(Id), body=product_ui:shorten(P#product.brief)} ]},

            #panel{class=[span12], body=[
                #link{body=#i{class=["icon-windows"]}},
                #link{url="#",body=[
                    #span{class=[?EN_CM_COUNT(Id)], body=
                        integer_to_list(kvs_feed:comments_count(product, P#product.id))},
                    #i{class=["icon-comment-alt"]} ]} ]} ]) end;


% Render blog view

render_element(#div_entry{entry=#entry{}=E, state=#feed_state{view=blog}=State})->
    {Eid,_} = Id = element(State#feed_state.entry_id, E),
    UiId = wf:to_list(erlang:phash2(element(State#feed_state.entry_id, E))),
    From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error,_} -> E#entry.from end,

    Entry = #panel{class=["blog-post"], body=[
        #header{class=["blog-header"], body=[
            #h3{body=[#span{id=?EN_TITLE(UiId), body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, 
            #small{body=[<<" by ">>, #link{body=From}, product_ui:to_date(E#entry.created)]}]}]},

        #figure{class=["thumbnail-figure"], body=[
            #carousel{items=[#entry_media{media=Media, mode=blog} || Media <- E#entry.media]},
            if length(E#entry.media) > 1 ->
                #figcaption{class=["thumbnail-title"], body=[#h4{body=#span{body=E#entry.title}}]}; true -> [] end ]},

        #panel{id=?EN_DESC(UiId), body=product_ui:shorten(E#entry.description), data_fields=[{<<"data-html">>, true}]},

        #footer{class=["blog-footer", "row-fluid"], body=[
            #link{body=[ #i{class=["icon-comments-alt", "icon-large"]},
                #span{class=[?EN_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))}],
                url=?URL_REVIEW(Id)},
            #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}],
                url=?URL_REVIEW(Eid)} ]} ]},
    element_panel:render_element(Entry);
render_element(E)-> feed_ui:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);

event({edit_entry, E=#entry{title=Title, description=Desc}, ProdId, MsId}) ->
  Tid = ?EN_TITLE(E#entry.entry_id), Did = ?EN_DESC(E#entry.entry_id), Toid = ?EN_TOOL(E#entry.entry_id),
  Dir = "static/"++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:replace(Tid, #textbox{id=Tid, value=Title}),
  wf:replace(Did, #panel{body=[#htmlbox{id=Did, html=Desc, root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE}]}),
  wf:update(Toid, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, ProdId}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E#entry{title=Title, description=Desc}}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
  ]});
event({save_entry, #entry{}=E, ProductId})->
  Title = wf:q(?EN_TITLE(E#entry.entry_id)),
  Description = wf:q(?EN_DESC(E#entry.entry_id)),
  User = wf:user(),

  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where, type=member} <- kvs_group:participate(ProductId), E#entry.type==reviews],

  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Id, lists:keyfind(feed, 1, Feeds)} || #group{id=Id, feeds=Feeds} <-Groups]] 
    ++ if E#entry.type==reviews -> [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}];true-> [] end,

  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [ msg:notify([kvs_feed, RouteType, To, entry, Fid, edit], E#entry{title=Title, description=Description}) || {RouteType, To, Fid} <- Recipients];

event({cancel_entry, E=#entry{title=Title, description=Desc}}) ->
  Tid = ?EN_TITLE(E#entry.entry_id), Did = ?EN_DESC(E#entry.entry_id),
  wf:replace(Tid, #span{id=Tid, body=Title}),
  wf:replace(Did, #panel{id=Did, body=Desc, data_fields=[{<<"data-html">>, true}]}),
  wf:update(?EN_TOOL(E#entry.entry_id), []);

event({remove_entry, E=#entry{}, ProductId}) ->
  User = wf:user(),
  Groups = [case kvs:get(group,Where) of {error,_}->[]; {ok,G} ->G end ||
    #group_subscription{where=Where} <- kvs_group:participate(ProductId), E#entry.type == reviews],
  Recipients = [{product, ProductId, {E#entry.type, E#entry.feed_id}} | [{group, Gid, lists:keyfind(feed, 1, Feeds)} || #group{id=Gid, feeds=Feeds} <-Groups]]
  ++ [{user, User#user.email, lists:keyfind(feed,1, User#user.feeds)}],

  error_logger:info_msg("Recipients: ~p", [Recipients]),

  [msg:notify([kvs_feed, To, entry, delete], [E#entry{id={E#entry.entry_id, Fid}, feed_id=Fid}]) || {_, To, Fid} <- Recipients];

event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event({add_cart, P}) ->
    store:event({add_cart, P}),
    wf:redirect("/shopping_cart");
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    P = wf:session(product),
    case lists:keyfind(list_to_atom(Id), 1, P#product.feeds) of false -> ok;
        {reviews,_} -> ok;
        Tab -> wf:update(list_to_atom(Id), feed(P, Tab)) end,
    wf:wire("Holder.run();");

api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
    FileName = filename:join([Root,Dir,binary_to_list(File)]),

    {exist, case file:read_file_info(FileName) of {error, _} -> 0;
    {ok, FileInfo} ->
        Url = filename:join([Dir,binary_to_list(File)]),
        Media = #media{id=element_upload:hash(FileName), url = Url, type ={attachment,MimeType}},
        wf:cache(medias, [Media]),
        error_logger:info_msg("[product]query_file ~p", [Media]),
        wf:update(Target, input:media_preview(Target, [Media])),
        wf:wire("Holder.run();"),
        FileInfo#file_info.size end}.

process_delivery([_,_,entry,_,edit], #entry{entry_id=Id, title=Title, description=Desc, media=Media}=E) ->
  wf:cache(medias, []),
  Tid = ?EN_TITLE(Id), Did = ?EN_DESC(Id),
  wf:replace(Tid, #span{id =Tid, body=Title}),
  wf:replace(Did, #panel{id=Did, body=Desc, data_fields=[{<<"data-html">>, true}]}),
  wf:update(?EN_MEDIA(Id), #entry_media{media=Media, mode=reviews}),
  wf:update(?EN_TOOL(Id), feed:controls(E)),
  wf:wire("Holder.run();");

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
