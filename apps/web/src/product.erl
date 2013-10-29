-module(product).
-compile({parse_transform, shen}).
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

-jsmacro([on_shown/0,show/1]).

on_shown() ->
    X = jq("a[data-toggle=\"tab\"]"),
    X:on("shown", fun(E) -> T = jq(E:at("target")), tabshow(T:attr("href")) end).

show(E) ->
    D = jq(document),
    D:ready(fun() -> T = jq("a[href=\"#" ++ E ++ "\"]"), T:tab("show") end).

main() -> #dtl{file="prod", bindings=[{title,<<"product">>},
                                      {body, body()},{css,?CSS},{less,?LESS},{bootstrap, ?BOOTSTRAP}]}.

-define(FILE_DIR, "static/"++ case wf:user() of undefined-> "anonymous"; User -> User#user.email end ++"/files").

body() ->
    Id = case wf:qs(<<"id">>) of undefined -> <<"no">>; I-> I end,
    wf:wire(#api{name=tabshow}),
    wf:wire(on_shown()),
    User = wf:user(),

    index:header()++[
    #section{class=[section], body=#panel{class=[container], body=
    case kvs:get(product, binary_to_list(Id)) of
    {ok, P} ->
        wf:session(product, P),
        Owner = case User of undefined -> false; _ -> P#product.owner == User#user.email end,

        [#panel{class=["row-fluid", "page-header"], body=[
            #h4{class=[span9], style="line-height:30px;", body= [
                #link{url= <<"/reviews">>, body= <<"Categories ">>, style="color:#999"}, 
                #small{body=[[begin
                    Name = case kvs:get(group,I) of {ok, G}-> G#group.name; _ -> "noname" end,
                    [<<" | ">>, #link{url="/store?id="++I, body=[#span{class=["icon-asterisk"]},Name]}]
                end] || #group_subscription{where=I} <- kvs_group:participate(P#product.id)]} ]},

            #panel{class=[span3, "input-append"], style="margin:10px 0", body=[
                #textbox{id="search-button", placeholder= <<"Search">>},
                #button{class=[btn], body= <<"Go!">>} ]} ]},

        #section{class=[section, alt], body=#panel{class=[container], body=[
            #product_hero{product=P},
            #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
                [#li{class= if Feed == reviews -> ["active"]; true -> [] end,
                    body=#link{data_fields=?DATA_TAB, url= "#"++wf:to_list(Feed), body=wf:to_list(Feed)}}
                    || {Feed, _Fid} <- P#product.feeds, Feed /= comments andalso Feed /= bundles],
                if Owner -> [
                    #li{body=#link{data_fields=?DATA_TAB, url="#files", body= <<"files">> }},
                    #li{body=#link{data_fields=?DATA_TAB, url="#finance", body= <<"finance">> }}]; true -> [] end
            ]} ]}},

        #section{class=[section], body=
            #panel{class=[container], body=
                #panel{class=["row-fluid"], body=[
                    #panel{class=[span9], body=
                        #panel{class=["tab-content"], body=[
                            [begin
                                Active = if Feed == reviews -> "active"; true -> "" end,
                                #panel{id=Feed, class=["tab-pane", Active], body=[
                                    if Feed == reviews -> feed(P, Fid); true -> [] end]}
                            end || {Feed, _}=Fid <- P#product.feeds, Feed /= bundles],
                            if Owner -> [
                                #panel{id=files, class=["tab-pane"], body=files(P)},
                                #panel{id=finance, class=["tab-pane"], body=payments(P)}]; true -> [] end
                        ]}},
                    #panel{class=[span3], body=aside()} ]}}} 
        ];
    {error, E} -> #panel{class=[alert, "alert-danger","alert-block"], body=[
        #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>},
        #strong{body= atom_to_list(E)} ]} end }}]++index:footer().

feed(#product{} = P, {Tab, Id})->
    User = case wf:user() of undefined -> #user{}; U -> U end,

    State = ?BLOG_STATE(Id)#feed_state{enable_selection=User#user.email == P#product.owner},
    Is = ?BLOG_INPUT(Id)#input_state{
        entry_type=case Tab of reviews -> review; _-> Tab end,
        recipients=[{product, P#product.id, {Tab,Id}}],
        expand_btn= "Write "++atom_to_list(Tab)},

    wf:session(Id, State),
    wf:session(?FD_INPUT(Id), Is),

    #feed_ui{title=wf:to_list(Tab), icon="icon-circle", state=State, header=[
        if User#user.email == P#product.owner orelse Tab == reviews ->
            #input{icon="", state = Is};
        true -> [] end]}.

payments(P) -> [
    #table{class=[table, "table-hover", payments],
      header=[#tr{cells=[#th{body= <<"Date">>}, #th{body= <<"Status">>}, #th{body= <<"Price">>}, #th{body= <<"User">>}]}],
      body=[[begin
        #tr{cells= [
          #td{body= [product_ui:to_date(Py#payment.start_time)]},
          #td{class=[case Py#payment.state of done -> "text-success"; added-> "text-warning"; _-> "text-error" end],body= [atom_to_list(Py#payment.state)]},
          #td{body=[case Cur of "USD"-> #span{class=["icon-usd"]}; _ -> #span{class=["icon-money"]} end, float_to_list(Price/100, [{decimals, 2}])]},
          #td{body=#link{body=User, url= "/profile?id="++wf:to_list(User)}}]}
      end || #payment{user_id=User, product=#product{price=Price, currency=Cur}} = Py <-kvs:all_by_index(payment, product_id, P#product.id) ]]}
    ].

files(P)->
    case lists:keyfind(bundles, 1, P#product.feeds) of false->[];
    {_,Fid}->
        State = ?FD_STATE(Fid)#feed_state{view=files,
            enable_selection=true,
            delegate=product},

        Is = #input_state{id=?FD_INPUT(Fid),
            fid=Fid,
            show_upload = true,
            show_recipients = false,
            show_title = false,
            show_body=true,
            media_thumbs = false,
            upload_title= <<"Title">>,
            upload_dir= ?FILE_DIR,
            post_upload = attach_file,
            delegate_query = product,
            img_tool = undefined,
            entry_type=bundles,
            recipients=[{product, P#product.id, lists:keyfind(bundles, 1, P#product.feeds)}],
            control_title = <<"upload file">>
        },
        wf:session(Fid, State),
        wf:session(?FD_INPUT(Fid),Is),

        #feed_ui{title="product files", state=State, header=[#input{state=Is}]} end.

aside()->
    DiscussionState = ?FD_STATE(?FEED(comment))#feed_state{
        flat_mode=true,
        view=comment,
        entry_type=comment,
        entry_id=#comment.comment_id},

    EntriesState = ?FD_STATE(?FEED(entry))#feed_state{
        flat_mode=true,
        entry_id=#entry.entry_id},

    #aside{class=[sidebar], body=[
%      #panel{class=["sidebar-widget"], body=[
%        #feed_ui{
%            title = <<"More shooters">>,
%            state = EntriesState } ]},

%      #panel{class=["sidebar-widget"], body=[
%        #feed_ui{
%            title = <<"Recent posts">>,
%            state = EntriesState } ]},

      #panel{class=["sidebar-widget"], body=[
        #feed_ui{
            title= <<"Active discussion">>,
            icon="icon-comments-alt",
            class="comments-flat",
            state=DiscussionState} ]} ]}.

controls(#entry{type=Type} =  E) -> [
  #link{body=[case Type of product -> <<"view ">>; _-> <<"read more ">> end, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, Type, E#entry.id}} ].


% Render file entries

render_element(#div_entry{entry=E, state=#feed_state{view=files}=S})->
    Uid = wf:to_list(erlang:phash2(element(S#feed_state.entry_id, E))),
    Panel =#panel{class=["file-entry"],body=[
        #p{id=?EN_DESC(Uid), body=E#entry.description},
        [#entry_media{media=M, mode=files} ||M <- E#entry.media]
    ]},
    element_panel:render_element(Panel);

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
                #figcaption{class=["thumbnail-title"], body=[#h4{body=#span{body=wf:js_escape(E#entry.title)}}]}; true -> [] end ]},

        #panel{id=?EN_DESC(UiId), body=product_ui:shorten(wf:js_escape(E#entry.description)), data_fields=[{<<"data-html">>, true}]},

        #footer{class=["blog-footer", "row-fluid"], body=[
            #link{body=[ #i{class=["icon-comments-alt", "icon-large"]},
                #span{class=[?EN_CM_COUNT(UiId)], body=integer_to_list(kvs_feed:comments_count(entry, Id))}],
                postback={read, entry, Id}},
            #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}],
                postback={read, entry, Eid}} ]} ]},
    element_panel:render_element(Entry);
render_element(E)-> feed_ui:render_element(E).

% Events

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);

event({edit_entry, E=#entry{title=Title, description=Desc}, ProdId, MsId}) ->
  Tid = ?EN_TITLE(E#entry.entry_id), Did = ?EN_DESC(E#entry.entry_id), Toid = ?EN_TOOL(E#entry.entry_id),
  Dir = "static/"++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:replace(Tid, #textbox{id=Tid, value=wf:js_escape(Title)}),
  wf:replace(Did, #panel{body=[#htmlbox{id=Did, html=wf:js_escape(Desc), root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, post_target=MsId, size=?THUMB_SIZE}]}),
  wf:update(Toid, #panel{class=["btn-toolbar"], body=[
    #link{postback={save_entry, E, ProdId}, source=[Tid, Did], class=[btn, "btn-large", "btn-success"], body= <<"Save">>},
    #link{postback={cancel_entry, E#entry{title=wf:js_escape(Title), description=wf:js_escape(Desc)}}, class=[btn, "btn-large", "btn-info"], body= <<"Cancel">>}
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

  [msg:notify([kvs_feed, RouteType, To, entry, Fid, delete], [E, (wf:user())#user.email]) || {RouteType, To, Fid} <- Recipients];

event({read, entry, Id})-> wf:redirect("/review?id="++Id);
event({checkout, Pid}) -> wf:redirect("/checkout?product_id="++Pid);
event({add_cart, P}) ->
    store:event({add_cart, P}),
    wf:redirect("/shopping_cart");
event(Event) -> error_logger:info_msg("[product]Page event: ~p", [Event]), [].

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    error_logger:info_msg("Show tab ~p", [Id]),
    P = wf:session(product),
    case lists:keyfind(list_to_atom(Id), 1, P#product.feeds) of false -> ok;
    {_,_}=Tab -> wf:update(list_to_atom(Id), feed(P, Tab)) end,
    wf:wire("Holder.run();");

api_event(Name,Tag,Term) -> error_logger:info_msg("[product] api Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

control_event(_, {query_file, Root, Dir, File, MimeType, _PostWrite, Target})->
    FileName = filename:join([Root,Dir,binary_to_list(File)]),

    {exist, case file:read_file_info(FileName) of {error, _} -> 0;
    {ok, FileInfo} ->
        Url = filename:join([Dir,binary_to_list(File)]),
        Media = #media{id=element_upload:hash(FileName), url = Url, type ={attachment,MimeType}},
        wf:session(medias, [Media]),
        error_logger:info_msg("[product]query_file ~p", [Media]),
        wf:update(Target, input:media_preview(Target, [Media])),
        wf:wire("Holder.run();"),
        FileInfo#file_info.size end}.

process_delivery([_,_,entry,_,edit], #entry{entry_id=Id, title=Title, description=Desc, media=Media}=E) ->
  wf:session(medias, []),
  Tid = ?EN_TITLE(Id), Did = ?EN_DESC(Id),
  wf:replace(Tid, #span{id =Tid, body=wf:js_escape(Title)}),
  wf:replace(Did, #panel{id=Did, body=wf:js_escape(Desc), data_fields=[{<<"data-html">>, true}]}),
  wf:update(?EN_MEDIA(Id), #entry_media{media=Media, mode=reviews}),
  wf:update(?EN_TOOL(Id), feed:controls(E)),
  wf:wire("Holder.run();");

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
