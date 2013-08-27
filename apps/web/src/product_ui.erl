-module(product_ui).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

render_element(#product_cart{product=P}) ->
  From = P#product.owner,

  Entry = #panel{class=["row-fluid", article], style="margin-bottom:10px;padding-bottom:10px;", body=[
    #panel{class=[span12, username], body=[
      <<"From: ">>,#link{body=From}
    ]},
    #panel{class=[span12], body =[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[thumbnail], body=[
            #image{image= case P#product.cover of undefined -> <<"holder.js/100%x100/text:no cover">>;
              Th ->
                Ext = filename:extension(Th),
                Name = filename:basename(Th, Ext),
                Dir = filename:dirname(Th),
                filename:join([Dir, "thumbnail", Name++"_270x124"++Ext]) end}
          ]}
        ]},
        #panel{class=[span6], style="overflow:hidden", body=[
          #h3{body= P#product.title},
          #p{body= [P#product.brief,
            #link{class=[more], body=[<<"view ">>, #i{class=["icon-double-angle-right", "icon-large"]}], url="/product?id="++P#product.id }
          ]}
        ]},
        #panel{class=[span2], body=[ #h4{body= <<"Quantity:">>}, #textbox{class=[span12], value=1, disabled=true},
          #link{class=[more], body=[#span{class=["icon-remove-circle"]}, <<" remove">>]}
        ]},
        #panel{class=[span1], body=[#span{class=["icon-usd"]}, #b{body=float_to_list(P#product.price/100, [{decimals, 2}])}  ]} 
      ]}
    ]}
  ]},

  element_panel:render_element(Entry);

render_element(#product_row{product=P}) ->
  Date = to_date(P#product.created),

  Row = #tr{id=wf:temp_id(), postback={product_feed, P#product.id},cells=[
    #td{body=[
      #h4{body = P#product.title},
      #p{body=[#span{style="display:block;", body = P#product.creator}, #small{body= Date}]},
      #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ]},
      #link{url="#",body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ]} ]},
    #td{body= P#product.brief}
  ]},
  element_tr:render_element(Row);

render_element(#product_hero{product=P}) ->
  Hero = #panel{class=["row-fluid"], body=[
    #panel{class=[span6], body=[
    #panel{class=["hero-unit"], body=[
        #h2{body=P#product.title},
        #p{body=P#product.brief},
        #list{class=[unstyled], body=[
          #li{body= <<"Game rating: Ages 16+">>}
        ]},
        #panel{body=#span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]}},
        #panel{class=["btn-toolbar", "text-center"], body=[
          #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win],
            body= [<<"buy for ">>, #span{body= "$"++ float_to_list(P#product.price/100, [{decimals, 2}]) }], postback={checkout, P#product.id}},
          #button{class=[btn, "btn-large", "btn-warning"], body= [#span{class=["icon-shopping-cart"]}, <<" add to cart ">>], postback={add_cart, P}}
        ]}
      ]}
    ]},
    #panel{class=[span6, "text-center"], body=[
      #image{image=P#product.cover}
    ]}
  ]},
  element_panel:render_element(Hero);

render_element(#product_entry{entry=#entry{type={features, "jumbotron"}}=E}) ->
  error_logger:info_msg("View desctiption entry with jumbotron layout ~p~n", [E]),
  PostId = wf:temp_id(),
  Entry = #panel{id=PostId, class=["blog-post", "jumbotron"],body=[
    <<"Jumbotron">>
  ]},
element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type={features, _}}=E, prod_id=ProdId})->
  PostId = E#entry.entry_id,
  EntryId = ?ID_DESC(PostId),
  TitleId = ?ID_TITLE(PostId),
  Ms = E#entry.media,
  EntryActionsLine = #list{class=[unstyled, inline], style="display:inline-block;", body=[
              #li{body=#link{body= <<"Edit">>, postback={edit_entry, E, ProdId, wf:temp_id()}, source=[TitleId, EntryId]}},
              #li{body=#link{body= <<"Remove">>, postback={remove_entry, E, ProdId}}}
            ]},
  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
          #h2{body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}]}
    ]},
    [#entry_media{media=M, fid=E#entry.entry_id} || M <- Ms],

    #panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

    #footer{class=["blog-footer", "row-fluid"], body=[
      EntryActionsLine
    ]}
  ]},
  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type=Type}=E, mode=line, category=Category, controls=Controls})->
  Id = E#entry.entry_id,
  From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,

  Entry = #panel{id=E#entry.entry_id, class=["row-fluid", article], body=[
    #panel{class=[span3, "article-meta"], body=[
      #h3{class=[blue], body= Category},
      #p{class=[username], body= #link{body=From, url= "/profile?id="++E#entry.from}},
      #p{class=[datestamp], body=[ #span{body= to_date(E#entry.created)} ]},
      #p{class=[statistics], body=[
        #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ]},
        #link{url="#",body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ]}
      ]} ]},

      #panel{id=?ID_MEDIA(Id), class=[span4, shadow], body = #entry_media{media=E#entry.media, mode=reviews}},
      #panel{class=[span5, "article-text"], body=[
        #h3{body=#span{id=?ID_TITLE(Id), class=[title], body= E#entry.title}},
        #p{id = ?ID_DESC(Id), body=shorten(E#entry.description)},
        #panel{id=?ID_TOOL(Id), class=[more], body=Controls}
      ]}
  ]},

  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type=Type}=E, mode=full})->
  PostId = E#entry.entry_id,
  TitleId = ?ID_TITLE(PostId),
  EntryId= ?ID_DESC(PostId),
  {_, Fid} = lists:keyfind(comments, 1, E#entry.feeds),
  Comments = kvs:entries(kvs:get(feed, Fid), comment),
  CommentId = wf:temp_id(),
  CommentsId = wf:temp_id(),
  Ms = E#entry.media,
  Dir = "static/"++case wf:user() of undefined->"anonymous"; User-> User#user.email end,
  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #h3{class=[blue], body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]} ]},
    #figure{class=["thumbnail-figure"], body=[
      [#entry_media{media=M, fid=E#entry.entry_id} || M <- Ms]
    ]},
    #panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]},
    #panel{class=[comments, "row-fluid"], body=[
        #h3{body= <<"comments">>},
        #panel{id=CommentsId, class=[], body=[#entry_comment{comment=C}||C<-Comments]},
        #h3{class=["comments-form"], body= <<"Add your comment">>},
        #htmlbox{id=CommentId, root=?ROOT, dir=Dir, post_write=attach_media, img_tool=gm, size=?THUMB_SIZE},
        #panel{class=["btn-toolbar"], body=[#link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, E#entry.id, CommentId, CommentsId, undefined, ""}, source=[CommentId]}]}
      ]}
  ]},

  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{}=E, prod_id=ProdId})->
  PostId = E#entry.entry_id,
  EntryId = ?ID_DESC(PostId),
  TitleId = ?ID_TITLE(PostId),
  Ms = E#entry.media,
  From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
  EntryActionsLine = [
    #link{body= [#i{class=["icon-edit", "icon-large"]}, <<" edit">>], postback={edit_entry, E, ProdId, wf:temp_id()}, source=[TitleId, EntryId]},
    #link{body= [#i{class=["icon-remove", "icon-large"]},<<" remove">>], postback={remove_entry, E, ProdId}}
  ],

  Date = to_date(E#entry.created),

  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
      #h2{body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, #small{body=[<<" by ">>, #link{body=From}, Date]}]}
    ]},
    #figure{class=["thumbnail-figure"], body=[
      [#entry_media{media=Me, fid=E#entry.entry_id} || Me <- Ms],
      #figcaption{class=["thumbnail-title"], body=[
%            #h3{body=#span{body= E#entry.title}}
      ]}
    ]},
    #panel{id=EntryId, body=wf:js_escape(E#entry.description), data_fields=[{<<"data-html">>, true}]},
    #panel{id=?ID_TOOL(PostId)},

    #footer{class=["blog-footer", "row-fluid"], body=[
      #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ], postback={read, entry, E#entry.id}},
      #link{body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ], postback={read, entry, E#entry.id}},
      EntryActionsLine,
      #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read, entry, E#entry.id}}
    ]}
  ]},
  element_panel:render_element(Entry);

render_element(#entry_comment{comment=#comment{}=C})->
  {Cid, {Eid, Fid}} = C#comment.id,
  {Author, Avatar} = case kvs:get(user, C#comment.from) of 
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,

  Date = to_date(C#comment.created),
  Entries = case lists:keyfind(comments, 1, C#comment.feeds) of
    {_, CFid} -> kvs:entries(kvs:get(feed, CFid), comment);
    _-> [] end,

  Comment = #panel{class=[media, "media-comment"], body=[
    #link{class=["pull-left"], body=[Avatar]},
    #panel{id=C#comment.comment_id, class=["media-body"], body=[
        #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
        #p{body= C#comment.content},
        #p{body= [#entry_media{media=M, fid=Fid, cid = Cid} ||  M <- C#comment.media]},
        #p{class=["media-heading"], body=[
          #link{class=["comment-reply"], body=[ <<"reply ">>, #i{class=["icon-reply", "icon-large"]}], postback={comment_reply, C#comment.id}}
        ]},
        #panel{body=[#entry_comment{comment=C} || C <- Entries ]}
    ]}
  ]},
  element_panel:render_element(Comment);

render_element(#entry_media{media=undefined, mode=reviews}) -> 
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}], alt="no media", class=["BorderAndShadow"]});
render_element(#entry_media{media=[], mode=reviews}) -> 
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt="no media", class=["BorderAndShadow"]});
render_element(#entry_media{media=[#media{thumbnail_url=undefined, title=T}|_], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt=T, class=["BorderAndShadow"]});
render_element(#entry_media{media=[#media{title=Title, thumbnail_url=Thumb}|_], mode=reviews}) ->
  Ext = filename:extension(Thumb),
  Name = filename:basename(Thumb, Ext),
  Dir = filename:dirname(Thumb),
  element_image:render_element(#image{class=["BorderAndShadow"], alt=Title, image=filename:join([Dir, Name++"_270x124"++Ext])});
render_element(#entry_media{media=Media, fid=Fid}) ->
  M = #panel{body=[
%    #image{image=Media#media.url}
  ]},
  element_panel:render_element(M);

render_element(#feature_req{entry=E})->
  From = case kvs:get(user, E#entry.from) of {ok, U} -> U#user.display_name; {error, _} -> E#entry.from end,
  User = wf:user(),
  Admin = kvs_acl:check_access(User#user.email, {feature, admin})==allow, 
  R = #panel{id=E#entry.entry_id, style="border-bottom:1px solid #eeeeee;", body=[
    #p{body=[
      #small{body=["[", to_date(E#entry.created), "] "]},
      #link{body= if From == User#user.email -> <<"you">>; true -> From end, url= "/profile?id="++E#entry.from},
      <<" ">>,
      wf:js_escape(wf:to_list(E#entry.title)),
      case E#entry.type of {feature, _}-> #b{body=io_lib:format(" ~p", [E#entry.type])}; _-> [] end
    ]},
    #p{body= E#entry.description},
    case E#entry.type of 
      {feature, F} when Admin ->
        #panel{class=["btn-toolbar"], body=[
          #link{class=[btn, "btn-success"], body= <<"allow">>, postback={allow, E#entry.from, E#entry.entry_id, E#entry.type}},
          #link{class=[btn, "btn-info"], body= <<"reject">>, postback={cancel, E#entry.from, E#entry.entry_id, E#entry.type}}
        ]};
      _ -> [] end
  ]},
  element_panel:render_element(R).

preview_medias(Id, Medias, Delegate)->
  L = length(Medias),
  if L > 0 ->
    #carousel{indicators=false, style="border:1px solid #eee;", items=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[close], style="position:absolute; right:10px;top:5px; color:red;",  body= <<"&times;">>, postback={remove_media, M, Id}, delegate=Delegate},
          #link{class=[thumbnail], body=[
            #image{image= case M#media.thumbnail_url of undefined -> <<"holder.js/100%x80">>;
              Th ->
                Ext = filename:extension(Th),
                Name = filename:basename(Th, Ext),
                Dir = filename:dirname(Th),
                filename:join([Dir, Name++"_139x80"++Ext])
             end}
          ]}
        ]}|| M <- lists:sublist(Medias, I, 4)
      ]}|| I <- lists:seq(1, L, 4) ],
      caption=#panel{body= <<"Entry will be posted with this medias.">>}};
    true-> [] end.

timestamp_label({0, _}, Time) ->
  {_, H} = calendar:now_to_local_time(Time),
  io_lib:format("~2..0b:~2..0b:~2..0b", tuple_to_list(H));
timestamp_label({Days, _}, _) when Days < 7 -> io_lib:format("~p " ++ "days ago", [Days]);
timestamp_label({Days, _}, _) when Days < 31 -> io_lib:format("~p " ++ "weeks ago", [trunc(Days/7)]);
timestamp_label({Days, _}, _) when Days < 365 -> io_lib:format("~p " ++ "months ago", [trunc(Days/30)]);
timestamp_label({Days, _}, _) when Days > 365 -> io_lib:format("~p " ++ "years ago", [trunc(Days/365)]);
timestamp_label({Days, _}, _) -> io_lib:format("~p days ago", [Days]).

shorten(Input) ->
  re:replace(re:replace(re:replace(re:replace(re:replace(re:replace(re:replace(re:replace(Input,
    "<img[^>]*>",           "...",  [global, {return, list}]),
      "\\s+$",              "",     [global, {return, list}]),
        "^\\s+",            "",     [global, {return, list}]),
          "\n",            "<br/>", [global, {return, list}]),
            "<br[\\s+]/>",  "",     [global, {return, list}]),
              "<p></p>",    "",     [global, {return, list}]),
                "<p>",      "",     [global, {return, list}]),
                  "</p>",   "",     [global, {return, list}]).

to_price(Str)->
  PriceStr2 = case string:to_float(Str) of {error, no_float} -> Str; {F, _} -> float_to_list(F, [{decimals, 2}]) end,
  {P1, Rest} = case string:to_integer(PriceStr2) of {error, no_integer} -> {0, "0"}; {Pa, [_|Ra]} -> {Pa, Ra}; {Pa, Ra} -> {Pa, Ra} end,
  P2 = case string:to_integer(Rest) of {error,no_integer}-> 0; {Re,_} -> Re  end,
  P2+P1*100.

to_date(undefined) -> to_date(now());
to_date(Date)->
  {{Y, M, D}, _} = calendar:now_to_datetime(Date),
  io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]).
