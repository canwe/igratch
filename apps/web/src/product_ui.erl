-module(product_ui).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

render_element(#product_row{product=P}) ->
  {{Y, M, D}, _} = calendar:now_to_datetime(P#product.creation_date),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),

  Row = #tr{id=wf:temp_id(), postback={product_feed, P#product.id},cells=[
    #td{body=[
      #h4{body = P#product.name},
      #p{body=[#span{style="display:block;", body = P#product.creator}, #small{body= Date}]},
      #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ]},
      #link{url="#",body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ]} ]},
    #td{body= P#product.title},
    #td{body= P#product.brief}
  ]},
  element_tr:render_element(Row);

render_element(#product_line{product=P, meta=Meta, controls=Controls})->
  Line = #panel{class=[span12, "game-article", "shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span2, "article-meta"], body=Meta},

      #panel{class=[span3, shadow], body=[
        #image{class=["border"], alt= P#product.title, image=P#product.cover}
      ]},
      #panel{class=[span5, "article-text"], body=[
        #p{body=[P#product.brief, #link{url= <<"#">>, body= <<"Read">>}]}
      ]},
      #panel{class=[span2, "dev-controls"], body=Controls}
    ]}
  ]},
  element_panel:render_element(Line);

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
        #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win],
          body= [<<"buy for ">>,#span{body= "$"++ if is_float(P#product.price) -> float_to_list(P#product.price, [{decimals, 2}, compact]); is_integer(P#product.price) -> integer_to_list(P#product.price);true-> [0] end }],
          postback={product, integer_to_list(P#product.id)}}
      ]}
    ]},
    #panel{class=[span6, "text-center"], body=[
      #image{image=P#product.cover}
    ]}
  ]},
  element_panel:render_element(Hero);

%render_element(#product_entry{entry=#entry{type={features, "figure"}}=E}) ->
%  PostId = wf:temp_id(),
%  EntryId= wf:temp_id(),
%  TitleId = wf:temp_id(),
%  EntryActionsLine = #list{class=[unstyled, inline, "pull-right"], style="display:inline-block;", body=[
%              #li{body=#link{style="color:white", body= <<"Edit">>, postback={edit_entry, E, TitleId, EntryId}, source=[TitleId, EntryId]}},
%              #li{body=#link{style="color:white", body= <<"Remove">>, postback={remove_entry, E, PostId}}}
%            ]},
%  Entry = #panel{id=PostId, class=["blog-post", "figure"], body=[
%    #figure{style="margin:0;position:relative;", body=[
%      [#entry_media{media=M, fid=E#entry.entry_id} || M <- E#entry.media],
%      #figcaption{style="position:absolute; top:35%%; left:0;", body=[
%        #panel{style="margin-left:40px;color:white;text-shadow:none;", body=#h1{body=#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}}},
%        #panel{style="margin-left: 40px;color:white;text-shadow:none;", body=#panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]}}
%      ]}
%    ]},
%    #footer{style="position:relative;", body=#panel{class=["row-fluid"], style="position:absolute; bottom:0; right:0;", body=[EntryActionsLine]}}
%  ]},
%  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type={features, "jumbotron"}}=E}) ->
  error_logger:info_msg("View desctiption entry with jumbotron layout ~p~n", [E]),
  PostId = wf:temp_id(),
  Entry = #panel{id=PostId, class=["blog-post", "jumbotron"],body=[
    <<"Jumbotron">>
  ]},
element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type={features, _}}=E, prod_id=ProdId})->
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Ms = E#entry.media,
  EntryActionsLine = #list{class=[unstyled, inline], style="display:inline-block;", body=[
              #li{body=#link{body= <<"Edit">>, postback={edit_entry, E, ProdId, TitleId, EntryId, wf:temp_id()}, source=[TitleId, EntryId]}},
              #li{body=#link{body= <<"Remove">>, postback={remove_entry, E, ProdId, PostId}}}
            ]},
  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
          #h2{body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}]}
    ]},
%    #figure{class=["thumbnail-figure"], body=[

      [#entry_media{media=M, fid=E#entry.entry_id} || M <- Ms],

%      #figcaption{class=["thumbnail-title"], body=[
%            #h3{body=#span{body= E#entry.title}}
%      ]}
%    ]},
    #panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

    #footer{class=["blog-footer", "row-fluid"], body=[
      EntryActionsLine
    ]}
  ]},
  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{}=E, mode=line, category=Category})->
  From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
  {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),

  Short = re:replace(re:replace(re:replace(re:replace(re:replace(re:replace(re:replace(E#entry.description,
    "<img[^>]*>",           "...", [global, {return, list}]),
      "\\s+$",              "", [global, {return, list}]),
        "^\\s+",            "", [global, {return, list}]),
          "<br[\\s+]/>",    "", [global, {return, list}]),
            "<p></p>",      "", [global, {return, list}]),
              "<p>",        "", [global, {return, list}]),
                "</p>",     "", [global, {return, list}]),

  Entry = #panel{class=["row-fluid", article], body=[
    #panel{class=[span3, "article-meta"], body=[
      #h3{ class=[blue], body= Category},
      #p{class=[username], body= #link{body=From}},
      #p{class=[datestamp], body=[ #span{body= Date} ]},
      #p{class=[statistics], body=[
        #link{url="#",body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ]},
        #link{url="#",body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ]}
      ]} ]},

      #panel{class=[span4, shadow], body = #entry_media{media=E#entry.media, mode=reviews}},
      #panel{class=[span5, "article-text"], body=[
        #h3{class=[title], body= E#entry.title},
        Short, #link{class=[more], body=[<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read_entry, E#entry.id}} ]} ]},

  element_panel:render_element(Entry);


render_element(#product_entry{entry=#entry{type={reviews, _}}=E, mode=full})->
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Comments = kvs_comment:read_comments(E#entry.comments_rear),
  CommentId = wf:temp_id(),
  CommentsId = wf:temp_id(),
  User = wf:user(),
  Ms = E#entry.media,
  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
          #h2{body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}]}
    ]},
    #figure{class=["thumbnail-figure"], body=[
      [#entry_media{media=M, fid=E#entry.entry_id} || M <- Ms],
      #figcaption{class=["thumbnail-title"], body=[
%            #h3{body=#span{body= E#entry.title}}
      ]}
    ]},
    #panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]},
    #panel{class=[comments, "row-fluid"], body=[
        #h3{body= <<"5 comments">>},
        #panel{id=CommentsId, class=[], body=[#entry_comment{comment=C}||C<-Comments]},
        #h3{class=["comments-form"], body= <<"Add your comment">>},
        #htmlbox{id=CommentId, root=?ROOT, dir="static/"++User#user.email, post_write=attach_media, img_tool=gm, size=[{270, 124}, {200, 200} , {139, 80}]},
        #panel{class=["btn-toolbar"], body=[#link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, E#entry.id, CommentId, CommentsId, undefined, ""}, source=[CommentId]}]}
      ]}
  ]},

  element_panel:render_element(Entry);

render_element(#product_entry{entry=E, prod_id=ProdId})->
%  error_logger:info_msg("Render entry: ~p ~p", [E#entry.id, E#entry.type]),
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Ms = E#entry.media,
  From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
  EntryActionsLine = [
    #link{body= [#i{class=["icon-edit", "icon-large"]}, <<" edit">>], postback={edit_entry, E, ProdId, TitleId, EntryId, wf:temp_id()}, source=[TitleId, EntryId]},
    #link{body= [#i{class=["icon-remove", "icon-large"]},<<" remove">>], postback={remove_entry, E, ProdId, PostId}}
  ],
  {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
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
    #panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]},

    #footer{class=["blog-footer", "row-fluid"], body=[
      #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ], postback={read_entry, E#entry.id}},
      #link{body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ], postback={read_entry, E#entry.id}},
      EntryActionsLine,
      #link{class=["pull-right"], body= [<<"read more ">>, #i{class=["icon-double-angle-right", "icon-large"]}], postback={read_entry, E#entry.id}}
    ]}
  ]},
  element_panel:render_element(Entry);

render_element(#entry_comment{comment=#comment{}=C})->
  {Cid, {Eid, Fid}} = C#comment.id,
  {Author, Avatar} = case kvs:get(user, C#comment.author_id) of 
      {ok, User} -> {User#user.display_name, case User#user.avatar of
        undefined-> #image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>};
        Img-> #image{class=["media-object", "img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end};
      {error, _}-> {<<"John">> ,#image{class=["media-objects","img-circle"], image= <<"holder.js/64x64">>}} end,
  {{Y, M, D}, _} = calendar:now_to_datetime(C#comment.creation_time),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),

  Comment = #panel{class=[media, "media-comment"], body=[
    #link{class=["pull-left"], body=[Avatar]},
    #panel{id=C#comment.comment_id, class=["media-body"], body=[
        #p{class=["media-heading"], body=[#link{body= Author}, <<",">>, Date ]},
        #p{body= C#comment.content},
        #p{body= [#entry_media{media=M, fid=Fid, cid = Cid} ||  M <- C#comment.media]},
        #p{class=["media-heading"], body=[
          #link{class=["comment-reply"], body=[ <<"reply ">>, #i{class=["icon-reply", "icon-large"]}], postback={comment_reply, C#comment.id}}
        ]},
        #panel{body=
        case C#comment.comments_rear of undefined -> []; Crs ->
            [#entry_comment{comment=C} || C <- kvs_comment:read_comments(Crs)]
        end}
    ]}
  ]},
  element_panel:render_element(Comment);

render_element(#entry_media{media=undefined, mode=reviews}) -> 
  element_image:render_element(#image{image="holder.js/270x124/text:no media", alt="Row Three Image", class=["BorderAndShadow"]});
render_element(#entry_media{media=[], mode=reviews}) -> 
  element_image:render_element(#image{image="holder.js/270x124/text:no media", alt="Row Three Image", class=["BorderAndShadow"]});
render_element(#entry_media{media=[#media{thumbnail_url=undefined, title=T}|_], mode=reviews}) ->
  element_image:render_element(#image{image="holder.js/270x124/text:no media", alt=T, class=["BorderAndShadow"]});
render_element(#entry_media{media=[#media{title=Title, thumbnail_url=Thumb}|_], mode=reviews}) ->
  Ext = filename:extension(Thumb),
  Name = filename:basename(Thumb, Ext),
  Dir = filename:dirname(Thumb),
  element_image:render_element(#image{class=["BorderAndShadow"], alt=Title, image=filename:join([Dir, Name++"_270x124"++Ext])});
render_element(#entry_media{media=Media, fid=Fid}) ->
  M = #panel{body=[
%    #image{image=Media#media.url}
  ]},
  element_panel:render_element(M).

preview_medias(Id, Medias)->
  L = length(Medias),
  if L > 0 ->
    #carousel{indicators=false, style="border:1px solid #eee;", items=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], style="position:relative;", body=[
          #link{class=[close], style="position:absolute; right:10px;top:5px;",  body= <<"&times;">>, postback={remove_media, M, Id}},
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


