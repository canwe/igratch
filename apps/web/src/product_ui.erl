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

render_element(#product_entry{entry=#entry{type={features, "figure"}}=E}) ->
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  EntryActionsLine = #list{class=[unstyled, inline, "pull-right"], style="display:inline-block;", body=[
              #li{body=#link{style="color:white", body= <<"Edit">>, postback={edit_entry, E, TitleId, EntryId}, source=[TitleId, EntryId]}},
              #li{body=#link{style="color:white", body= <<"Remove">>, postback={remove_entry, E, PostId}}}
            ]},
  Entry = #panel{id=PostId, class=["blog-post", "figure"], body=[
    #figure{style="margin:0;position:relative;", body=[
      [#entry_media{media=M, fid=E#entry.entry_id} || M <- E#entry.media],
      #figcaption{style="position:absolute; top:35%%; left:0;", body=[
        #panel{style="margin-left:40px;color:white;text-shadow:none;", body=#h1{body=#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}}},
        #panel{style="margin-left: 40px;color:white;text-shadow:none;", body=#panel{id=EntryId, body=E#entry.description, data_fields=[{<<"data-html">>, true}]}}
      ]}
    ]},
    #footer{style="position:relative;", body=#panel{class=["row-fluid"], style="position:absolute; bottom:0; right:0;", body=[EntryActionsLine]}}
  ]},
  element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type={features, "jumbotron"}}=E}) ->
  error_logger:info_msg("View desctiption entry with jumbotron layout ~p~n", [E]),
  PostId = wf:temp_id(),
  Entry = #panel{id=PostId, class=["blog-post", "jumbotron"],body=[
    <<"Jumbotron">>
  ]},
element_panel:render_element(Entry);

render_element(#product_entry{entry=#entry{type={features, _}}=E})->
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Ms = E#entry.media,
  EntryActionsLine = #list{class=[unstyled, inline], style="display:inline-block;", body=[
              #li{body=#link{body= <<"Edit">>, postback={edit_entry, E, TitleId, EntryId}, source=[TitleId, EntryId]}},
              #li{body=#link{body= <<"Remove">>, postback={remove_entry, E, PostId}}}
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

render_element(#product_entry{entry=#entry{type={reviews, _}}=E, mode=full})->
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Comments = kvs_comment:feed_comments({E#entry.id, E#entry.feed_id}),
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
    #panel{class=[comments], body=[
        #h3{body= <<"5 comments">>},
        comment([comment(), comment([comment()])]),
        comment()
      ]},
      #panel{class=["comments-form"], body=[
        #h3{class=["comments-form"], body= <<"Add your comment">>},
        #panel{class=["form-horizontal"], body=[
          #fieldset{body=[
            #panel{class=["control-group"], body=[
              #label{class=["control-label"], for="email", body= <<"Email">>},
              #panel{class=["controls"], body=[
                #textbox{id=email, class=["input-xxlarge"]}
              ]}
            ]},
            #panel{class=["control-group"], body=[
              #label{class=["control-label"], for="message", body= <<"Your message">>},
              #panel{class=["controls"], body=[
                #textarea{id=message, class=["input-xxlarge"]}
              ]}
            ]},
            #panel{class=["control-group"], body=[
              #panel{class=["controls"], body=[
                #button{class=[btn, "btn-info", "btn-large"], body= <<"send">>}
              ]}
            ]}
          ]}
        ]}
      ]}
  ]},

  element_panel:render_element(Entry);

render_element(#product_entry{entry=E})->
  error_logger:info_msg("Render entry: ~p ~p", [E#entry.id, E#entry.type]),
  PostId = wf:temp_id(),
  EntryId= wf:temp_id(),
  TitleId = wf:temp_id(),
  Comments = kvs_comment:feed_comments({E#entry.id, E#entry.feed_id}),
  Ms = E#entry.media,
  From = case kvs:get(user, E#entry.from) of {ok, User} -> User#user.display_name; {error, _} -> E#entry.from end,
  EntryActionsLine = [
    #link{body= [#i{class=["icon-edit", "icon-large"]}, <<" edit">>], postback={edit_entry, E, TitleId, EntryId}, source=[TitleId, EntryId]},
    #link{body= [#i{class=["icon-remove", "icon-large"]},<<" remove">>], postback={remove_entry, E, PostId}}
  ],
  {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
  Entry = #panel{id=PostId, class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
          #h2{body=[#span{id=TitleId, body=E#entry.title, data_fields=[{<<"data-html">>, true}]}, #small{body=[<<" by ">>, #link{body=From}, Date]}]}
    ]},
    #figure{class=["thumbnail-figure"], body=[
      [#entry_media{media=M, fid=E#entry.entry_id} || M <- Ms],
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

render_element(#entry_media{media=Media, fid=Fid}) ->
  error_logger:info_msg("RENDER: ~p", [Media]),
  M = #panel{body=[
    #image{image=Media#media.url}
  ]},
  element_panel:render_element(M).



% -templates waiting for apply
comment() -> comment([]).
comment(InnerComment)->
  #panel{class=[media, "media-comment"], body=[
    #link{class=["pull-left"], body=[
      #image{class=["media-objects","img-circle"], data_fields=[{<<"data-src">>, <<"holder.js/64x64">>}]}
    ]},
    #panel{class=["media-body"], body=[
      #p{class=["media-heading"], body=[
        <<"John Doe, 12 Sep 2012.">>, 
        #link{class=["comment-reply","pull-right"], body= <<"reply">>}
      ]},
      #p{body= <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.">>},
      InnerComment
    ]}
  ]}.
