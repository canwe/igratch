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
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}], alt="no media", class=[]});
render_element(#entry_media{media=[], mode=reviews}) -> 
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt="no media", class=[]});
render_element(#entry_media{media=[#media{thumbnail_url=undefined, title=T}|_], mode=reviews}) ->
  element_image:render_element(#image{data_fields=[{<<"data-src">>,<<"holder.js/270x124/text:no media">>}],alt=T, class=[]});
render_element(#entry_media{media=[#media{title=Title, thumbnail_url=Thumb}|_], mode=reviews}) ->
  Ext = filename:extension(Thumb),
  Name = filename:basename(Thumb, Ext),
  Dir = filename:dirname(Thumb),
  element_image:render_element(#image{alt=Title, image=filename:join([Dir, Name++"_270x124"++Ext])});
render_element(#entry_media{media=Media, fid=Fid}) ->
  element_panel:render_element(#panel{body=[]}).

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
