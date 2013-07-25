-module(account).
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/attachments.hrl").
-include_lib("kvs/include/membership.hrl").

-record(game, {
  title = <<"Kingdoms of Amalur: Reckoning">>,
  developer= <<"Electronic Arts">>,
  purchased_date=erlang:now(),
  submit_date=erlang:now(),
  upload_date=erlang:now(),
  image = <<"/static/img/row4.jpg">>,
  description= <<"Ken Rolston was the game's executive designer, R. A. Salvatore created the game universe and lore, with Todd McFarlane working on the artwork, and Grant Kirkhope creating the musical score. It was developed by 38 Studios and Big Huge Games. The game was released on February 7, 2012, in North America and on February 9, 2012, in Europe...">>
}).

-record(price, {?ELEMENT_BASE(account), value, currencies=[{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}], span}).
-record(product_row, {?ELEMENT_BASE(account), product=[]}).
-record(product_entry, {?ELEMENT_BASE(account), entry=[]}).
-record(entry_media, {?ELEMENT_BASE(account), media=[], fid}).

-define(ROOT, code:priv_dir(web)++"/static").

main()-> #dtl{file="dev", bindings=[{title,<<"account">>},{body, body()}]}.

body()->
  index:header() ++ [
  #panel{id="main-container", class=["container", "main-no-slider", account], body=#panel{body=[
    profile_ctl(),
    #panel{class=["row-fluid", "tab-content"], body=[
      #panel{id=user, class=["tab-pane"], body= inputs(user)},
      #panel{id=reviewer, class=["tab-pane"], body=inputs(reviewer)},
      #panel{id=developer, class=["tab-pane", active], body=[
        inputs(developer),
        #h3{body= <<"your products">>},
        #table{id=products, class=[table, "table-hover"], body=[[#product_row{product=P} || P <- kvs:all(product)]] }
      ]},
      #panel{id=product_feed, class=["tab-pane"]}
    ]}
%    #panel{id="account-games", class=["row-fluid"], body=account_games(user)}
  ]}} ] ++ index:footer().


profile_ctl() ->
  Controls = [
    #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-pencil"]}, <<" Edit Profile">>]},
    #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-share"]}, <<" Upgrade">>]}],

  #panel{class=["row-fluid"], body=[
    #h3{class=[blue], body= <<"My Account">>},
    #panel{class=["control-panel", clearfix], body=[
      #list{id=ctl, class=[nav, "nav-pills", "control-roles", span8], body=[
        #li{class=[], body=[
          #link{url= <<"#user">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[
          #span{class=["icon-stack"], body=[#i{class=["icon-stack-base", "icon-circle"]},#i{class=["icon-user icon-light"]}]}, <<" User">> ]}]},
        #li{body=[#link{url= <<"#reviewer">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[
          #span{class=["icon-stack"], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-list-alt icon-light"]}]}, <<" Reviewer">>]}]},
        #li{class=[active], body=[#link{url= <<"#developer">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[
          #span{class=["icon-stack"], body=[#i{class=["icon-circle", "icon-stack-base"]},#i{class=["icon-wrench icon-light"]}]}, <<" Developer">>]}]},
        #li{style="display:none", body=[#link{url= <<"#product_feed">>, data_fields=[{<<"data-toggle">>, <<"tab">>}]}]}
      ]},

      #panel{class=[span4], body=[#panel{class=["btn-toolbar", "pull-right", "control-buttons"], body=[C || C <- Controls]} ]} ]} ]}.

account_games(Type)-> [
  #h3{class=["blue"], body=[case Type of user -> []; developer  -> <<"My Games">>; reviewer -> <<"My Reviews">> end ]},
  [game_article(#game{}, 1, controls(Type), article_meta(Type, #game{})) || _ <- lists:seq(1,3)]].

game_article(Game, _Rating, Controls, Meta)->
  #panel{class=["game-article", "shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span2, "article-meta"], body=Meta},

      #panel{class=[span3, shadow], body=[
        #image{class=["border"], alt= <<"Row Four Image">>, image=Game#game.image}
      ]},
      #panel{class=[span5, "article-text"], body=[
        #p{body=[Game#game.description, #link{url= <<"#">>, body= <<"Read">>}]}
      ]},
      #panel{class=[span2, "dev-controls"], body=Controls}
    ]}
  ]}.

controls(user)-> [
  #link{url= <<"#">>, class=[btn, "btn-block", "btn-grey", capital], body=[#i{class=["icon-download-alt", "icon-white"]}, <<"Download">>]},
  #p{class=["download-info"], body=[#span{class=["blue"], body= <<"3.7">>}, <<"GB">>]} ];
controls(reviewer) -> [
  #link{url= <<"#">>, class=[btn, "btn-grey", "btn-block", capital], body=[#i{class=["icon-pencil", "icon-white"]}, <<"Edit">>]},
  #link{url= <<"#">>, class=[btn, "btn-rust", "btn-block", capital], body=[#i{class=["icon-remove", "icon-white"]}, <<"Delete">>]} ];
controls(developer)-> [
  #price{value= <<"49.95">>, span=span11},
  #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[
    #span{class=["divider"], body=[#i{class=["icon-folder-open", "icon-white"]}, <<"3.7 GB">>]},
    #i{class=["icon-plus", "icon-white"]}, <<"Reupload File">>  ]}].

article_meta(user, Game)-> 
  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.purchased_date),
  PurchaseDate = io_lib:format("~p.~p.~p", [M, D, Y]),
  [
    #p{class=["game-title"], body=Game#game.title},
    #p{class=["game-dev", blue], body=Game#game.developer},
    #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},
    #p{class=["purchase-date"], body=[<<"Purchased on ">>,PurchaseDate, <<" ">>,#i{class=["icon-list-alt"]}]}
  ];
article_meta(reviewer, Game)->
  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.submit_date),
  SubmitDate = io_lib:format("~p.~p.~p", [M, D, Y]),
  [
    #p{class=["game-title"], body=[ <<"Kingdoms of Amalur: Reckoning">>, #link{url= <<"#">>, body=[#i{class=["icon-share"]}]}]},
    #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},
    #p{class=["number-comments"], body=[#i{class=["icon-comment"]}, <<"0 comments">>]},
    #p{class=["purchase-date"], body= [<<"Submitted on ">>, SubmitDate]}
  ];
article_meta(developer, Game)-> 
  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.upload_date),
  UploadDate = io_lib:format("~p.~p.~p", [M, D, Y]),
  [
    #p{class=["game-title"], body=[ <<"Kingdoms of Amalur: Reckoning">>, #link{url= <<"#">>, body=[#i{class=["icon-share"]}]}]},
    #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},

    #p{class=["downloads-views"], body=[
      #span{body=[#i{class=["icon-download-alt", "no-margin-left"]}, #span{body= <<"2855">>}]},
      #span{body=[#i{class=["icon-eye-open"]},#span{body= <<"12536">>}]}]},

    #p{class=["purchase-date"], body= [<<"Uploaded on ">>, UploadDate]}
  ].

inputs()->[].
inputs(user)-> [];
inputs(developer) -> [
  #h3{body= <<"Add new game">>},
  #panel{class=["row-fluid"], body=[
    #panel{class=[span9], body=[
      #textbox{id=title, class=[span12], placeholder="Game title"},
      #textarea{id=brief, class=[span12], rows="5", placeholder="Brief description"},
      #panel{class=["form-inline", span4], body=[#label{body= <<"Set a price">>}, #price{span=span12}]},
      #link{id=save_prod, class=[btn, "btn-grey", capital, "pull-right"], body=[#i{class=["icon-file", "icon-light"]}, <<" Save Game">>],
        postback=save, source=[title, brief, platform, version, price, currency, category]}
    ]},
    #panel{class=[span3], body=[
      #h4{body= [#i{class=["icon-picture", "icon-light"]}, #i{class=["icon-plus", "icon-light"]}, <<"Add Picture">> ]},
      #upload{id=image, delegate=account, root=?ROOT},
      #h4{body= [#i{class=["icon-folder-open", "icon-light"]}, #i{class=["icon-plus", "icon-light"]}, <<"Upload File">>]},
      #upload{id=file, delegate=account, root=?ROOT},
      #panel{class=["control-group"], body=[
        #label{class=["control-label"],body= <<"Version">>},
        #panel{class=[controls], body=#textbox{id=version, body=[]}}
      ]},
      #panel{class=["control-group"], body=[
        #label{class=["control-label"],body= <<"Platform">>},
        #panel{class=[controls], body=#select{id=platform, body=[
          #option{value="windows", body=  <<"Windows">>},
          #option{value="wii", body= <<"Wii">>},
          #option{value="xbox", body= <<"Xbox">>}
        ]}}
      ]},
      #panel{class=["control-group"], body=[
        #label{class=["control-label"],body= <<"Category">>},
        #panel{class=[controls], body=[
          #select{id=category, body=[
            #option{value=shooter, body= <<"Shooter">>},
            #option{value=rpg, body= <<"RPG">>}
          ]}
        ]}
      ]}
      
    ]}
  ]}
 ];
inputs(reviewer)->
  #panel{class=[inputs, reviewer], body=[
    #panel{class=["inputs-inner", "row-fluid"], body=[
      #textbox{class=[span12], placeholder="Review title..."},
      #image{image= <<"/static/img/text-style-btns.png">>, alt= <<"Text Style Buttons">>},
      #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital, "pull-right"], body=[
        #i{class=["icon-picture", "icon-white"]}, #i{class=["icon-plus", "icon-white"]}, <<"Add Picture">>]},
      #textarea{class=[span12], name="", id="", cols="30", rows="10"},
      #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right"], body=[#i{class=["icon-ok", "icon-white"]}, <<"Submit Review">>]} ]} ]}.

product_feed(P=#product{}) -> [
  #h2{body=[<<"[feed] ">>,P#product.title]},
  feed_essential(),
  feed_detail(P),
  entry_form(P)
  ].

feed_essential()-> [].
feed_detail(P) -> [
  #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
    #li{class=[active], body=[#link{url= <<"#overview">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Overview">>}]},
    #li{body=[#link{url= <<"#features">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Features">>}]},
    #li{body=[#link{url= <<"#specs">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Specification">>}]},
    #li{body=[#link{url= <<"#gallery">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Gallery">>}]},
    #li{body=[#link{url= <<"#trailers">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Videos">>}]},
    #li{body=[#link{url= <<"#reviews">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Reviews">>}]},
    #li{body=[#link{url= <<"#news">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"News">>}]},
    #li{body=[#link{url= <<"#bundles">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Bundles">>}]}
  ]},
  #panel{class=["tab-content"], body=[
    #panel{id=overview, class=["tab-pane", active], body=[]},
    #panel{id=features, class=["tab-pane"], body=[]},
    #panel{id=specs, class=["tab-pane"], body=[]},
    #panel{id=gallery, class=["tab-pane"], body=[]},
    #panel{id=trailers, class=["tab-pane"], body=[]},
    #panel{id=reviews, class=["tab-pane"], body=[feed(P)]},
    #panel{id=news, class=["tab-pane"], body=[]},
    #panel{id=bundles, class=["tab-pane"], body=[]}
  ]}
].

entry_form(P) ->
%  Id = wf:temp_id(),
  [
  #h3{body="post review"},
  #panel{class=["row-fluid"], body=[
    #panel{class=[span9], body=[
      #textbox{id=entry_title, class=[span12], placeholder= <<"Title">>},
      #htmlbox{id=entry, class=[span12]}
    ]},
    #panel{class=[span3], body=[
      #select{name=type, id=type, body=[
        #option{value=overview,  body= <<"Overview">>},
        #option{value=features,  body= <<"Features">>},
        #option{value=specs,  body= <<"Specification">>},
        #option{value=gallery, body= <<"Galery">>},
        #option{value=trailers, body= <<"Videos">>},
        #option{value=reviews, body= <<"Reviews">>},
        #option{value=news, body= <<"News">>},
        #option{value=bundles, body= <<"Bundles">>}
      ]}
      %#upload{id=upload, delegate=prod, root=code:priv_dir(web)++"/static"}
    ]}
  ]},
  #panel{class=["btn-toolbar"], body=[#link{id=save, postback={post_entry, P#product.feed, P#product.id}, source=[entry, entry_title, type], class=[btn, "btn-large", "btn-success"], body= <<"Post">>}]} ].

feed(P) ->
  Entries = kvs_feed:entries(P#product.feed, undefined, 10),
  [#product_entry{entry=E} || E <- Entries].


event(init) -> [];
event({post_entry, Fid, Id}) ->
  Entry = wf:q(entry),
  Title = wf:q(entry_title),
%  Type =  wf:q(type),
  Recipients = [{Id, product}],
  SharedBy = "",
  Type = {product, wf:q(type)},
  error_logger:info_msg("Entry ~p ~p ~p", [Title, Entry, Type]),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  error_logger:info_msg("Medias to save ~p", [Medias]),
  Desc = Entry,
  Title1 = Title,
  EntryId = uuid(),
  User = wf:user(),
  From = User#user.username,
  [begin
    % Route = [feed, product, ProductId, entry, uuid(), add]
    % Message = [Product, Destinations, Desrciption, Medias]
    kvs_feed:add_entry(Fid, From, To, EntryId, Title1, Desc, Medias, Type, SharedBy)
  end || {To, _RoutingType} <- Recipients];
event(save) ->
%title, brief, platform, version, price  
  error_logger:info_msg("Save product ~p ~p ~p", [wf:q(title), wf:q(brief), wf:q(category)]),
  User = wf:user(),
  Title = wf:q(title),
  Descr = wf:q(brief),
  Version = wf:q(version),
  Price = wf:q(price),
  Currency = wf:q(currency),
  Categories = [1],%wf:q(category),
  TitlePic = wf:session(image),
  File = wf:session(file),
  Product = #product{
    creator= User#user.username,
    owner=User#user.username,
    title = Title,
    title_picture = TitlePic,
    brief = Descr,
    categories = Categories,
    price = Price,
    currency = Currency,
    version = Version,
    file = File
  },
  case kvs_products:register(Product) of
    {ok, P} -> P,
      wf:session(title_picture, undefined),
      error_logger:info_msg("?~p", [wf:render(#product_row{product=P})]),
      wf:wire(wf:f("$('#products > tbody:first').append('~s');", [binary_to_list(wf:render(#product_row{product=P})) ])),
      wf:update(product_feed, product_feed(P)),
      wf:wire("$('#ctl a[href=\"#product_feed\"]').tab('show');");
    E -> error_logger:info_msg("E: ~p", [E]), error
  end;
event({product_feed, P=#product{}})->
  wf:update(product_feed, product_feed(P)),
  wf:wire("$('#ctl a[href=\"#product_feed\"]').tab('show');");
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

control_event(Cid, {File, Data, Room}) ->
%  file:write_file(File, Data, [write, raw]),
%  wf:wire(wf:f("uploaded('~s');", [File--Base])),
%  wf:flush(ActionHolder),
  error_logger:info_msg("Control ~p File ~p", [Cid, File]),
  Base = code:priv_dir(web),
  User = wf:user(),
  case feed_attachment:process_uploaded_file(User#user.username, User#user.feed, Data, File) of
    {error, Error} -> ok;
    {ok, Att} ->
      case Att#attachment.thumb of
        undefined -> ok;
        Thumb -> ok
%          wf:insert_bottom(attachment_thumb, #image{image=Thumb, class=upload_attachment_thumb})
      end,
%      wf:update(BoxId, #span{class=view_media_other_attachment, text=OrigFile}),
%      wf:wire("upd_parent_to_float('"++ wf:to_list(BoxId) ++"');"),
      ThisMedia = create_media(Att),
      Medias = wf:session(medias),
      NewMedias = [ ThisMedia | Medias ],
      wf:session(medias, NewMedias)
  end.

create_media(Att) ->
    #media{id = Att#attachment.id,
	   title = Att#attachment.name,
	   width = 130,
	   height = 130,
	   url = Att#attachment.file,
	   type = {attachment, Att#attachment.type},
	   thumbnail_url = Att#attachment.thumb}.

render_element(#product_entry{entry=E})->
  Ms = E#entry.media,
  error_logger:info_msg("Entry: ~p", [E#entry.description]),
  {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
  Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
  Entry = #panel{class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
          #h2{body=[E#entry.title, #small{body=[<<" by ">>, #link{body=E#entry.from}, Date]}]}
    ]},
    #figure{class=["thumbnail-figure"], body=[
      [#entry_media{media=Me, fid=E#entry.entry_id} || Me <- Ms],
          #figcaption{class=["thumbnail-title"], body=[
            #h3{body=#span{body= E#entry.title}}
          ]}
    ]},
    #p{body=E#entry.description},
    #footer{class=["blog-footer", "row-fluid"], body=[
          #panel{class=[span4, "blog-categories"], body=[#i{class=["icon-pushpin"]}, #link{body= <<" consectetur">>}]},
          #panel{class=[span4, "blog-tags"], body=[#i{class=["icon-tags"]}, #link{body= <<" fugiat, nulla, pariatur">>}]},
          #panel{class=[span4, "blog-more"], body=[#i{class=["icon-link"]}, #link{body= <<" read more">>}]}
    ]}
  ]},
  element_panel:render_element(Entry);
render_element(#product_row{product=P}) ->
  Row = #tr{cells=[
    #td{body= integer_to_list(P#product.id)},
    #td{body= #link{id="prodrow"++integer_to_list(P#product.id), class=[], postback={product_feed, P}, body=P#product.title}},
    #td{body= P#product.brief}
  ]},
  element_tr:render_element(Row);
render_element(R = #price{}) ->
  C = [
    #textbox{id = price, value = R#price.value},
    #select{id=currency, class=[selectpicker, R#price.span], body=[#option{label= L, body = V} || {L,V} <- R#price.currencies]}
  ],
  wf_tags:emit_tag(<<"div">>, wf:render(C), [{<<"class">>, [<<"input-append">>, <<"price">>]}]).


uuid() ->
  R1 = random:uniform(round(math:pow(2, 48))) - 1,
  R2 = random:uniform(round(math:pow(2, 12))) - 1,
  R3 = random:uniform(round(math:pow(2, 32))) - 1,
  R4 = random:uniform(round(math:pow(2, 30))) - 1,
  R5 = erlang:phash({node(), now()}, round(math:pow(2, 32))),

  UUIDBin = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>,
  <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = UUIDBin,

  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b-~8.16.0b",
                              [TL, TM, THV, CSR, CSL, N, R5])).

