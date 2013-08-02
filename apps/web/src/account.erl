-module(account).
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").

-record(struct, {lst=[]}).

-define(SYSTEM_MESSAGE_EXPIRES, 600).
-define(SYSTEM_MESSAGE_STAYS_FOR_READING, 20).
-define(LIKERS_TO_SHOW, 5).

-record(price, {?ELEMENT_BASE(account), value, currencies=[{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}], span}).

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
        #h3{body= <<"My products">>, class=[blue]},
        #table{id=products, class=[table, "table-hover"], body=[[#product_row{product=P} || P <- kvs:all(product)]] }
%        #panel{id=my_products, class=["row-fluid"], body=[#product_line{product=P, meta=article_meta(P), controls=[]} || P <-kvs:all(product)]}
      ]}
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

%account_games(Type)-> [
%  #h3{class=["blue"], body=[case Type of user -> []; developer  -> <<"My Games">>; reviewer -> <<"My Reviews">> end ]},
%  [game_article(#game{}, 1, controls(Type), article_meta(Type, #game{})) || _ <- lists:seq(1,3)]].

%game_article(Game, _Rating, Controls, Meta)->
%  #panel{class=["game-article", "shadow-fix"], body=[
%    #panel{class=["game-article-inner", clearfix], body=[
%      #panel{class=[span2, "article-meta"], body=Meta},
%
%      #panel{class=[span3, shadow], body=[
%        #image{class=["border"], alt= <<"Row Four Image">>, image=Game#game.image}
%      ]},
%      #panel{class=[span5, "article-text"], body=[
%        #p{body=[Game#game.description, #link{url= <<"#">>, body= <<"Read">>}]}
%      ]},
%      #panel{class=[span2, "dev-controls"], body=Controls}
%    ]}
%  ]}.

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

%article_meta(user, Game)-> 
%  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.purchased_date),
%  PurchaseDate = io_lib:format("~p.~p.~p", [M, D, Y]),
%  [
%    #p{class=["game-title"], body=Game#game.title},
%    #p{class=["game-dev", blue], body=Game#game.developer},
%    #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},
%    #p{class=["purchase-date"], body=[<<"Purchased on ">>,PurchaseDate, <<" ">>,#i{class=["icon-list-alt"]}]}
%  ];
%article_meta(reviewer, Game)->
%  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.submit_date),
%  SubmitDate = io_lib:format("~p.~p.~p", [M, D, Y]),
%  [
%    #p{class=["game-title"], body=[ <<"Kingdoms of Amalur: Reckoning">>, #link{url= <<"#">>, body=[#i{class=["icon-share"]}]}]},
%    #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},
%    #p{class=["number-comments"], body=[#i{class=["icon-comment"]}, <<"0 comments">>]},
%    #p{class=["purchase-date"], body= [<<"Submitted on ">>, SubmitDate]}
%  ];
article_meta(#product{} = P)-> 
  {{Y, M, D}, _} = calendar:now_to_datetime(P#product.creation_date),
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
inputs(developer) ->
  User = wf:user(),
  Curs = [{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}],
  case User of undefined ->[]; _-> [
  #panel{class=["row-fluid"], body=[
    #panel{class=[span10, "pull-left"], body=[
      #h3{body= <<"Add new game">>},
      #textbox{id=title, class=[span12], placeholder="Game title"},
      #textarea{id=brief, class=[span12], rows="5", placeholder="Brief description"},

      #panel{class=["input-append"], body=[
        #textbox{id = price, class=[span2]},
        #select{id=currency, class=[selectpicker], body=[#option{label= L, body = V} || {L,V} <- Curs]}
      ]},

      #link{id=save_prod, class=[btn, "btn-grey", capital, "pull-right"], body=[#i{class=["icon-file"]}, <<" Save Game">>],
        postback=save, source=[title, brief, price, currency]}
    ]},
    #panel{class=[span2], body=[
      #h3{body= <<"cover">>},
      #upload{root=?ROOT++"/"++User#user.email, post_write=attach_cover, img_tool=gm, preview=true}
    ]}
  ]}
 ] end;
inputs(reviewer)->
  #panel{class=[inputs, reviewer], body=[
    #panel{class=["inputs-inner", "row-fluid"], body=[
      #textbox{class=[span12], placeholder="Review title..."},
      #image{image= <<"/static/img/text-style-btns.png">>, alt= <<"Text Style Buttons">>},
      #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital, "pull-right"], body=[
        #i{class=["icon-picture", "icon-white"]}, #i{class=["icon-plus", "icon-white"]}, <<"Add Picture">>]},
      #textarea{class=[span12], name="", id="", cols="30", rows="10"},
      #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right"], body=[#i{class=["icon-ok", "icon-white"]}, <<"Submit Review">>]} ]} ]}.

feed(Fid) ->
  Entries = kvs_feed:entries(Fid, undefined, 10),
  [#product_entry{entry=E} || E <- Entries].


event(init) -> [];
event(save) ->
  error_logger:info_msg("Save product ~p ~p", [wf:q(title), wf:q(brief)]),
  User = wf:user(),
  Title = wf:q(title),
  Descr = wf:q(brief),
  {Price, _Rest} = string:to_float(wf:q(price)),
  Currency = wf:q(currency),
  Categories = [1],%wf:q(category),
  TitlePic = wf:session(cover),
  Product = #product{
    creator= User#user.username,
    owner=User#user.username,
    title = list_to_binary(Title),
    cover = TitlePic,
    brief = list_to_binary(Descr),
    categories = Categories,
    price = Price,
    currency = Currency
  },
  case kvs_products:register(Product) of
    {ok, P} ->
      msg:notify([product, init], [P#product.id, P#product.feed, P#product.blog, P#product.features, P#product.specs, P#product.gallery, P#product.videos, P#product.bundles]),
      wf:session(cover, undefined),
      error_logger:info_msg("?~p", [wf:render(#product_row{product=P})]),
      wf:wire(wf:f("$('#products > tbody:first').append('~s');", [wf:js_escape(binary_to_list(wf:render(#product_row{product=P}))) ]));
    E -> error_logger:info_msg("E: ~p", [E]), error
  end;
event({product_feed, Id})-> wf:redirect("/product?id="++integer_to_list(Id));
event(Event) -> error_logger:info_msg("[account]Page event: ~p", [Event]), ok.

api_event(attach_cover, Tag, _)->
  Args = n2o_json:decode(Tag),
  Props = Args#struct.lst,
  wf:session(cover, "/static"++ [binary_to_list(proplists:get_value(<<"file">>, Props))--?ROOT]),
  error_logger:info_msg("[ext] Here we go. Attach cover! ~p ", [Props]);
api_event(attach_media, Tag, _Term) ->
  Args = n2o_json:decode(Tag),
  Props = Args#struct.lst,
  error_logger:info_msg("[ext] Here we go. Attach media! ~p ", [Props]),

  Id = proplists:get_value(<<"id">>, Props),
  File = proplists:get_value(<<"file">>, Props),
  Name = filename:basename(File),
  Type = proplists:get_value(<<"type">>, Props),
  Thumb = proplists:get_value(<<"thumb">>, Props),
  %User = wf:user(),

%  {{Y, M, D}, _} = calendar:local_time(),
%  Date = integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D),

  ThisMedia = #media{id = Id,
    title = Name,
    width = 130,
    height = 130,
    url = "/static"++binary_to_list(File)--?ROOT,
    type = {attachment, Type},
    thumbnail_url = "/static"++binary_to_list(Thumb)--?ROOT},
  error_logger:info_msg("MEDIAS? ~p", [wf:session(medias)]),
  Medias = wf:session(medias),
%  Medias = case State of undefined -> []; [] -> [];  M -> M end,
  NewMedias = [ ThisMedia | Medias ],
  error_logger:info_msg("new medias: ~p", [NewMedias]),
  wf:session(medias, NewMedias);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

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

render_element(R = #price{}) ->
  C = [
    #textbox{id = price, value = R#price.value},
    #select{id=currency, class=[selectpicker, R#price.span], body=[#option{label= L, body = V} || {L,V} <- R#price.currencies]}
  ],
  wf_tags:emit_tag(<<"div">>, wf:render(C), [{<<"class">>, [<<"input-append">>, <<"price">>]}]).
