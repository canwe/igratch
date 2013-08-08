-module(account).
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").

-record(price, {?ELEMENT_BASE(account), value, currencies=[{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}], span}).

main()-> #dtl{file="prod", bindings=[{title,<<"account">>},{body, body()}]}.

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
        postback=save, source=[title, brief, price, currency, cats]}
    ]},
    #panel{class=[span2], body=[
      #h3{body= <<"cover">>},
      #upload{preview=true, root=?ROOT, dir="static/"++User#user.email, post_write=attach_media, img_tool=gm, size=[{270, 124}, {200, 200}, {139, 80}]},
      #h3{body= <<"categories">>},
      #textboxlist{id=cats}
    ]}
  ]}
 ] end;
inputs(reviewer)->
  User = wf:user(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
  MsId = wf:temp_id(),
  case User of undefined -> []; _->
  #panel{class=["row-fluid"], body=[
      #panel{class=[span10], body=[
        #h3{body= <<"Submit review">>},
        #textbox{class=[span12], placeholder= <<"Title">>},
        #htmlbox{class=[span12], root=?ROOT, dir="static/"++User#user.email, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200} , {139, 80}]},
        #panel{class=["btn-toolbar"], body=[#link{class=[btn, capital, "btn-gray", "pull-right"], body=[#i{class=["icon-ok", "icon-white"]}, <<" Post">>]}]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias)}
      ]},
      #panel{class=[span2], body=[]}
  ]} end.

feed(Fid) ->
  Entries = kvs_feed:entries(Fid, undefined, 10),
  [#product_entry{entry=E} || E <- Entries].

control_event("cats", _) ->
  SearchTerm = wf:q(term),
  Data = [ [list_to_binary(Id++"="++Name), list_to_binary(Name)] || #group{id=Id, name=Name} <- kvs:all(group), string:str(string:to_lower(Name), string:to_lower(SearchTerm)) > 0],
  element_textboxlist:process_autocomplete("cats", Data, SearchTerm);
control_event(_, _) -> ok.


event(init) -> wf:reg(product_channel), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(save) ->
  User = wf:user(),
  Title = wf:q(title),
  Descr = wf:q(brief),
  Cats = wf:q(cats),
  {Price, _Rest} = string:to_float(wf:q(price)),
  Currency = wf:q(currency),
  Categories = [1],
  TitlePic = case wf:session(medias) of undefined -> undefined; []-> undefined; Ms -> (lists:nth(1,Ms))#media.url--?ROOT end,
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
      msg:notify([kvs_products, product, init], [P#product.id, P#product.feed, P#product.blog, P#product.features, P#product.specs, P#product.gallery, P#product.videos, P#product.bundles]),
      wf:session(medias, []),
      [kvs_group:join(P#product.name, G) || G <- string:tokens(Cats, ",")],
      wf:wire(wf:f("$('#products > tbody:first').append('~s');", [wf:js_escape(binary_to_list(wf:render(#product_row{product=P}))) ]));
    E -> error_logger:info_msg("E: ~p", [E]), error
  end;
event({product_feed, Id})-> wf:redirect("/product?id="++integer_to_list(Id));
event(<<"PING">>) -> ok;
event(Event) -> error_logger:info_msg("[account]Page event: ~p", [Event]), ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery(_R, _M) -> skip.

render_element(R = #price{}) ->
  C = [
    #textbox{id = price, value = R#price.value},
    #select{id=currency, class=[selectpicker, R#price.span], body=[#option{label= L, body = V} || {L,V} <- R#price.currencies]}
  ],
  wf_tags:emit_tag(<<"div">>, wf:render(C), [{<<"class">>, [<<"input-append">>, <<"price">>]}]).
