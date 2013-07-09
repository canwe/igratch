-module(account).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").

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

main()-> wf:user(#user{type=user}),#dtl{file="dev", bindings=[{title,<<"account">>},{body, body()}]}.

body()->
  #user{type=Type} = wf:user(),
  index:header() ++ [
  #panel{id="main-container", class=["container", "main-no-slider", account], body=#panel{body=[
    profile_ctl(Type),
    inputs(Type),
    #panel{id="account-games", class=["row-fluid"], body=account_games(Type)}
  ]}} ] ++ index:footer().

profile_ctl(Type) ->
  Items = [{<<"User">>, "icon-user", if Type == user -> active; true -> inactive end},
    {<<"Reviewer">>, "icon-list-alt", if Type == reviewer -> active; true-> inactive end},
    {<<"Developer">>, "icon-wrench", if Type == developer -> active; true-> inactive end} ],
  Controls = [
    #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-pencil"]}, <<" Edit Profile">>]},
    #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-share"]}, <<" Upgrade">>]}],

  #panel{class=["row-fluid"], body=[
    #h3{class=[blue], body= <<"My Account">>},
    #panel{class=["control-panel", clearfix], body=[
      #list{class=[inline, unstyled, "control-roles", span8], body=[
        #li{class=[Class], body=[
          #span{class=["icon-stack"], body=[
            #i{class=["icon-circle", "icon-stack-base", "blue"]},
            #i{class=[Icon]}]},
          #span{body=Text} ]} || {Text, Icon, Class} <-Items]},
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

inputs(user)-> [];
inputs(developer) ->
  #panel{class=[inputs, developer], body=[
    #panel{class=["inputs-inner", "row-fluid"], body=[
      #panel{class=[span10], body=[
        #textbox{class=[span12], placeholder="New game title..."},
        #textarea{class=[span12], cols="30", rows="10"},
        #panel{class=["form-inline", span4], body=[#label{body= <<"Set a price">>}, #price{span=span12}]},
        #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right", "save-game", "save-one"], body=[#i{class=["icon-file", "icon-white"]}, <<"Save Game">>]}
      ]},
      #panel{class=[span2, "dev-controls"], body=[
        #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[#span{class=["divider"], body=[#i{class=["icon-picture", "icon-white"]}]}, #i{class=["icon-plus", "icon-white"]}, <<"Add Picture">>]},
        #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[#span{class=["divider"], body=[#i{class=["icon-folder-open", "icon-white"]}]}, #i{class=["icon-plus", "icon-white"]}, <<"Upload File">>]},
        #panel{class=["control-group"], body=[
          #label{class=["control-label"],body= <<"Version">>},
          #panel{class=[controls], body=#textbox{id="version"}}
        ]},
        #select{name="platform", id="platform", body=[
          #option{value="windows", body=  <<"Windows">>},
          #option{value="wii", body= <<"Wii">>},
          #option{value="xbox", body= <<"Xbox">>} ]} ]} ]} ]};
inputs(reviewer)->
  #panel{class=[inputs, reviewer], body=[
    #panel{class=["inputs-inner", "row-fluid"], body=[
      #textbox{class=[span12], placeholder="Review title..."},
      #image{image= <<"/static/img/text-style-btns.png">>, alt= <<"Text Style Buttons">>},
      #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital, "pull-right"], body=[
        #i{class=["icon-picture", "icon-white"]}, #i{class=["icon-plus", "icon-white"]}, <<"Add Picture">>]},
      #textarea{class=[span12], name="", id="", cols="30", rows="10"},
      #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right"], body=[#i{class=["icon-ok", "icon-white"]}, <<"Submit Review">>]} ]} ]}.


event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

render_element(R = #price{}) ->
  C = [
    #textbox{value = R#price.value},
    #select{id=currency, class=[selectpicker, R#price.span], body=[#option{label= L, body = V} || {L,V} <- R#price.currencies]}
  ],
  wf_tags:emit_tag(<<"div">>, wf:render(C), [{<<"class">>, [<<"input-append">>, <<"price">>]}]).
