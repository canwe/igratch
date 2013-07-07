-module(account).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

-record(game, {
  title = <<"Kingdoms of Amalur: Reckoning">>,
  developer= <<"Electronic Arts">>,
  purchased_date=erlang:now(),
  image = <<"/static/img/row4.jpg">>,
  description= <<"Ken Rolston was the game's executive designer, R. A. Salvatore created the game universe and lore, with Todd McFarlane working on the artwork, and Grant Kirkhope creating the musical score. It was developed by 38 Studios and Big Huge Games. The game was released on February 7, 2012, in North America and on February 9, 2012, in Europe...">>
}).

main()-> #dtl{file="dev", bindings=[{title,<<"notifications">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container", "main-no-slider", account], body=[
    #panel{id=first, body=[
      #panel{class=["row-fluid"], body=[
        #h3{class=[blue], body= <<"My Account">>},
        control_panel([
          {<<"User">>, "icon-user", active},
          {<<"Reviewer">>, "icon-list-alt", inactive},
          {<<"Developer">>, "icon-wrench", inactive}
        ],[
            #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-pencil"]}, <<" Edit Profile">>]},
            #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-share"]}, <<" Upgrade">>]}
        ])
      ]},
      #panel{id="account-games", class=["row-fluid"], body=[game_article(#game{}, 1) || _ <- lists:seq(1,3)]}
    ]}
  ]} ] ++ index:footer().

control_panel(Items, Controls)->
  #panel{class=["control-panel", clearfix], body=[
    #list{class=[inline, unstyled, "control-roles", span8], body=[
      #li{class=[Class], body=[
        #span{class=["icon-stack"], body=[
          #i{class=["icon-circle", "icon-stack-base", "blue"]},
          #i{class=[Icon]}
        ]},
        #span{body=Text} ]} || {Text, Icon, Class} <-Items]},
    #panel{class=[span4], body=[#panel{class=["btn-toolbar", "pull-right", "control-buttons"], body=[C || C <- Controls]} ]}
  ]}.

game_article(Game, _Rating)->
  {{Y, M, D}, _} = calendar:now_to_datetime(Game#game.purchased_date),
  PurchaseDate = io_lib:format("~p.~p.~p", [M, D, Y]),

  #panel{class=["game-article", "shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span2, "article-meta"], body=[
        #p{class=["game-title"], body=Game#game.title},
        #p{class=["game-dev", blue], body=Game#game.developer},
        #span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]},
        #p{class=["purchase-date"], body=[<<"Purchased on ">>,PurchaseDate, <<" ">>,#i{class=["icon-list-alt"]}]}
      ]},
      #panel{class=[span3, shadow], body=[
        #image{class=["border"], alt= <<"Row Four Image">>, image=Game#game.image}
      ]},
      #panel{class=[span5, "article-text"], body=[
        #p{body=[Game#game.description, #link{url= <<"#">>, body= <<"Read">>}]}
      ]},
      #panel{class=[span2], body=[
        #link{url= <<"#">>, class=[btn, "btn-block", "btn-grey", capital], body=[#i{class=["icon-download-alt", "icon-white"]}, <<" Download">>]},
        #p{class=["download-info"], body=[#span{class=["blue"], body= <<"3.7">>}, <<"GB">>]}
      ]}
    ]}
  ]}.

event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

