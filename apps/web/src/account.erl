-module(account).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="dev", bindings=[{title,<<"notifications">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container-fluid main-no-slider account"], body=[
    #panel{id=first, body=[
      #panel{class=["row-fluid"], body=[
        #h3{class=[blue], body= <<"My Account">>},
        #panel{id="account-panel", class=["img-rounded", clearfix], body=[
          #list{id="account-roles", class=["span6"], body=[
            #li{id="role-user", class=[active, "no-margin-left"], body=[
              #span{class=["i-base"], body=[
                #i{class=["icon-user icon-white"]}
              ]},
              #span{class=["role"], body=  <<"User">>}
            ]},
            #li{id="role-reviewer", class=["active"], body=[
              #span{class=["i-base"], body=[
                #i{class=["icon-list-alt icon-white"]}
              ]},
              #span{class=["role"], body= <<"Reviewer">>}
            ]},
            #li{id="role-developer", class=["inactive"], body=[
              #span{class=["i-base"], body=[
                #i{class=["icon-wrench icon-white"]}
              ]},
              #span{class=["role"], body= <<"Developer">>}
            ]}
          ]},
          #panel{id="account-buttons", class=["pull-right"], body=[
            #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-pencil icon-white"]}, <<"Edit Profile">>]},
            #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-share icon-white"]}, <<"Upgrade">>]}
          ]}
        ]}
      ]},
      #panel{id="account-games", class=["row-fluid"], body=[
        article(),
        article(),
        article()
      ]}
    ]}
  ]} ] ++ index:footer().

article()->
  #panel{class=["game-article", "shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span2, "article-meta"], body=[
        #p{class=["game-title"], body= <<"Kingdoms of Amalur: Reckoning">>},
        #p{class=["game-dev", blue], body= <<"Electronic Arts">>},
        #image{class=["game-rating"], alt= <<"Game Rating">>, image= <<"/static/img/rating.png">>},
        #p{class=["purchase-date"], body=[<<"Purchased on 02.05.2013">>, #i{class=["icon-list-alt"]}]}
      ]},
      #panel{class=[span3, shadow], body=[
        #image{class=["border"], alt= <<"Row Four Image">>, image= <<"/static/img/row4.jpg">>}
      ]},
      #panel{class=[span5, "article-text"], body=[
        #p{body=[ <<"Ken Rolston was the game's executive designer, R. A. Salvatore created the game universe and lore, with Todd McFarlane working on the artwork, and Grant Kirkhope creating the musical score. It was developed by 38 Studios and Big Huge Games. The game was released on February 7, 2012, in North America and on February 9, 2012, in Europe...">>, 
          #link{url= <<"#">>, body= <<"Read">>}
        ]}
      ]},
      #panel{class=[span2], body=[
        #link{url= <<"#">>, class=[btn, "btn-grey", capital], body=[#i{class=["icon-download-alt", "icon-white"]}, <<"Download">>]},
        #p{class=["download-info"], body=[#span{class=["blue"], body= <<"3.7">>}, <<"GB">>]}
      ]}
    ]}
  ]}.


event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

