-module(account_dev).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="dev", bindings=[{title,<<"account developer">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container-fluid", "main-no-slider", account], body=[
    #panel{id=first, body=[
      #panel{class=["row-fluid"], body=[
        #h3{class=["blue"], body= <<"My Account">>},
        #panel{id="account-panel", class=["img-rounded", clearfix], body=[
          #list{id="account-roles", class=["span6"], body=[
            #li{id="role-user", class=[active, "no-margin-left"], body=[
              #span{class=["i-base"], body=[#i{class=["icon-user", "icon-white"]}]},
              #span{class=["role"], body= <<"User">>} ]},
            #li{id="role-reviewer", class=["inactive"], body=[
              #span{class=["i-base"], body=[#i{class=["icon-list-alt", "icon-white"]}]},
              #span{class=["role"], body= <<"Reviewer">>} ]},
            #li{id="role-developer", class=["active"], body=[
              #span{class=["i-base"], body=[#i{class=["icon-wrench", "icon-white"]} ]},
              #span{class=["role"], body= <<"Developer">>} ]}
          ]},
          #panel{id="account-buttons", class=["pull-right"], body=[
            #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-pencil", "icon-white"]}, <<"Edit Profile">>]},
            #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-share", "icon-white"]}, <<"Upgrade">>]}
          ]}
        ]}
      ]},
      #panel{id="inputs", class=[developer, "row-fluid"], body=[
        #panel{id="inputs-inner", class=[developer, clearfix], body=[
          #panel{class=[span10], body=[
            #textbox{placeholder="New game title..."},
            #textarea{cols="30", rows="10"},
            #panel{class=["input-append"], body=[
              #label{body= <<"Set a price">>},
              #textbox{id="price"},
              #panel{class=["btn-group"], body=[
                #button{class=[btn, "dropdown-toggle"], body=[#span{class=["toggle-text", currency], body= <<"Dollar">>}, #span{class=[caret]}]},
                #list{class=["dropdown-menu"], body=[
                  #li{body=[#link{url="javascript:void()", body= <<"Dollar">>}]},
                  #li{body=[#link{url="javascript:void()", body= <<"Euro">>}]},
                  #li{body=[#link{url="javascript:void()", body= <<"Frank">>}]}
                ]}
              ]}
            ]},
            #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right", "save-game", "save-one"], body=[#i{class=["icon-file", "icon-white"]}, <<"Save Game">>]}
          ]},
          #panel{class=["span2"], body=[
            #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[#i{class=["icon-picture", "icon-white"]}, #i{class=["icon-plus", "icon-white"]}, <<"Add Picture">>]},
            #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[#i{class=["icon-folder-open", "icon-white"]}, #i{class=["icon-plus", "icon-white"]}, <<"Upload File">>]},
            #panel{class=["row-fluid"], body=[
              #label{body= <<"Version">>},
              #textbox{id="version"}
            ]},
            #dropdown{name="platform", id="platform", options=[
              #option{value="wind", body=  <<"Windows">>},
              #option{value="wii", body= <<"Wii">>},
              #option{value="xbox", body= <<"Xbox">>}
            ]},
            #link{url= <<"#">>, class=[btn, "btn-grey", capital, "pull-right", "save-game", "save-two"], body=[#i{class=["icon-file", "icon-white"]}, <<"Save Game">>]}
          ]}
        ]},
        #panel{id="account-games", class=["row-fluid", developer], body=[
          #h3{class=["blue"], body= <<"My Games">>},
          article(),
          article()
        ]}
      ]}
    ]}
  ]} ] ++ index:footer().

article()->
  #panel{class=["game-article", "shadow-fix"], body=[
    #panel{class=["game-article-inner",clearfix], body=[
      #panel{class=[span2,"article-meta"], body=[
        #p{class=["game-title"], body=[ <<"Kingdoms of Amalur: Reckoning">>, #link{url= <<"#">>, body=[#i{class=["icon-share"]}]}]},
        #image{class=["game-rating"], alt= <<"Game Rating">>, image= <<"/static/img/rating.png">>},
        #p{class=["number-comments"], body=[#i{class=["icon-comment"]}, <<"0 comments">>]},
        #p{class=["submit-date"], body= <<"Submitted on 02.05.2013">>}
      ]},
      #panel{class=[span3, shadow], body=[
        #image{class=["border"], alt= <<"Row Four Image">>, image= <<"/static/img/row4.jpg">>}
      ]},
      #panel{class=[span5, "article-text"], body=[
        #p{body=[ <<"Ken Rolston was the game's executive designer, R. A. Salvatore created the game universe and lore, with Todd McFarlane working on the artwork, and Grant Kirkhope creating the musical score. It was developed by 38 Studios and Big Huge Games. The game was released on February 7, 2012, in North America and on February 9, 2012, in Europe...">>,
          #link{url= <<"#">>, class=[edit], body= <<"edit">>}
        ]}
      ]},
      #panel{class=["span2"], body=[
        #panel{class=["input-append"], body=[
          #textbox{body= <<"45.99">>},
          #panel{class=["btn-group"], body=[
            #button{class=["btn dropdown-toggle"], body=[#span{class=["toggle-text currency"], body= <<"Dollar">>}, #span{class=["caret"]}]},
            #list{class=["dropdown-menu"], body=[
              #li{body=[#link{url="javascript:void()", body= <<"Dollar">>}]},
              #li{body=[#link{url="javascript:void()", body= <<"Euro">>}]},
              #li{body=[#link{url="javascript:void()", body= <<"Frank">>}]}
            ]}
          ]}
        ]},
        #link{url= <<"#">>, class=[btn, "btn-blue", "btn-border", capital], body=[
          #span{body=[#i{class=["icon-folder-open", "icon-white"]}, <<"3.7 GB">>]},
          #i{class=["icon-plus", "icon-white"]}, <<"Reupload File">> 
        ]}
      ]}
    ]}
  ]}.


event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

