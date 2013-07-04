-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="dev", bindings=[{title,<<"Store">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container-fluid", "main-no-slider"], body=[
    #panel{id=first, body=[
      #panel{class=["row-fluid", "store-item"], body=[
        #panel{class=["store-item-inner", clearfix], body=[
          #panel{class=[breadcrumbs], body=[
            #h3{class=[capital, blue], body=[<<"Action">>, #span{class=["title"], body= <<"Crysis 3">>} ]}
          ]},
          #panel{class=["no-margin-left","big-shadow"], body=[
            #image{class=[border], alt= <<"Store Image">>, image= <<"/static/img/store-img1.jpg">>}
          ]},
          #panel{class=[span10, offset1], body=[
            #panel{class=["game-info"], body=[
              #p{body=[
                <<"Developer: ">>,
                #span{class=["blue"], body= <<"Electronic Arts">>},
                #image{class=["pull-right"], alt= <<"Rating">>, image= <<"/static/img/rating.png">>}
              ]},
              #p{class=["store-item-text"], body= <<"The award-winning developer Crytek is back with Crysis 3, the first blockbuster shooter of 2013! Return to the fight as Prophet, the Nanosuit soldier on a quest to rediscover his humanity and exact brutal revenge. Adapt on the fly with the stealth and armor abilities of your unique Nanosuit as you battle through the seven wonders of New York’s Liberty Dome. Unleash the firepower of your all-new, high-tech bow and alien weaponry to hunt both human and alien enemies. And uncover the truth behind the death of your squad while reestablishing the power of human will in a rich story full of exciting twists and turns. Crysis 3 is the ultimate sandbox shooter, realised in the stunning visuals only Crytek and the latest version of CryENGINE can deliver. Assess Adapt, and Attack starting spring 2013 on Xbox 360, PlayStation 3, and PC.">>}
            ]},
            #panel{class=["buy-review", span3, offset5, "center-wider"], body=[
              #link{url= <<"#">>, class=[btn, "btn-orange capital"], body= <<"Buy it!">>},
              #link{url= <<"review.html">>, class=[btn, "btn-blue", capital], body= <<"Review">>}
            ]}
          ]}
        ]},
        #panel{class=["row-fluid", "store-item"], body=[
          #panel{class=["store-item-inner", clearfix], body=[
            #panel{class=[breadcrumbs], body=[
              #h3{class=[capital, blue], body=[<<"Action">>, #span{class=["title"], body= <<"Battlefield 4">>}]}
            ]},
            #panel{class=["no-margin-left","big-shadow"], body=[
              #image{class=[border], alt= <<"Store Image">>, image= <<"/static/img/store-img2.jpg">>}
            ]},
            #panel{class=[span10, offset1], body=[
              #panel{class=["game-info"], body=[
                #p{body=[<<"Developer: ">>, #span{class=[blue], body= <<"Electronic Arts">>}]},
                #image{class=["pull-right"], alt= <<"Rating">>, image= <<"/static/img/rating.png">>}
              ]},
              #p{class=["store-item-text"], body= <<"The award-winning developer Crytek is back with Crysis 3, the first blockbuster shooter of 2013! Return to the fight as Prophet, the Nanosuit soldier on a quest to rediscover his humanity and exact brutal revenge. Adapt on the fly with the stealth and armor abilities of your unique Nanosuit as you battle through the seven wonders of New York’s Liberty Dome. Unleash the firepower of your all-new, high-tech bow and alien weaponry to hunt both human and alien enemies. And uncover the truth behind the death of your squad while reestablishing the power of human will in a rich story full of exciting twists and turns. Crysis 3 is the ultimate sandbox shooter, realised in the stunning visuals only Crytek and the latest version of CryENGINE can deliver. Assess Adapt, and Attack starting spring 2013 on Xbox 360, PlayStation 3, and PC.">>}
            ]},
            #panel{class=["buy-review", span3, offset5, "center-wider"], body=[
              #link{url= <<"#">>, class=[btn, "btn-orange", capital], body= <<"Buy it!">>},
              #link{url= <<"review.html">>, class=[btn, "btn-blue", capital], body= <<"Review">>}
            ]}
          ]}
        ]}
      ]}
    ]}
  ]} ] ++ index:footer().

event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.
