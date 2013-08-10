-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"Store">>},{body, body()}]}.

body()->
  case wf:qs(<<"id">>) of undefined ->skip; I -> error_logger:info_msg("~p RECEIVED!", [I]),wf:wire(wf:f("$('a[href=\"#~s\"]').addClass('text-warning').tab('show');", [binary_to_list(I)])) end,
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){$(e.target).addClass('text-warning').siblings().removeClass('text-warning');});"),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [#link{url="#all", body= <<"Categories ">>, style="color: black", data_fields=[{<<"data-toggle">>, <<"tab">>}]}, #small{body=[
          begin
          [
            <<" / ">>,
            #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}
          ] end || #group{id=Id, name=Name} <- kvs:all(group)]} ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body=[
            [[#product_entry{entry=E, mode=line, category=Name} || E <- kvs_feed:entries(lists:keyfind(products,1,Feeds), undefined, ?PAGE_SIZE)] || #group{feeds=Feeds, name=Name} <- kvs:all(group)]
          ]},
          [ begin
              Fid = lists:keyfind(products,1,Feeds),
              Entries = kvs_feed:entries(Fid, undefined, ?PAGE_SIZE),
              Last = case Entries of []-> []; E-> lists:last(E) end,
              EsId = wf:temp_id(),
              BtnId = wf:temp_id(),
              Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId, category=Name},
              NoMore = length(Entries) < ?PAGE_SIZE,
              #panel{id=Id, class=["tab-pane"], body=[
                #panel{id=EsId, body=[#product_entry{entry=E, mode=line, category=Name} || E <- Entries]},
                #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
                  if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Last, Info}} end
                ]}
              ]}
            end ||#group{id=Id, name=Name, feeds=Feeds} <- kvs:all(group)]]},
        #panel{class=[span3], body=[<<"">>]} ]}
    ]}
  ]},
  #section{class=[section, alt], body=#panel{class=[container], body=[
      #panel{class=["hero-unit", "text-center"], body=[
        #h1{body= <<"Got a question?">>},
        #p{body= <<"want to work with us to move your bussines to the next level? Well, dont be afraid">>},
        #link{class=[btn, "btn-large", "btn-info"], body= <<"contact us">>}
      ]}
  ]}}
  ] ++ index:footer().


body1()-> index:header() ++ [
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
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, {Id,_}})-> wf:redirect("/product?id="++Id);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

process_delivery([show_entry], M) -> product:process_delivery([show_entry], M);
process_delivery([no_more], M) -> product:process_delivery([no_more], M);
process_delivery(_,_) -> skip.

