-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

-include("records.hrl").

-define(PAGE_SIZE, case wf:session(page_size) of list -> 2; _ -> 8 end).

main() -> wf:session(page_size, list), #dtl{file="prod", bindings=[{title,<<"Store">>},{body, body()}]}.

body() -> index:header() ++[
  #section{class=[section, "main-no-slider"], body=[
    #panel{class=[container], body=[
      #panel{class=["row-fluid"], body=[
        #table{id=products, class=[table, "table-hover"], body=[list_products(1)] }
      ]},
      #panel{class=[pagination, "pagination-large","pagination-centered"],body=[ #list{id=pagination, body=pagination(1)} ]}
    ]}
  ]},
  #section{class=[section, alt], body=#panel{class=[container], body=[
      #panel{class=["hero-unit", "text-center"], body=[
        #h1{body= <<"Got a question?">>},
        #p{body= <<"want to work with us to move your bussines to the next level? Well, dont be afraid">>},
        #link{class=[btn, "btn-large", "btn-info"], body= <<"contact us">>}
      ]}
  ]}}
  ] ++index:footer().

list_products(Page) -> [#product_row{product=P} || P <- lists:sublist(kvs:all(product), (Page-1) * ?PAGE_SIZE + 1, ?PAGE_SIZE)].

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

pagination(Page)->
  PageCount = (length(kvs:all(product))-1) div ?PAGE_SIZE + 1,
  error_logger:info_msg("Page: ~p", [PageCount]),
  [
  #li{class=[if Page==1-> "disabled"; true->[] end, "previous"], body=#link{body=#i{class=["icon-circle-arrow-left", "icon-large"]}, postback={page, 1} }},
  [#li{class=if I==Page -> active;true->[] end,body=#link{id="pglink"++integer_to_list(I),body=#span{style="line-height:normal;", body=integer_to_list(I)}, postback={page, I} }} 
    || I <- lists:seq(1, PageCount)],
  #li{class=[if PageCount==Page -> "disabled";true->[] end,"next"], body=#link{body=#i{class=["icon-circle-arrow-right", "icon-large"]}, postback={page, PageCount}}}
  ].

event(init) -> [];
event({product_feed, Id})-> wf:redirect("/product?id=" ++ integer_to_list(Id));
event({page, Page})->
  wf:update(pagination, pagination(Page)),
  wf:update(products, wf:js_escape(wf:render(list_products(Page))));
event({product, Id})-> wf:redirect("/product?id=" ++ Id);
event(to_list)->
  wf:session(page_size, list),
  wf:update(products, wf:js_escape(list_products(1))),
  wf:update(pagination, pagination(1));
event(to_grid)->
  wf:session(page_size, grid),
  wf:update(products, list_products(1)),
  wf:update(pagination, pagination(1));
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.
