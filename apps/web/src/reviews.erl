-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="dev", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container-fluid", "main-no-slider"], body=[
    #panel{id=first, body=[
      #panel{class=["row-fluid"], body=[
        #panel{class=["big-shadow"], body=[
          #image{class=[border], alt= <<"Store Image">>, image= <<"/static/img/store-img1.jpg">>}
        ]}
      ]},
      #panel{class=["row-fluid", review], body=[
        #panel{class=[span3], body=[
          #panel{class=[sidebar], body=[
            #panel{id="review-meta", class=["row-fluid"], body=[
              #h3{class=["blue capital"], body= <<"Action">>},
              #image{class=[border], alt= <<"The author">>, image= <<"/static/img/author.jpg">>},
              #panel{class=["article-meta"], body=[
                #p{class=[username], body= <<"John Smith">>},
                #p{class=[datestamp], body=[<<"Yesterday">>, #span{body= <<"1:00pm">>}]},
                #p{class=[statistics], body=[
                  #i{class=["icon-user"]},
                  #span{body= <<"1,045">>},
                  #i{class=["icon-comment"]},
                  #span{body= <<"25">>}
                ]}
              ]},
              #panel{class=[span5, offset2], body=[
                #link{url= <<"#">>, class=[btn, "btn-orange", capital], body= <<"Buy it!">>}
              ]}
            ]}
          ]}
        ]},
        #panel{class=[span9], body=[
          #panel{class=["row-fluid"], body=[
            #h3{class=["light-grey"], body= <<"Lorem ipsum dolor sit amet">>},
            #blockquote{class=["custom-quote"], body= <<"Nunc non sem lorem, at dictum ante. Duis id sapien in leo gravida vehicula id quis ipsum. Sed consectetur ullamcorper justo, id vestibulum ligula porttitor sit amet. Curabitur et lorem nunc, non facilisis turpis. Nunc ullamcorper gravida sodales. Nulla elit enim, dictum et consequat quis, accumsan viverra sem.">>},
            #p{body= <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas eget augue vitae leo vulputate viverra. Nunc sed risus eget arcu facilisis lacinia. Nunc non sem lorem, at dictum ante. Duis id sapien in leo gravida vehicula id quis ipsum. Sed consectetur ullamcorper justo, id vestibulum ligula porttitor sit amet. Curabitur et lorem nunc, non facilisis turpis. Nunc ullamcorper gravida sodales. Nulla elit enim, dictum et consequat quis, accumsan viverra sem. In hac habitasse platea dictumst. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. In dapibus tincidunt purus vitae vulputate. Proin a dolor ante. Praesent vel augue id ipsum luctus aliquet vitae a urna. Vivamus suscipit eleifend laoreet. Morbi cursus massa at magna feugiat eget facilisis arcu aliquam.">>},
            #panel{class=[span6, "no-margin-left"], body=[
              #image{class=[border], alt= <<"Review Image">>, image= <<"/static/img/review-img1.jpg">>}
            ]},
            #panel{class=[span6], body=[
              #image{class=[border], alt= <<"Review Image">>, image= <<"/static/img/review-img2.jpg">>}
            ]},
            #p{body=[
              <<"Aliquam erat tellus, ullamcorper ut facilisis quis, volutpat fermentum turpis. Pellentesque a orci a enim elementum congue. Sed tincidunt egestas adipiscing. Fusce elit justo, pellentesque id blandit eget, auctor et tortor.">>,
              #strong{class=["new-line"], body= <<"Quisque varius">>},
              <<"Morbi sit amet ligula vel enim rhoncus mollis vel ut lacus. Curabitur posuere adipiscing sapien vel sagittis. Donec et leo iaculis felis ornare congue sed id ligula. Maecenas cursus, diam sit amet lacinia luctus, leo mauris dignissim massa, vel pharetra nisl sem quis risus.">>,
              #strong{class=["new-line"], body= <<"Nullam eu justo">>},
              <<"Quisque fringilla sagittis quam, id fermentum felis volutpat in. Aenean faucibus nisi at diam laoreet ut vestibulum ipsum rhoncus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.">> 
            ]}
          ]},

          #panel{id="commentsWrap", class=["row-fluid", clearfix], body=[
            #panel{id="commentsWrap-inner", class=["clearfix"], body=[
              #image{class=["disq-options"], alt= <<"Disquss Buttons">>, image= <<"/static/img/disq-options.png">>},
              #panel{class=["row-fluid", "disq-arrows"], body=[
                #link{url= <<"#">>, class=["leftArrow"], body=[#image{alt= <<"">>, image= <<"/static/img/disq-arrow-left.png">>}]},
                #link{url= <<"#">>, class=["rightArrow","pull-right"], body=[#image{alt= <<"">>, image= <<"/static/img/disq-arrow-right.png">>}]}
              ]},
              #panel{id="comments-container", class=["row-fluid", span10, offset1], body=[
                comments_row(),
                comments_row(),
                comments_row()
              ]}
            ]}
          ]}
        ]},
        #panel{class=["row-fluid"], body=[
          #h3{class=[blue, capital, offset3, "h3-more"], body= <<"More action reviews">>},
          more_article(),
          more_article()
        ]}
      ]}
    ]} 
  ]}
  ] ++ index:footer().


comment() ->
  #panel{class=["single-comment", span6], body=[
    #panel{class=["comment-headline"], body=[
      #image{class=["avatar"], alt= <<"">>, image= <<"/static/img/commenter-avatar.jpg">>},
      #span{class=["commenter-from"], body= <<"Alan Brown">>},
      #span{class=["commenter-to"], body= <<"John Smith">>},
      #image{class=["pull-right", "disq-btns"], alt= <<"Disquss Buttons">>, image= <<"/static/img/disq-btns.png">>}
    ]},
    #panel{class=["comment-text"], body=[
      #p{body=[
        <<"Cras malesuada adipiscing risus, sed vehicula nibh hendrerit eu. Pellentesque posuere vulputate justo, ultrices vestibulum enim lobortis quis. Nam nisl est, commodo vitae blandit ac, mattis vel nibh.">>,
        #span{class=["comment-timestamp"], body= <<"Today 12:54am">>}
      ]}
    ]}
  ]}.

comments_row()-> 
  #panel{class=["row-fluid"], body=[
    comment(),
    comment()
  ]}.

more_article() ->
  #panel{class=["game-article","shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue, capital], body= <<"Action">>},
        #p{class=[username], body= <<"John Smith">>}
        #p{class=[datestamp], body=[ <<"Yesterday">>, #span{body= <<"1:00pm">>}]},
        #p{class=[statistics], body=[
          #i{class=["icon-user"]},
          #span{body=[1,045]},
          #i{class=["icon-comment"]},
          #span{body= <<"25">>}
        ]}
      ]},
      #panel{class=[span3, shadow], body=#image{class=["border"], alt= <<"Row Four Image">>, image= <<"/static/img/row4.jpg">>}},
      #panel{class=[span6, "article-text"], body=[
        #h3{class=["light-grey"], body= <<"Lorem ipsum dolor sit amet">>},
        #p{body=[<<"Duis bibendum tortor at ligula condimentum sed dignissim elit tincidunt. Aliquam luctus ornare tortor ac hendrerit. Nam arcu odio, pretium et cursus nec, tempus ac massa. Nam eleifend quam eu justo adipiscing id eleifend tortor ullamcorper... ">>,
          #link{url= <<"#">>, body= <<"Read">>}
        ]}
      ]}
    ]}
  ]}.

event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.
