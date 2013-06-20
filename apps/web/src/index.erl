-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file = "dev", bindings=[{title, <<"iGratch">>},{body, body()}]}.

body() -> header() ++ [
  #section{id="slider-box", class=["row-fluid"], body=#panel{class=[container], body=
    #carousel{class=["product-carousel"], items=[slide() || _ <-lists:seq(1,3)],
      caption=#panel{class=["row-fluid"],body=[
        box(50, 12.99, orange, pc), box(50, 12.99, green, wii),
        box(50, 12.99, violet, xbox), box(50, 12.99, blue, pc) ]} }}},

  #section{class=["row-fluid"], body=[
    #panel{class=[container], body=[
      #panel{class=["row-fluid"], body=[
        #panel{class=["span9"], body=[
          article("RPG",       "/static/img/row1.jpg", "Lorem ipsum dolor sit amet", long_description()),
          article("ADVENTURE", "/static/img/row2.jpg", "Lorem ipsum dolor sit amet", long_description()),
          article("STRATEGY",  "/static/img/row3.jpg", "Lorem ipsum dolor sit amet", long_description()),
          article("ACTION",    "/static/img/row4.jpg", "Lorem ipsum dolor sit amet", long_description()) ]},

        #aside{class=[span3], body=[
          #panel{class=[sidebar], body=[
            #panel{class=["row-fluid"], body=[
              #h3{ class=[blue], body= <<"TAGS">>},
              #p{class=[tagcloud], body= <<"Praesent et nisl in lacus tincidunt convallis. Sed eget felis at neque sodales suscipit sit amet eu metus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Praesent leo ligula, adipiscing vitae placerat quis, condimentum fringilla est. Nulla nec diam erat. Sed non velit ligula, vitae scelerisque arcu. Vivamus fermentum rutrum neque pellentesque tristique">>}
            ]},
            #panel{class=["row-fluid"], body=[#h3{ class=[blue], body= <<"MOST POPULAR">>}, [popular_item() || _ <-lists:seq(1,7)] ]}
          ]}
        ]}
      ]},
      #panel{class=["btn-center"], body=[#button{class=[btn], body= <<"SHOW MORE">>}]}
    ]}
  ]} ] ++ footer().

slide() -> [
  #panel{class=["slide-one"]}, %#image{image= <<"/static/img/slide1.jpg">>},
  #panel{class=[buy], body=[#p{body=[ <<"Buy for ">>, #span{body= <<"$79.99">>}]}]} ].

popular_item()->
  #panel{class=["popular-item"], body=[
  #panel{class=["popular-item-inner"], body=[
    #p{body= <<"Vivamus fermentum rutrum neque pellentesque tristique.">>},
      #i{ class=["icon-comment"]},#span{body= <<"25">>}
    ]}
  ]}.

article(Category, Image, DescrHead, Description)->
  #panel{class=["row-fluid"], body=[
    #panel{class=["homepage-article"], body=[
      #panel{class=["homepage-article-inner", clearfix], body=[
        #panel{class=[span3, "article-meta"], body=[
          #h3{ class=[blue], body= list_to_binary(Category)},
          #p{class=[username], body= <<"John Smith">>},
          #p{class=[datestamp], body=[ <<"Yesterday">>, #span{body= <<"1:00pm">>} ]},
          #p{class=[statistics], body=[#i{class=["icon-user"]},#span{body= <<"1,045">>},#i{class=["icon-comment"]},#span{body= <<"25">>} ]} ]},
        #panel{class=[span4, shadow], body=[#image{image=list_to_binary(Image), alt="Row Three Image", class=["BorderAndShadow"]}]},
        #panel{class=[span5, "article-text"], body=[
          #h3{body= list_to_binary(DescrHead)},
          #p{body= [list_to_binary(Description), #link{body= <<"Read">>}]} ]} ]} ]} ]}.

long_description()->
  "Duis bibendum tortor at ligula condimentum sed dignissim elit tincidunt."
  "Aliquam luctus ornare tortor ac hendrerit. Nam arcu odio, pretium et cursus nec,"
  " tempus ac massa. Nam eleifend quam eu justo adipiscing id eleifend tortor ullamcorper...".

box(Discount, Price, ColorClass, IconClass)->
  #panel{class=[box, span3, ColorClass], body=[
    #p{body= <<"Lorem: Ipsum dolor sit amet">>},
    #panel{class=[accent], body= list_to_binary(integer_to_list(Discount)++"% OFF")},
    #panel{class=[price], body= list_to_binary("$"++io_lib:format("~.2f", [Price]))},
    #panel{class=[hardware, IconClass]} ]}.


header() -> [
  #header{class=[navbar, "navbar-fixed-top", ighead], body=[
    #panel{class=["navbar-inner"], body=[
      #panel{class=["container"], body=[
        #button{class=[btn, "btn-navbar"], data_fields=[{<<"data-toggle">>, <<"collapse">>}, {<<"data-target">>, <<".nav-collapse">>}], body=[#span{class=["icon-bar"]}||_<-lists:seq(1,3)]},

        #h1{body=#link{class=[brand], body= <<"iGratch">>}},
        #panel{class=["nav-collapse", collapse], body=[
          #list{class=[nav, "pull-right"], body=[
            #li{body=#link{body= <<"Home">>,url="#"}},
            #li{body=#link{body= <<"Games">>,url="#"}},
            #li{body=#link{body= <<"Reviews">>}},
            #li{class=[dropdown], body=[
              #link{class=["dropdown-toggle"], body=[<<"My Account">>]},
              #list{class=["dropdown-menu"], body=[]}
            ]} ]} ]} ]} ]} ]} ].

footer() -> [
  #footer{class=[igfoot],body=#panel{class=[container, "text-center"], body=[
    #panel{body=[
      #image{image= <<"/static/img/footer-highlight.png">>},
      #image{image= <<"/static/img/footer-shadow.png">>}
    ]},
    #panel{body=[
      #list{class=[icons, inline], body=[
        #li{body=#link{body= <<"About">>}},
        #li{body=#link{body= <<"Help">>}},
        #li{body=#link{body= <<"Terms of Use">>}},
        #li{body=#link{body= <<"Privacy">>}},
        #li{body=#link{body= <<"RSS">>}},
        #li{body= <<"&copy; iGratch 2013">>}
      ]},
      #list{class=[icons, inline], body=[
        #li{body=#link{body=#image{image= <<"/static/img/social1.png">>}}},
        #li{body=#link{body=#image{image= <<"/static/img/social2.png">>}}},
        #li{body=#link{body=#image{image= <<"/static/img/social3.png">>}}},
        #li{body=#link{body=#image{image= <<"/static/img/social4.png">>}}},
        #li{body=#link{body=#image{image= <<"/static/img/social5.png">>}}},
        #li{body=#link{body=#image{image= <<"/static/img/social6.png">>}}} ]} ]} ]}}].

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> [];
event(Event) -> error_logger:info_msg("Event: ~p", [Event]).
