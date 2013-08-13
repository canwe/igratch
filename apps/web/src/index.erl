-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/products.hrl").
-include("records.hrl").

main() -> #dtl{file = "prod", ext="dtl", bindings=[{title, <<"iGratch">>},{body, body()}]}.

body() -> 
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){
    $(e.target).parent().siblings().find('a.text-warning').removeClass('text-warning');
  });"),
  {Tabs, Reviews} = reviews:reviews(),
  header() ++ [
  #section{id="slider-box", class=["row-fluid"], body=#panel{class=[container], body=
    #carousel{class=["product-carousel"], items=featured(),
      caption=#panel{class=["row-fluid"],body=[
        box(50, 12.99, orange, pc), box(50, 12.99, green, wii),
        box(50, 12.99, violet, xbox), box(50, 12.99, blue, pc) ]} }}},

  #section{class=["row-fluid"], body=[
    #panel{class=[container], body=[
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body=reviews:all(Reviews)}, Tabs]},
        #aside{class=[span3], body=[
          #panel{class=[sidebar], body=[
            #panel{class=["row-fluid"], body=[
              #h3{ class=[blue], body= <<"TAGS">>},
              #list{class=[inline, tagcloud], body=[
                #li{body=#link{url="#all", body= <<"all">>, data_fields=[{<<"data-toggle">>, <<"tab">>}] }},
                [#li{body=#link{url="#"++Id, body=Name, data_fields=[{<<"data-toggle">>, <<"tab">>}, {<<"data-toggle">>, <<"tooltip">>}], title=Desc}}
                || #group{id=Id, name=Name, description=Desc}<-kvs:all(group)] ]}
            ]},
            #panel{class=["row-fluid"], body=[#h3{ class=[blue], body= <<"MOST POPULAR">>}, [popular_item() || _ <-lists:seq(1,7)] ]}
          ]}
        ]}
      ]}
%      #panel{class=["btn-center"], body=[#button{class=[btn], body= <<"SHOW MORE">>}]}
    ]}
  ]} ] ++ footer().

featured() ->
  case kvs:get(group, "featured") of
    {error_notfound} -> [];
    {ok, G} ->
      Ps = lists:flatten([ case kvs:get(product, Who) of {ok, P}->P; {error,_}-> [] end || #group_subscription{who=Who}<-kvs_group:members(G#group.name)]),
      error_logger:info_msg("Featured items: ~p", [Ps]),
      [begin
        {Cover, Class} = case P#product.cover of
          undefined -> {<<"holder.js/1170%x380/text:no cover">>, "img-polaroid"};
          C -> 
            Ext = filename:extension(C),
            Name = filename:basename(C, Ext),
            Dir = filename:dirname(C),
            {filename:join([Dir, "thumbnail", Name++"_1170x380"++Ext]),""}
        end,
        [
          #panel{class=["slide-one"], body=[
            #h1{body=P#product.title},
            #image{class=[Class], image=Cover}
          ]},
          #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win, buy],
            body= [<<"Buy for ">>, #span{body= "$"++ float_to_list(P#product.price/100, [{decimals, 2}]) }], postback={checkout, P}}
        ]
      end || P <- Ps]
  end.

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

        #link{url="/index", class=[brand], body= #image{alt= <<"iGratch">>, image= <<"/static/img/logo.png">>}},
%        #link{url="/index", class=[brand], body=[ #image{alt= <<"iGratch">>, image= <<"/static/img/brand.png">>}, #span{class=["brand-label"], body= <<" iGratch">>}] },
        #panel{class=["nav-collapse", collapse], body=[
          #list{class=[nav, "pull-right"], body=[
            #li{body=#link{body= <<"Home">>, url= <<"/index">>}},
            #li{body=#link{body= <<"Games">>,url= <<"/store">>}},
            #li{body=#link{body= <<"Reviews">>, url= <<"/reviews">>}},
            case wf:user() of
              undefined -> #li{body=#link{body= <<"Sign In">>, url= <<"/login">>}};
              User -> [
                #li{body=[
                  #link{class=["dropdown-toggle", "profile-picture"], data_fields=[{<<"data-toggle">>, <<"dropdown">>}],
                    body=case User#user.avatar of undefined-> ""; Img-> #image{class=["img-circle", "img-polaroid"], image=iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]), width= <<"50px">>, height= <<"50px">>} end},
                  #list{class=["dropdown-menu"], body=[
                    #li{body=#link{id=logoutbtn, postback=logout, delegate=login, body=[#i{class=["icon-off"]}, <<"Logout">> ] }}
                  ]}]},
                #li{body=#link{body= <<"My Account">>, url= <<"/profile">>}}]
            end
          ]} ]} ]} ]} ]} ].

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

error(Msg)->
  #panel{class=[alert, "alert-danger","alert-block"], body=[#button{class=[close], data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>}, #strong{body= Msg} ]}.

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, reviews, {Id,_}})-> wf:redirect("/review?id="++Id);
event({checkout, #product{}=P}) -> wf:redirect("/checkout?product_id="++P#product.id);
event(Event) -> error_logger:info_msg("[index]Event: ~p", [Event]).

process_delivery([show_entry], M) -> product:process_delivery([show_entry], M);
process_delivery([no_more], M) -> product:process_delivery([no_more], M);
process_delivery(_R, _M) -> skip.
