-module(feed).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, 4).

main() -> #dtl{file="dev", bindings=[{title,<<"feed">>},{body, body()}]}.

body() ->
%  Qid = wf:qs(<<"id">>),
  index:header()++[
  #section{class=[section, alt], body=#panel{class=[container], body=[
    #panel{class=["hero-unit"], body=[
      #h1{body= <<"Product feed">>},
      #p{body= <<"Thats a single review page">>}
    ]}
  ]}},

  #section{class=[section], body=#panel{class=[container], body=#panel{class=["row-fluid"], body=[
    #panel{class=[span9], body=[

  #article{class=["blog-post"], body=[
    #header{class=["blog-header"], body=[
      #h2{body=[<<"Lorem ipsum dolor">>, #small{body=[<<" by">>, #link{body= <<" John Doe">>}, <<" 12 Sep 2012.">>]}]}
    ]},
    #figure{class=["thumbnail-figure"], body=[
          #link{url= <<"/feed?id=1">>, body=[#image{image= <<"/static/img/crysis3-bg1.png">>} ]},
          #figcaption{class=["thumbnail-title"], body=[
            #h3{body=#span{body= <<"Lorem ipsum dolor sit amet">>}}
          ]}
    ]},
    #p{body= <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,            quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo            consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.">>},
    #p{body= <<"Nullam eget suscipit turpis. Suspendisse nec magna et velit elementum vulputate. Suspendisse ac nibh lectus, at sollicitudin turpis. Aenean ut tortor a felis consectetur pulvinar. Phasellus mattis viverra luctus. Pellentesque tempor bibendum arcu non vestibulum. In bibendum mattis nibh, nec laoreet enim pretium a. Donec augue sem, convallis euismod pellentesque non, facilisis non nulla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Duis blandit cursus mi, malesuada euismod enim accumsan nec. Pellentesque nisl enim, elementum non molestie eu, lobortis sed odio. Mauris vehicula commodo neque, nec viverra libero accumsan in.">>},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span6], body=#p{body= <<"Vestibulum et dapibus orci. Vivamus non elit sed quam egestas egestas. Donec interdum ultrices ante ac pharetra. Praesent euismod augue erat, vel ornare sem. Morbi ante nisl, fringilla at commodo vitae, cursus aliquet lacus. Maecenas nec orci at est venenatis congue vitae eget justo. Aenean dui odio, eleifend sed viverra et, eleifend vitae tortor. Duis ac diam vitae risus aliquam tempor. Phasellus volutpat, metus porttitor ornare gravida, erat lacus bibendum lectus, et dictum tortor ligula a est. Proin id purus eget mi vestibulum accumsan. Nam scelerisque malesuada tellus vel laoreet. Vestibulum eleifend, risus ut faucibus iaculis">>}},
      #panel{class=[span6], body=[#image{image= <<"/static/img/crysis3-bg2.png">>}]}
    ]},
    #p{body= <<"Quisque diam libero, aliquam eget blandit et, vulputate vel felis. Quisque ut purus at justo mattis volutpat. Curabitur nibh neque, sodales feugiat suscipit vel, vulputate nec quam. Sed quam nulla, sollicitudin non pulvinar in, viverra ac nunc. Maecenas a neque quis mauris vehicula viverra eu eget elit. Suspendisse potenti. Donec tincidunt sollicitudin elementum. Nunc volutpat purus ac lectus tincidunt et bibendum quam sollicitudin. Pellentesque rutrum ultricies porttitor. Suspendisse pellentesque rutrum mollis. Integer varius nulla quis metus varius imperdiet. ">>},
    #footer{class=["blog-footer", "row-fluid"], body=[
          #panel{class=[span4, "blog-categories"], body=[#i{class=["icon-pushpin"]}, #link{body= <<" consectetur">>}]},
          #panel{class=[span4, "blog-tags"], body=[#i{class=["icon-tags"]}, #link{body= <<" fugiat, nulla, pariatur">>}]},
          #panel{class=[span4, "blog-more"], body=[#i{class=["icon-link"]}, #link{body= <<" read more">>}]}
    ]}
  ]},

      #panel{class=[comments], body=[
        #h3{body= <<"comments">>},
        comment([comment(), comment([comment()])]),
        comment()
      ]},
      #panel{class=["comments-form"], body=[
        #h3{class=["comments-form"], body= <<"Add your comment">>},
        #panel{class=["form-horizontal"], body=[
          #fieldset{body=[
            #panel{class=["control-group"], body=[
              #label{class=["control-label"], for="name", body= <<"Name">>},
              #panel{class=["controls"], body=[
                #textbox{id=name, class=["input-xxlarge"]}
              ]}
            ]},
            #panel{class=["control-group"], body=[
              #label{class=["control-label"], for="email", body= <<"Email">>},
              #panel{class=["controls"], body=[
                #textbox{id=email, class=["input-xxlarge"]}
              ]}
            ]},
            #panel{class=["control-group"], body=[
              #label{class=["control-label"], for="message", body= <<"Your message">>},
              #panel{class=["controls"], body=[
                #textarea{id=message, class=["input-xxlarge"]}
              ]}
            ]},
            #panel{class=["control-group"], body=[
              #panel{class=["controls"], body=[
                #button{class=[btn, "btn-info", "btn-large"], body= <<"send">>}
              ]}
            ]}
          ]}
        ]}
      ]}
    ]}
  ]}}}

  ]++index:footer().

title()-> <<"Review title">>.


comment() -> comment([]).
comment(InnerComment)->
  #panel{class=[media, "media-comment"], body=[
    #link{class=["pull-left"], body=[
      #image{class=["media-objects","img-circle"], data_fields=[{<<"data-src">>, <<"holder.js/64x64">>}]}
    ]},
    #panel{class=["media-body"], body=[
      #p{class=["media-heading"], body=[
        <<"John Doe, 12 Sep 2012.">>, 
        #link{class=["comment-reply","pull-right"], body= <<"reply">>}
      ]},
      #p{body= <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.">>},
      InnerComment
    ]}
  ]}.

description(Id, Description) -> [
  #panel{id="description"++Id, body=[
    #panel{class=[collapse, in], body= <<"Description head">>},
    #panel{id=Id, class=collapse, body= Description}
  ]},
  #button{class=[btn, "btn-link"], data_fields=[
    {<<"data-toggle">>, <<"collapse">>},
    {<<"data-target">>, list_to_binary("#"++Id) },
    {<<"data-parent">>, list_to_binary("#description"++Id)}], body= <<"Read...">>}].

list_medias(C)->
  {Cid, Fid} = C#comment.id,
  [#feed_media{media=M, target=wf:temp_id(), fid = Fid, cid = Cid} ||  M <- C#comment.media].

image_media(_Media)-> element_image:render_element(#image{image= "/static/img/item-bg.png"}).

event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), [].

api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]), event(change_me).

control_event(Id, Tag) -> 
  error_logger:info_msg("Tinymce editor control event ~p: ~p", [Id, Tag]),
  Ed = wf:q(mcecontent),
  error_logger:info_msg("Data:  ~p", [Ed]),
  ok.