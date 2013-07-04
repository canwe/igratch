-module(notifications).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="dev", bindings=[{title,<<"notifications">>},{body, body()}]}.

body()-> index:header() ++ [
  #panel{id="main-container", class=["container-fluid", "main-no-slider", account], body=[
    #panel{id=first, body=[
      #h2{class=[blue], body= <<"Notifications">>},
      notification_bar(),

      #panel{class=["row-fluid"], body=[
        #panel{class=[span2, "notif-messages-nav"], body=[
          #list{body=[
            #li{body=[
              <<"All notifications">>,
              #span{class=["notif-numbers"], body= <<"2">>},
              #list{body=[
                #li{body=[<<"Messages">>, #span{class=["notif-numbers"], body= <<"1">>}]},
                #li{body=[<<"Comments">>, #span{class=["notif-numbers"], body= <<"1">>}]}
              ]}
            ]}
          ]}
        ]},
        #panel{class=["span10"], body=[
          message(true),
          message(false),
          message(false)
        ]}
      ]},
      notification_bar(),
      #panel{class=["row-fluid"], body=[
        #panel{id="notif-btns-large", class=[span5, offset4], body=[
          #link{url= <<"#">>, class=[btn, "btn-orange", capital], body=[#i{class=["icon-trash", "icon-white"]}, <<"Delete Entries">> ]},
          #link{url= <<"#">>, class=[btn, "btn-blue", capital], body=[#i{class=["icon-ok", "icon-white"]}, <<"Mark as Read">>]}
        ]}
      ]}
    ]}
  ]} ] ++ index:footer().

message(Text) ->
  #panel{class=["row-fluid", "notif-message"], body=[
    #panel{class=["notif-message-inner", clearfix], body=[
      #panel{class=[span1], body=[
        #checkbox{id=selectall, class=["checkbox"], checked=true},
        #link{url= <<"#">>, class=["notif-check-arrow"], body=[#image{image= <<"/static/img/notif-arrow-down.png">>}]}
      ]},
      #panel{class=[span10, "message-text"], body=[
        #strong{body=[ <<"Message \"Lorem ipsum\" from">>, #link{url= <<"#">>, body= <<"User282">>}]},
        #p{show_if=Text==true, body= <<"Donec libero velit, rutrum ac mollis a, porttitor id libero. Mauris congue cursus scelerisque. Sed eu commodo metus. Vestibulum vel lobortis risus. Proin a quam felis. Vestibulum et nulla eu dolor egestas elementum vestibulum vel ipsum. Nam dui erat, varius non placerat et, cursus eget libero. Pellentesque sed ornare nunc. Vivamus volutpat nunc in felis tempor consectetur.">>}
      ]},
      #panel{class=[span1, "delete-flag"], body=[
        #link{url= <<"#">>, body=[#i{class=["icon-trash"]}]},
        #link{url= <<"#">>, body=[#i{class=["icon-flag"]}]}
      ]}
    ]}
  ]}.

notification_bar() ->
      #panel{class=["row-fluid"], body=[
        #panel{class=["img-rounded", clearfix, "notif-bar"], body=[
          #panel{class=[span2, offset2], body=[
            #checkbox{id=selectall, class=[checkbox], checked=true, body= "select all"}
          ]},
          #panel{class=[span3, "pull-right", "notif-meta"], body=[
            #p{body= <<"Page 1 of 4">>},
            #panel{class=["notif-nav-wrap","pull-right"], body=[
              #link{url= <<"#">>, body=[#image{image= <<"/static/img/notif-arrow-left.png">>}]},
              #link{url= <<"#">>, body=[#image{image= <<"/static/img/notif-arrow-right.png">>}]}
            ]}
          ]}
        ]}
      ]}.


event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

