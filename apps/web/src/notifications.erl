-module(notifications).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"notifications">>},{body, body()}]}.

body()->
  index:header() ++[
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(notifications, [#li{class=[divider]}, subnav() ])},
        #panel{class=[span9], body=[
          dashboard:section(notifications(), "icon-user")
        ]} ]} } }

  ]++index:footer().

notifications()-> [
  #h3{class=[blue], body= <<"Notifications">>},
  notification_bar(),

  #accordion{nav_stacked = false, items= [
    message(true),
    message(false),
    message(false)
  ]},
  #panel{class=["btn-toolbar", "text-center"], body=[
    #link{url= <<"#">>, class=[btn, "btn-warning", "btn-large"], body=[#i{class=["icon-trash", "icon-white"]}, <<"Delete Entries">> ]},
    #link{url= <<"#">>, class=[btn, "btn-info", "btn-large"], body=[#i{class=["icon-ok", "icon-white"]}, <<"Mark as Read">>]}
  ]}
  ].

subnav()-> [
  #li{body=#link{body=[<<"messages   ">>, #span{class=[label, "label-info"], body= <<"3">>}]}},
  #li{body=#link{body=[<<"comments   ">>, #span{class=[label, "label-info"], body= <<"0">>}]}} ].

message(Text) -> {
  #panel{body=[
    #strong{body=[#span{class=["icon-chevron-sign-down"]}, <<" Message \"Lorem ipsum\" from User282">>,  #span{class=["pull-right"],body= [#i{class=["icon-trash", "icon-large"]}, #i{class=["icon-flag", "icon-large"]}]} ]}
  ]},
  #panel{class=["notif-message-inner", clearfix], body=[
    #panel{class=[span10, "message-text"], body=[
      #p{body= <<"Donec libero velit, rutrum ac mollis a, porttitor id libero. Mauris congue cursus scelerisque. Sed eu commodo metus. Vestibulum vel lobortis risus. Proin a quam felis. Vestibulum et nulla eu dolor egestas elementum vestibulum vel ipsum. Nam dui erat, varius non placerat et, cursus eget libero. Pellentesque sed ornare nunc. Vivamus volutpat nunc in felis tempor consectetur.">>}
    ]}
  ]}}.

notification_bar() ->
  #panel{class=["row-fluid"], body=[
    #panel{class=[span2], body=[
      #checkbox{id=selectall, class=[checkbox], checked=true, body= "select all"}
    ]},
    #panel{class=[span2, "pull-right", "notif-meta"], body=[#p{body= <<"Page 1 of 1">>} ]}
  ]}.


event(init) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

