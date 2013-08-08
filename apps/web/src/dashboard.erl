-module(dashboard).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").


sidebar_menu(Page)-> sidebar_menu(Page, []).
sidebar_menu(Page, List)->
  Lis = [
    begin
      {Class,Sub} = if Page ==I -> {active, List} ; true -> {"", []} end,
      [#li{class=Class, body=#link{url="/"++atom_to_list(I), body=T}}, Sub]
    end 
    || {I, T} <- [
      {profile, <<"profile">>},
      {myreviews, <<"reviews">>},
      {mygames, <<"games">>},
      {notifications, <<"notifications">>},
      {admin, <<"admin">>} ]],
  #panel{class=["docs-sidebar-menu", "dash-sidebar-menu"], body=#list{class=[nav, "nav-list", "docs-sidebar-nav", "dash-sidebar-nav", "affix-top"],
    data_fields=[{<<"data-spy">>, <<"affix">>}],
    body=Lis}}.

section(Body, Icon) ->
  #section{class=["row-fluid", "dashboard-section"], body=[
    #panel{class=[span1], body=#i{class=[Icon, "icon-2x", blue]}},
    #panel{class=[span11, "dashboard-unit"], body=Body}
  ]}.