-module(dashboard).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").

sidebar_menu(Who, What, Page, Sublist) ->
%  error_logger:info_msg("~p look for ~p",[Who, What]),
  List = [
    begin
      {Class,Sub} = if Page ==I -> {active, Sublist} ; true -> {"", []} end,
      [#li{class=Class, body=#link{url="/"++atom_to_list(I), body=T}}, Sub]
    end 
    || {I, T} <- lists:filter(fun({E,_}) -> if Who==What -> true; true -> E==Page end end, [
      {profile, <<"profile">>},
      {myreviews, <<"reviews">>},
      {mygames, <<"games">>},
      {notifications, <<"notifications">>},
      {admin, <<"admin">>} ])
  ],

  #panel{class=["dash-sidebar-menu"], body=#list{class=[nav, "nav-list","dash-sidebar-nav", "affix-top"],
    data_fields=[{<<"data-spy">>, <<"affix">>}],
    body=List}}.

section(Body, Icon) ->
  #section{class=["row-fluid", "dashboard-section"], body=[
    #panel{class=[span1], body=#i{class=[Icon, "icon-2x", blue]}},
    #panel{class=[span11, "dashboard-unit"], body=Body}
  ]}.