-module(dashboard).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").

sidebar_menu(Who, What, Page, Sublist) ->
%  error_logger:info_msg("Who ~p What ~p Page ~p", [Who, What, Page]),
  List = [
    begin
      {Class,Sub} = if Page ==I -> {active, Sublist} ; true -> {"", []} end,
      {_, Fid}= lists:keyfind(direct,1,What#user.feeds),
      Label = if I == notifications -> #span{class=[label, "label-info"], body=[
        case kvs:get(feed, Fid) of
          {ok, Feed} -> integer_to_list(Feed#feed.entries_count);
          {error, not_found} -> <<"0">>
        end
      ]}; true-> [] end,
      [#li{class=Class, body=#link{url="/"++atom_to_list(I), body= [T, Label]}}, Sub]
    end 
    || {I, T} <- lists:filter(fun({E,_}) ->
        if Who==What ->
          case E of
            myreviews -> kvs_acl:check_access(What#user.email, {feature, reviewer})==allow;
            mygames -> kvs_acl:check_access(What#user.email, {feature, developer})==allow;
            admin -> kvs_acl:check_access(What#user.email, {feature, admin})==allow;
            _ -> true
          end; true -> E==Page end
      end, [
      {profile, <<"profile">>},
      {myreviews, <<"reviews">>},
      {mygames, <<"games">>},
      {notifications, <<"notifications">>},
      {admin, <<"admin">>} ])
  ],

  #panel{class=["dash-sidebar-menu"], body=#list{class=[nav, "nav-list","dash-sidebar-nav", "affix-top"],
    data_fields=[{<<"data-spy">>, <<"affix">>}],
    body=List}}.

alt_section(Body,Icon)-> section(wf:temp_id(), Body, Icon, "alt").
section(Body, Icon)-> section(wf:temp_id(),Body, Icon).
section(Id, Body, Icon) -> section(Id, Body,Icon, "").
section(Id, Body, Icon, Section) ->
  #section{class=["row-fluid", "dashboard-section"], body=[
    #panel{class=[span1], body=#i{class=[Icon, "icon-2x", blue]}},
    #panel{id=Id, class=[span11, "dashboard-unit", Section], body=Body}
  ]}.