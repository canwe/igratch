-module(myreviews). 
-compile(export_all).   
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/membership.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"my reviews">>},{body, body()}]}.

body()->
  index:header() ++[
  #section{id=content, body=
    #panel{class=[container, account], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(myreviews)},
        #panel{class=[span9], body=[
          dashboard:section(input(), "icon-user"),
          dashboard:section(reviews(), "icon-list")
        ]} ]} } }
  ]++index:footer().

input()->
  User = wf:user(),
  Medias = case wf:session(medias) of undefined -> []; Ms -> Ms end,
  MsId = wf:temp_id(),
  case User of undefined -> []; _->[
    #h3{class=[blue], body= <<"Submit review">>},
    #panel{class=["row-fluid"], body=[
      #panel{class=[span9], body=[
        #textboxlist{id=cats, placeholder= <<"Categories/Games/Tags">>},
        #textbox{class=[span12], placeholder= <<"Title">>},
        #htmlbox{class=[span12], root=?ROOT, dir="static/"++User#user.email, post_write=attach_media, img_tool=gm, post_target=MsId, size=[{270, 124}, {200, 200} , {139, 80}] },
        #panel{class=["btn-toolbar"], body=[#link{class=[btn, "btn-large"], body=[#i{class=["icon-ok", "icon-white"]}, <<" Post">>]}]},
        #panel{id=MsId, body=product_ui:preview_medias(MsId, Medias)}
      ]},
      #panel{class=[span2], body=[]}
  ]} ] end.

reviews() ->[
  #h3{class=[blule], body= <<"My Reviews">>},
  [[#product_entry{entry=E, mode=line, category=Name} || E <- lists:reverse(kvs_feed:entries(lists:keyfind(feed,1,Feeds), undefined, 10))] || #group{feeds=Feeds, name=Name} <- kvs:all(group)]].

control_event("cats", _) ->
  SearchTerm = wf:q(term),
  Data = [ [list_to_binary(Id++"="++Name), list_to_binary(Name)] || #group{id=Id, name=Name} <- kvs:all(group), string:str(string:to_lower(Name), string:to_lower(SearchTerm)) > 0],
  element_textboxlist:process_autocomplete("cats", Data, SearchTerm);
control_event(_, _) -> ok.


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id="++integer_to_list(Id));
event(Event) -> error_logger:info_msg("[account]Page event: ~p", [Event]), ok.

api_event(attach_media, Tag, Term) -> product:api_event(attach_media, Tag, Term);
api_event(Name,Tag,Term) -> error_logger:info_msg("[account]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery(_R, _M) -> skip.
