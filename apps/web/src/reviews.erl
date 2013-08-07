-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, case wf:session(page_size) of list -> 2; _ -> 8 end).

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){$(e.target).addClass('text-warning').siblings().removeClass('text-warning');});"),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [#link{url="#all", body= <<"Categories ">>, style="color: black", data_fields=[{<<"data-toggle">>, <<"tab">>}]}, #small{body=[[
            <<" / ">>,
            #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}
          ] || #group{id=Id, name=Name} <- kvs:all(group)]} ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body=[
            [#product_entry{entry=E, mode=line, category=Name} || E <- lists:reverse(kvs_feed:entries(Fid, undefined, 10))] || #group{feed=Fid, name=Name} <- kvs:all(group)
          ]},
          [ #panel{id=Id, class=["tab-pane"], body=[#product_entry{entry=E, mode=line, category=Name} || E <- lists:reverse(kvs_feed:entries(Fid, undefined, 10))]}
            ||#group{id=Id, name=Name, feed=Fid} <- kvs:all(group)]]},
        #panel{class=[span3], body=[<<"">>]} ]}
    ]}
  ]}
  ] ++ index:footer().

event(init) -> [];
event(<<"PING">>) -> [];
event({read_entry, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.
