-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
  case wf:qs(<<"id">>) of undefined ->skip; I -> wf:wire(wf:f("$('a[href=\"#~s\"]').addClass('text-warning').tab('show');", [binary_to_list(I)])) end,
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){$(e.target).addClass('text-warning').siblings().removeClass('text-warning');});"),
  {Tabs, Reviews} = reviews(),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [
            #link{url="#all", body= <<"Categories ">>, style="color: black", data_fields=[{<<"data-toggle">>, <<"tab">>}]},
            #small{body=[
              begin 
                [<<" / ">>, #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}]
              end || #group{id=Id, name=Name, scope=Scope} <- kvs:all(group), Scope==public
            ]} 
          ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body=all(Reviews)}, Tabs
        ]},
        #panel{class=[span3], body=[<<"">>]} ]}
    ]}
  ]}
  ] ++ index:footer().

all(Reviews) -> [
  #product_entry{entry=E, mode=line} || E <- lists:foldl(
    fun(#entry{entry_id=Eid}=E, Ai) -> [E|lists:filter(fun(#entry{entry_id=Eid1})-> Eid =/= Eid1 end, Ai)] end, [], lists:flatten(Reviews)) ].

reviews() ->
  Groups = [G || #group{scope=Scope}=G <- kvs:all(group), Scope==public],
  error_logger:info_msg("Groups: ~p", [Groups]),
  lists:mapfoldl(fun(#group{id=Id, name=Name, feeds=Feeds}, Acc)->
    {_, Fid}= Feed = lists:keyfind(feed, 1, Feeds),
    Entries = kvs_feed:entries(Feed, undefined, ?PAGE_SIZE),
    Last = case Entries of []-> []; E-> lists:last(E) end,
    EsId = wf:temp_id(),
    BtnId = wf:temp_id(),
    Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId, category=Name},
    NoMore = length(Entries) < ?PAGE_SIZE,
    {#panel{id=Id, class=["tab-pane"], body=[
      #panel{id=EsId, body=[#product_entry{entry=E, mode=line, category=Name} || E <- Entries]},
        #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
          if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Last, Info}} end ]} ]},
    [Acc|Entries]} end, [], Groups).

api_event(Name,Tag,Term) -> error_logger:info_msg("[review] api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read, reviews, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[reviews]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> product:process_delivery(R,M).
