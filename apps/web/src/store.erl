-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"Store">>},{body, body()}]}.

body()->
  case wf:qs(<<"id">>) of undefined ->skip; I -> wf:wire(wf:f("$(document).ready(function(){$('a[href=\"#~s\"]').addClass('text-warning').tab('show');});", [binary_to_list(I)])) end,
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){$(e.target).addClass('text-warning').siblings().removeClass('text-warning');});"),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [#link{url="#all", body= <<"Categories ">>, style="color: black", data_fields=[{<<"data-toggle">>, <<"tab">>}]}, #small{body=[
          begin
          [
            <<" / ">>,
            #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}
          ] end || #group{id=Id, name=Name, scope=Scope} <- kvs:all(group), Scope==public]} ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body= all()},
          [ begin
              {Feed,Fid} = lists:keyfind(products,1,Feeds),
              Entries = kvs_feed:entries({Feed,Fid}, undefined, ?PAGE_SIZE),
              Last = case Entries of []-> []; E-> lists:last(E) end,
              EsId = wf:temp_id(),
              BtnId = wf:temp_id(),
              Info = #info_more{fid=Fid, entries=EsId, toolbar=BtnId, category=Name},
              NoMore = length(Entries) < ?PAGE_SIZE,
              #panel{id=Id, class=["tab-pane"], body=[
                #panel{id=EsId, body=[#product_entry{entry=E, mode=line, category=Name} || E <- Entries]},
                #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
                  if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, delegate=product, postback={check_more, Last, Info}} end
                ]}
              ]}
            end ||#group{id=Id, name=Name, feeds=Feeds, scope=Scope} <- kvs:all(group), Scope==public]]},
        #panel{class=[span3], body=[<<"">>]} ]}
    ]}
  ]},
  #section{class=[section, alt], body=#panel{class=[container], body=[
      #panel{class=["hero-unit", "text-center"], body=[
        #h1{body= <<"Got a question?">>},
        #p{body= <<"want to work with us to move your bussines to the next level? Well, dont be afraid">>},
        #link{class=[btn, "btn-large", "btn-info"], body= <<"contact us">>}
      ]}
  ]}}
  ] ++ index:footer().

all() -> [
  #product_entry{entry=E, mode=line} || E <-  lists:foldl(
    fun(#entry{entry_id=Eid}=E, Ai) -> [E|lists:filter(fun(#entry{entry_id=Eid1})-> Eid =/= Eid1 end, Ai)] end,
    [],
    lists:flatten([ [E || E <- kvs_feed:entries(Feed, undefined, ?PAGE_SIZE)]
      || Feed <- [case lists:keyfind(products, 1, F) of false-> []; Fd -> Fd end || #group{feeds=F}<- kvs:all(group)] ]))].

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, {Id,_}})-> wf:redirect("/product?id="++Id);
event(Event) -> error_logger:info_msg("[store]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> product:process_delivery(R,M).

