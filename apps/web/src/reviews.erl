-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, case wf:session(page_size) of grid -> 8; _ -> 4 end).
-record(info, {entries, toolbar, category, fid}).

main()-> #dtl{file="prod", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
  case wf:qs(<<"id">>) of undefined ->skip; I -> error_logger:info_msg("~p RECEIVED!", [I]),wf:wire(wf:f("$('a[href=\"#~s\"]').addClass('text-warning').tab('show');", [binary_to_list(I)])) end,
  wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){$(e.target).addClass('text-warning').siblings().removeClass('text-warning');});"),
  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [#link{url="#all", body= <<"Categories ">>, style="color: black", data_fields=[{<<"data-toggle">>, <<"tab">>}]}, #small{body=[
          begin
            wf:wire(#api{name="api_"++Id, tag=tab}),
            wf:wire(wf:f("$('#~s').on('shown', function(e){console.log('shown' + e.target);});", [Id])),
          [
            <<" / ">>,
            #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}
          ] end || #group{id=Id, name=Name} <- kvs:all(group)]} ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane", active], body=[
            [[#product_entry{entry=E, mode=line, category=Name} || E <- lists:reverse(kvs_feed:entries(Fid, undefined, 10))] || #group{feed=Fid, name=Name} <- kvs:all(group)]
          ]},
          [ begin
              Entries = kvs_feed:entries(Fid, undefined, ?PAGE_SIZE),
              Last = case Entries of []-> []; E-> lists:last(E) end,
              EsId = wf:temp_id(),
              BtnId = wf:temp_id(),
              Info = #info{fid=Fid, entries=EsId, toolbar=BtnId, category=Name},
              NoMore = length(Entries) < ?PAGE_SIZE,
              #panel{id=Id, class=["tab-pane"], body=[
                #panel{id=EsId, body=[#product_entry{entry=E, mode=line, category=Name} || E <- Entries]},
                #panel{id=BtnId, class=["btn-toolbar", "text-center"], body=[
                  if NoMore -> []; true -> #link{class=[btn, "btn-large"], body= <<"more">>, postback={check_more, Last, Info}} end
                ]}
              ]}
            end ||#group{id=Id, name=Name, feed=Fid} <- kvs:all(group)]]},
        #panel{class=[span3], body=[<<"">>]} ]}
    ]}
  ]}
  ] ++ index:footer().

event(init) -> wf:reg(product_channel),[];
event(<<"PING">>) -> [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read_entry, {Id,_}})-> wf:redirect("/review?id="++Id);
event({check_more, Start, Info = #info{}}) ->
  read_entries(case Start of undefined -> undefined; S -> S#entry.entry_id end, Info),
  wf:update(Info#info.toolbar, []);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

api_event(Name,Tag,Term) -> error_logger:info_msg("[review] api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([show_entry], [Entry, #info{} = Info]) ->
  wf:insert_bottom(Info#info.entries, #product_entry{entry=Entry, mode=line, category=Info#info.category}),
  wf:wire("Holder.run();"), % hack to update the image placeholders
  wf:update(Info#info.toolbar, #link{class=[btn, "btn-large"], body= <<"more">>, postback={check_more, Entry, Info}});

process_delivery([no_more], [BtnId]) -> wf:update(BtnId, []), ok;
process_delivery(_R, _M) -> skip.

read_entries(StartFrom, #info{fid=Fid}=I)->
  Feed = case StartFrom of
    undefined-> kvs:get(feed, Fid);
    S -> kvs:get(entry, {S, Fid})
  end,
  case Feed of
    {error, not_found} -> [];
    {ok, #feed{}=F} -> traverse_entries(F#feed.top, ?PAGE_SIZE, I);
    {ok, #entry{prev = E}} -> traverse_entries(E, ?PAGE_SIZE, I)
  end.

traverse_entries(undefined,_, #info{toolbar=BtnId}) -> self() ! {delivery, [reviews, no_more], [BtnId]}, [];
traverse_entries(_,0,_) -> [];
traverse_entries(Next, Count, I)->
  case kvs:get(entry, Next) of
    {error, not_found} -> [];
    {ok, R}->
      self() ! {delivery, [reviews, show_entry], [R, I]},
      [R | traverse_entries(R#entry.prev, Count-1, I)]
  end.



