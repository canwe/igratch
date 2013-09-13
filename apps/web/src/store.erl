-module(store).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"Store">>},{body, body()}]}.

body()->
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){"
        "id=$(e.target).attr('href');"
        "if(id!='#all')$('a[href=\"#all\"').removeClass('text-warning');"
        "else $(e.target).parent().find('.text-warning').removeClass('text-warning');"
        "$(e.target).addClass('text-warning').siblings().removeClass('text-warning');"
        "tabshow(id);});"),
    Tab = case wf:qs(<<"id">>) of undefined -> "all"; T ->  T end,
    wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').addClass('text-warning').tab('show');});",[Tab])),

  index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #panel{class=["page-header"], body=[
          #h2{body= [#link{url="#all", body= [#span{class=["icon-home"]}], data_fields=[{<<"data-toggle">>, <<"tab">>}]}, #small{body=[
          begin
          [
            <<" / ">>,
            #link{url="#"++Id, data_fields=[{<<"data-toggle">>, <<"tab">>}], body=[#span{class=["icon-asterisk"]},Name]}
          ] end || #group{id=Id, name=Name, scope=Scope} <- kvs:all(group), Scope==public]} ]}
      ]},
      #panel{class=["row-fluid"], body=[
        #panel{class=[span9, "tab-content"], body=[
          #panel{id=all, class=["tab-pane"]},
          [#panel{id=Id, class=["tab-pane"]} || #group{id=Id, scope=Scope} <- kvs:all(group), Scope==public]
        ]},
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

feed("all")->
    State = ?FD_STATE(?FEED(product))#feed_state{view=product, html_tag=panel, entry_type=product},
    #feed_ui{title= <<"">>, icon="icon-tags", state=State};

feed(Group) ->
    case kvs:get(group, Group) of {error,_}->[];
    {ok, G}->
        {_, Id} = lists:keyfind(products, 1, element(#iterator.feeds, G)),
        State = ?FD_STATE(Id)#feed_state{view=product, html_tag=panel, entry_id=#entry.entry_id},
        #feed_ui{title= <<"">>, icon="icon-tags", state=State}
    end.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(Id)),
    wf:wire("Holder.run();").

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({product_feed, Id})-> wf:redirect("/product?id=" ++ Id);
event({read, product, Id})-> wf:redirect(?URL_PRODUCT(Id));
event({checkout, Pid}) -> wf:redirect("/checkout?product_id="++Pid);
event(Event) -> error_logger:info_msg("[store]Page event: ~p", [Event]), ok.

process_delivery(R,M) -> feed_ui:process_delivery(R,M).
