-module(admin).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").

-include("records.hrl").

-define(PAGE_SIZE, case wf:session(page_size) of list -> 2; _ -> 8 end).

main()-> #dtl{file="dev", bindings=[{title,<<"admin">>},{body, body()}]}.

body()-> index:header() ++ [
  #section{class=[section], body=[
    #panel{class=[container], body=[
      #h3{body= <<"Control Panel">>},
      #list{class=[nav, "nav-tabs", "sky-nav", "entry-type-tabs"], body=[
        #li{class=[active], body=[#link{url= <<"#categories">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Categories">>}]},
        #li{body=[#link{url= <<"#users">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Users">>}]},
        #li{body=[#link{url= <<"#products">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"Products">>}]}
      ]},

      #panel{class=["tab-content"], body=[
        #panel{id=categories, class=["tab-pane", active], body=[
          #h3{body= <<"Category">>},
          #panel{class=["row-fluid"], body=[
            #panel{class=[span8], body=[
              #panel{class=[controls, "controls-row"], body=[
%                #textbox{id=cat_tag, class=[span3], placeholder= <<"tag">>},
                #textbox{id=cat_name, class=[span12], placeholder= <<"name">>}
              ]},
              #textarea{id=cat_desc, class=[span12], placeholder= <<"description">>},
              #link{id=save_cat, class=[btn, "btn-large", "pull-right"], body=[#i{class=["icon-tags"]}, <<" Create">>], postback=save_cat, source=[cat_name, cat_desc]}
            ]}
          ]},
          #h3{body= <<"Categories">>},
          #table{id=cats, class=[table, "table-hover"], body=[[#tr{cells=[#td{body=Id}, #td{body=Name}, #td{body=Desc}]} || G=#group{id=Id, name=Name, description=Desc}<-kvs:all(group)]]}
        ]},
        #panel{id=users, class=["tab-pane"], body=[
        ]},
        #panel{id=products, class=["tab-pane"], body=[
        ]}
      ]}
    ]}
  ]}
  ] ++ index:footer().

list_products(Page) -> [#product_row{product=P} || P <- lists:sublist(kvs:all(product), (Page-1) * ?PAGE_SIZE + 1, ?PAGE_SIZE)].
pagination(Page)->
  PageCount = (length(kvs:all(product))-1) div ?PAGE_SIZE + 1,
  error_logger:info_msg("Page: ~p", [PageCount]),
  [
  #li{class=[if Page==1-> "disabled"; true->[] end, "previous"], body=#link{body=#i{class=["icon-circle-arrow-left", "icon-large"]}, postback={page, 1} }},
  [#li{class=if I==Page -> active;true->[] end,body=#link{id="pglink"++integer_to_list(I),body=#span{style="line-height:normal;", body=integer_to_list(I)}, postback={page, I} }} 
    || I <- lists:seq(1, PageCount)],
  #li{class=[if PageCount==Page -> "disabled";true->[] end,"next"], body=#link{body=#i{class=["icon-circle-arrow-right", "icon-large"]}, postback={page, PageCount}}}
  ].


event(init) -> wf:reg(product_channel), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(save_cat) ->
  Name = wf:q(cat_name),
  Desc = wf:q(cat_desc),
%  Id = wf:q(cat_tag),
  Id = wf:temp_id(),
  Publicity = public,
  Creator = (wf:user())#user.email,
  case kvs_group:create(Creator, Id, Name, Desc, Publicity) of 
    {ok, G} ->
      error_logger:info_msg("Creted ~p", [G]),
      msg:notify([kvs_group, group, init], [G#group.id, G#group.feed]),
      wf:wire(wf:f("$('#cats > tbody:first').append('~s');", [wf:render(#tr{cells=[#td{body= G#group.id}, #td{body=G#group.description}]})]));
    {error, _} -> skip
  end;
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

api_event(Name,Tag,Term) -> error_logger:info_msg("[admin]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([create],
                 [{Creator, Id, Name, Desc, Publicity}]) ->
  error_logger:info_msg("responce to create group"),
  ok;
process_delivery(_R, _M) -> skip.
