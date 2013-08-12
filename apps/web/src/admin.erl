-module(admin).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"admin">>},{body, body()}]}.

body() -> index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(admin, [#li{class=[divider]}, subnav() ])},
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
          #panel{class=["tab-content"], body=[
            #panel{id=categories, class=["tab-pane", active], body=[
              dashboard:section(input(), "icon-user"),
              dashboard:section(categories(), "icon-list")
            ]},
            #panel{id=users, class=["tab-pane"], body=[
              dashboard:section(users(), "icon-user")
            ]},
            #panel{id=products, class=["tab-pane"], body=[
              dashboard:section(products(), "icon-user")
            ]}
          ]}
        ]} ]} } }
  ] ++ index:footer().

subnav() -> [
    #li{class=[active], body=[#link{url= <<"#categories">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"categories">>}]},
    #li{body=[#link{url= <<"#users">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"users">>}]},
    #li{body=[#link{url= <<"#products">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"products">>}]}
  ].

input()-> [
  #h3{body= <<"Add category">>},
    #panel{class=["row-fluid"], body=[#panel{class=[span8], body=[
    #textbox{id=cat_name, class=[span12], placeholder= <<"name">>},
    #textarea{id=cat_desc, class=[span12], placeholder= <<"description">>},
    #select{id=cat_scope, class=[], body=[
      #option{label= <<"scope">>, body = <<"scope">>, disabled=true, selected=true, style="display:none; color:gray;"},
      #option{label= <<"Public">>, value = public},
      #option{label= <<"Private">>, value = private}
    ]},
    #link{id=save_cat, class=[btn, "btn-large"], body=[#i{class=["icon-tags"]}, <<" Create">>], postback=save_cat, source=[cat_name, cat_desc, cat_scope]} 
    ]} ]} ].

categories()->[
  #h3{body= <<"Categories">>},
  #table{id=cats, class=[table, "table-hover"], body=[[#tr{cells=[#td{body=Id}, #td{body=Name}, #td{body=Desc}]} || G=#group{id=Id, name=Name, description=Desc}<-kvs:all(group)]]}
].

users()->[
  #h3{body= <<"Users">>} ].
products()->[
  #h3{body= <<"Products">>} ].

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


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(save_cat) ->
  Name = wf:q(cat_name),
  Desc = wf:q(cat_desc),
  Publicity = case wf:q(cat_scope) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
  error_logger:info_msg("Scope: ~p", [Publicity]),
  Creator = (wf:user())#user.email,
  RegData = #group{name = Name, description = Desc, scope = Publicity, creator = Creator, owner = Creator, feeds = ?GRP_CHUNK},

  case kvs_group:register(RegData) of
    {ok, G} ->
      msg:notify([kvs_group, group, init], [G#group.id, G#group.feeds]),

      wf:wire(wf:f("$('#cats > tbody:first').append('~s');", [wf:render(#tr{cells=[#td{body= G#group.id},#td{body=G#group.name}, #td{body=G#group.description}]})])),
      wf:wire("$('#cat_name').val('');$('#cat_desc').val('')");
    {error, _} -> skip
  end;
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

api_event(Name,Tag,Term) -> error_logger:info_msg("[admin]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([create],
                 [{Creator, Id, Name, Desc, Publicity}]) ->
  error_logger:info_msg("responce to create group"),
  ok;
process_delivery(_R, _M) -> skip.
