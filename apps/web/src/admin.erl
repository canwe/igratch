-module(admin).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/acls.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"admin">>},{body, body()}]}.

body() ->
  {AclEn, Acl} = acls(),
  index:header() ++ [
  #section{id=content, body=
    #panel{class=[container], body=
      #panel{class=[row, dashboard], body=[
        #panel{class=[span3], body=dashboard:sidebar_menu(wf:user(), wf:user(), admin, [#li{class=[divider]}, subnav() ])},
        #panel{class=[span9, "tab-content"], style="min-height:400px;", body=[
          #panel{class=["tab-content"], body=[
            #panel{id=categories, class=["tab-pane", active], body=[
              dashboard:section(input(), "icon-user"),
              dashboard:section(categories(), "icon-list")
            ]},
            #panel{id=acl, class=["tab-pane"], body=[
              dashboard:section(acl(Acl), "icon-male"),
              dashboard:section(acl_entry(AclEn), "icon-list")
            ]},
            #panel{id=users, class=["tab-pane"], body=[
              dashboard:section(users(), "icon-user")
            ]},
            #panel{id=products, class=["tab-pane"], body=[
              dashboard:section(products(), "icon-gamepad")
            ]}
          ]}
        ]} ]} } }
  ] ++ index:footer().

subnav() -> [
    #li{class=[active], body=[#link{url= <<"#categories">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"categories">>}]},
    #li{body=[#link{url= <<"#acl">>, data_fields=[{<<"data-toggle">>, <<"tab">>}], body= <<"acl">>}]},
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
  #table{id=cats, class=[table, "table-hover"],
    header=[#tr{cells=[#th{body= <<"id">>}, #th{body= <<"name">>}, #th{body= <<"description">>}, #th{body= <<"scope">>}]}],
    body=[[#tr{class=[case Scope of private -> "info"; _-> "" end],
      cells=[#td{body=Id}, #td{body=Name}, #td{body=Desc}, #td{body=atom_to_list(Scope)}]} || #group{id=Id, name=Name, description=Desc, scope=Scope}<-kvs:all(group)]]}
].

resources()->[
  #h3{class=[blue], body= <<"Resources">>},
  #table{class=[table, "table-hover"], body=[[
      #tr{cells=[#td{body= <<"category">>}]},
      #tr{cells=[#td{body= <<"user">>}]},
      #tr{cells=[#td{body= <<"product">>}]},
      #tr{cells=[#td{body= <<"feed">>}]}
      #tr{cells=[#td{body= <<"feature">>}]}
    ]]}
  ].

acl(Rows)->[
  #h3{class=[blue], body= <<"ACL">>},
  #table{class=[table, "table-hover"], header=[#tr{cells=[#th{body= <<"id">>}, #th{body= <<"resourse">>}]}], body=[Rows]}].

acl_entry(Panes)-> [#panel{class=["tab-content"], body=Panes}].

acls()->
  lists:mapfoldl(fun(#acl{id={R,N}=Aid, resource=Ar}, Ain) ->
    Id = io_lib:format("~p", [Aid]),
    B = #panel{id=atom_to_list(R)++atom_to_list(N), class=["tab-pane"], body=[
      #h3{class=[blue], body=[Id, " entries"]},
      #table{class=[table, "table-hover"], header=[#tr{cells=[#th{body= <<"id">>}, #th{body= <<"accessor">>}, #th{body= <<"action">>}]}], body=[[
        #tr{cells=[#td{body=io_lib:format("~p", [Ai])}, #td{body= Accessor}, #td{body= atom_to_list(Action)}]} || #acl_entry{id=Ai, accessor={user, Accessor}, action=Action} <- kvs_acl:entries(Aid)
      ]]}
    ]},
    Ao = [#tr{cells=[#td{body=#link{url="#"++atom_to_list(R)++atom_to_list(N), body=Id, data_fields=[{<<"data-toggle">>, <<"tab">>}]}}, #td{body=io_lib:format("~p", [Ar])}]}|Ain],
   {B , Ao}
  end, [], kvs:all(acl)).

users()->[
  #h3{body= <<"Users">>},
  #table{class=[table, "table-hover"],
    header=[#tr{cells=[#th{body= <<"email">>}, #th{body= <<"roles">>}]}],
    body=[[
      begin
        #tr{id=wf:temp_id(), postback={view, U#user.email},cells=[#td{body=U#user.email}, #td{body=[profile:features(wf:user(), U)]} ]}
      end|| U <- kvs:all(user)
    ]]}].

products()->[
  #h3{body= <<"Products">>},
  #table{class=[table, "table-hover"],
    header=[#tr{cells=[#th{body= <<"title">>}]}],
    body=[[
      begin
        #tr{cells=[#td{body=U#product.title} ]}
      end|| U <- kvs:all(product)
    ]]}].

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(save_cat) ->
  Name = wf:q(cat_name),
  Desc = wf:q(cat_desc),
  Publicity = case wf:q(cat_scope) of "scope" -> public; undefined -> public; S -> list_to_atom(S) end,
  Creator = (wf:user())#user.email,
  Id = case Publicity of private -> Name; _ -> undefined end,
  RegData = #group{id=Id, name = Name, description = Desc, scope = Publicity, creator = Creator, owner = Creator, feeds = ?GRP_CHUNK},

  case kvs_group:register(RegData) of
    {ok, G} ->
      msg:notify([kvs_group, group, init], [G#group.id, G#group.feeds]),

      wf:wire(wf:f("$('#cats > tbody:first').append('~s');", [wf:render(
        #tr{class=[case G#group.scope of private -> "info"; _-> "" end], cells=[
          #td{body= G#group.id}, #td{body=G#group.name}, #td{body=G#group.description}, #td{body=atom_to_list(G#group.scope)} ]} )])),
      wf:wire("$('#cat_name').val('');$('#cat_desc').val('')");
    {error, _} -> skip
  end;
event({view, Id}) -> error_logger:info_msg("redirect"), wf:redirect("/profile?id="++Id);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

api_event(Name,Tag,Term) -> error_logger:info_msg("[admin]api_event: Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([create],
                 [{Creator, Id, Name, Desc, Publicity}]) ->
  error_logger:info_msg("responce to create group"),
  ok;
process_delivery(_R, _M) -> skip.
