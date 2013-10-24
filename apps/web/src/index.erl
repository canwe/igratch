-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> #dtl{file = "prod", ext="dtl", bindings=[{title, <<"iGratch">>},{body, body()}]}.

body() -> 
    wf:wire(#api{name=tabshow}),
    wf:wire("$('a[data-toggle=\"tab\"]').on('shown', function(e){"
        "id=$(e.target).attr('href');"
        "if(id!='#all')$('a[href=\"#all\"').removeClass('text-warning');"
        "else $(e.target).parent().parent().find('.text-warning').removeClass('text-warning');"
        "$(e.target).addClass('text-warning').siblings().removeClass('text-warning');"
        "tabshow(id);});"),
    Tab = case wf:qs(<<"id">>) of undefined -> "all"; T ->  T end,
    wf:wire(io_lib:format("$(document).ready(function(){$('a[href=\"#~s\"]').addClass('text-warning').tab('show');});",[Tab])),

    header() ++ [
        #section{class=["container-fluid", featured], body=#panel{id=carousel, class=[container], body=featured()}},

        #section{class=["row-fluid"], body=[
            #panel{class=[container], body=[
                #panel{class=["row-fluid"], body=[
                    #panel{class=[span8, "tab-content"], body=[
                        [#panel{id=Id, class=["tab-pane"]} || #group{id=Id, scope=Scope} <- [#group{id=all,scope=public}|kvs:all(group)], Scope==public]
                    ]},
                    #aside{class=[span4], body=[
                        #panel{class=[sidebar], body=[
                            #panel{class=["row-fluid"], body=[
                                #h4{class=[blue], body= #link{url="#all", body= <<"TAGS">>, data_fields=[{<<"data-toggle">>, <<"tab">>}] }},
                                #p{class=[inline, tagcloud], body=[
                                    [#link{url="#"++Id, body=[<<" ">>,Name],
                                        data_fields=[{<<"data-toggle">>, <<"tab">>}, {<<"data-toggle">>, <<"tooltip">>}], title=Desc}
                                    || #group{id=Id, name=Name, description=Desc, scope=Scope}<-kvs:all(group), Scope==public] ]} ]},
                            #feed_ui{title= <<"Active discussion">>, icon="icon-comments-alt", class="comments-flat",
                                state=?FD_STATE(?FEED(comment))#feed_state{
                                    flat_mode=true,
                                    view=comment,
                                    entry_type=comment,
                                    entry_id=#comment.comment_id}} ]}]}]}]}]}] ++ footer().

feed("all")->
    State = ?FD_STATE(?FEED(entry))#feed_state{view=review, entry_id=#entry.entry_id, delegate=reviews},
    #feed_ui{title= <<"Reviews">>, icon="icon-tags", state=State};
feed(Group) ->
    case kvs:get(group, Group) of {error,_}->[];
    {ok, G}-> 
        {_, Id} = lists:keyfind(feed, 1, element(#iterator.feeds, G)),
        State = ?FD_STATE(Id)#feed_state{view=review, entry_id=#entry.entry_id, delegate=reviews},
        #feed_ui{title= G#group.name, icon="icon-tags", state=State} end.

featured() ->
  #carousel{class=["product-carousel"], items=case kvs:get(group, "featured") of
    {error, not_found} -> [];
    {ok, G} ->
      Ps = lists:flatten([ case kvs:get(product, Who) of {ok, P}->P; {error,_}-> [] end || #group_subscription{who=Who}<-kvs_group:members(G#group.name)]),
      [begin
        {Cover, Class} = case P#product.cover of
          undefined -> {<<"">>, ""};
          C -> 
            Ext = filename:extension(C),
            Name = filename:basename(C, Ext),
            Dir = filename:dirname(C),
            {filename:join([Dir, "thumbnail", Name++"_1170x350"++Ext]),""}
        end,
        [
          #panel{id=P#product.id, class=["slide"], body=[
            #h1{body=P#product.title},
            #image{class=[Class], image=Cover}
          ]},
          #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win, buy],
            body= [<<"Buy for ">>, #span{body= "$"++ float_to_list(P#product.price/100, [{decimals, 2}]) }],
            postback={add_cart, P, #feed_state{}}}
        ]
      end || P <- Ps]
  end, caption= #panel{class=["row-fluid"],body=[
%        box(50, 12.99, "btn-warning", "icon-windows"), box(50, 12.99, "btn-success", "icon-windows"),
%        box(50, 12.99, "btn-violet", "icon-windows"), box(50, 12.99, "btn-info", "icon-windows") 
    ]}} .

box(Discount, Price, ColorClass, IconClass)->
  #panel{class=[span3, box], body=#button{class=[btn, "btn-large", ColorClass], body=[
    #p{style="margin-left:-10px;margin-right:-10px;", body= <<"Lorem: Ipsum dolor sit amet">>},
    #p{class=[accent], body= list_to_binary(integer_to_list(Discount)++"% OFF")},
    #p{class=["row-fluid"], body=[
      #span{class=[IconClass, "pull-left"]}, #span{class=["pull-right"], body=[#span{class=["icon-usd"]},
        list_to_binary(io_lib:format("~.2f", [Price]))]} 
    ]} ]}}.

header() ->
    User = wf:user(),
    IsAdmin = case User of undefined -> false; _ -> kvs_acl:check_access(User#user.email, {feature, admin})==allow end,
    [
  #header{class=[navbar, "navbar-fixed-top", ighead], body=[
    #panel{class=["navbar-inner"], body=[
      #panel{class=["container"], body=[
        #button{class=[btn, "btn-navbar"], data_fields=?DATA_COLLAPSE, body=[#span{class=["icon-bar"]}||_<-lists:seq(1,3)]},

        #link{url="/index", class=[brand], body=[ #image{alt= <<"iGratch">>, image= <<"/static/img/logo.png">>, width= <<"235px">>, height= <<"60px">>} ]},
        #panel{class=["nav-collapse", collapse], body=[
          #list{class=[nav, "pull-right"], body=[
            #li{body=#link{body= <<"Home">>, url= <<"/index">>}},
            #li{body=#link{body= <<"Games">>,url= <<"/store">>}},
            #li{body=#link{body= <<"Reviews">>, url= <<"/reviews">>}},
            case wf:user() of
              undefined -> #li{body=#link{body= <<"Sign In">>, url= <<"/login">>}};
              User -> [
                #li{body=#link{body= <<"Account">>, url= <<"/profile">>}},
                #li{body=[#link{body=[#span{id=?USR_CART(User#user.id), class=["cart-number"],
                    body= case lists:keyfind(cart, 1, User#user.feeds) of false -> <<"">>;
                        {_, Id} -> case kvs:get(feed,Id) of {error,_} -> <<"">>;
                        {ok, #feed{entries_count=C}} when C==0 -> <<"">>;
                        {ok, #feed{entries_count=C}} -> integer_to_list(C) end end},
                    #span{class=["icon-stack"], title= <<"shopping cart">>, body=[
                        #i{class=["icon-check-empty","icon-stack-base"]},
                        #i{class=["icon-shopping-cart"]}]}], url= <<"/shopping_cart">>} ]},

                #li{body=[
                  #link{class=["dropdown-toggle", "profile-picture"], data_fields=[{<<"data-toggle">>, <<"dropdown">>}],
                    body=#image{class=["img-circle", "img-polaroid"],
                        image = case User#user.avatar of undefined -> "/holder.js/50x50";
                            Img -> iolist_to_binary([Img,"?sz=50&width=50&height=50&s=50"]) end,
                        width= <<"45px">>, height= <<"45px">>}},
                  #list{class=["dropdown-menu"], body=[
                    #li{body=#link{url="/profile",  body=[#i{class=["icon-user", "icon-large"]}, <<" Profile">>]}},
                    #li{body=#link{url="/myreviews",body=[#i{class=["icon-list", "icon-large"]}, <<" Reviews">>]}},
                    #li{body=#link{url="/mygames",  body=[#i{class=["icon-gamepad", "icon-large"]}, <<" Games">>]}},
                    #li{body=#link{url="/notifications", body=[#i{class=["icon-envelope", "icon-large"]}, <<" Notifications">>]}},
                    #li{body=#link{url="/shopping_cart", body=[#i{class=["icon-shopping-cart", "icon-large"]}, <<" Shopping Cart">>]}},
                    if IsAdmin ->
                        #li{body=#link{url="/admin", body=[#i{class=["icon-cog", "icon-large"]}, <<" Admin">>]}};
                    true -> [] end,
                    #li{body=#link{id=logoutbtn, postback=logout, delegate=login, 
                        body=[#i{class=["icon-off", "icon-large"]}, <<" Logout">> ] }}
                  ]}]}
                ]
            end
          ]} ]} ]} ]} ]} ].

footer() -> [
  #footer{class=[igfoot],body=#panel{class=[container, "text-center"], body=[
    #panel{body=[
      #image{image= <<"/static/img/footer-highlight.png">>},
      #image{image= <<"/static/img/footer-shadow.png">>}
    ]},
    #panel{body=[
      #list{class=[icons, inline], body=[
        #li{body=#link{body= <<"About">>}},
        #li{body=#link{body= <<"Help">>}},
        #li{body=#link{body= <<"Terms of Use">>}},
        #li{body=#link{body= <<"Privacy">>}},
        #li{body= <<"&copy; iGratch 2013">>}
      ]},
      #list{class=[icons, inline], body=[
        #li{body=#link{body=[#i{class=["icon-youtube",      "icon-2x"]}]}},
        #li{body=#link{body=[#i{class=["icon-facebook",     "icon-2x"]}]}},
        #li{body=#link{body=[#i{class=["icon-google-plus",  "icon-2x"]}]}},
        #li{body=#link{body=[#i{class=["icon-twitter",      "icon-2x"]}]}},
        #li{body=#link{body=[#i{class=["icon-pinterest",    "icon-2x"]}]}},
        #li{body=#link{body=[#i{class=["icon-envelope-alt", "icon-2x"]}]}} ]} ]} ]}}].

success(Msg)-> alert(Msg, "alert-success").
error(Msg)  -> alert(Msg,"alert-danger").
info(Msg)   -> alert(Msg,"alert-info").
warn(Msg)   -> alert(Msg,"alert-warning").
alert(Msg, Class)->
    #panel{class=[alert, Class, "alert-block", fade, in], body=[
    #link{class=[close], url="#", data_fields=[{<<"data-dismiss">>,<<"alert">>}], body= <<"&times;">>}, #strong{body= Msg} ]}.

api_event(tabshow,Args,_) ->
    [Id|_] = string:tokens(Args,"\"#"),
    wf:update(list_to_atom(Id), feed(Id)),
    wf:wire("Holder.run();");
api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({read,_, {Id,_}})-> wf:redirect("/review?id="++Id);
event({read,_, Id})-> wf:redirect("/review?id="++Id);
event({add_cart, P, S}) ->
    store:event({add_cart, P, S}),
    wf:redirect("/shopping_cart");
event(Event) -> error_logger:info_msg("[index]Event: ~p", [Event]).

process_delivery([_Id, join,  G], {}) when G=="featured"-> wf:update(carousel, featured());
process_delivery([_Id, leave, G], {}) when G=="featured"-> wf:update(carousel, featured());
process_delivery(R,M) -> 
    %error_logger:info_msg("[index] delivery -> feed | ~p", [R]),
    feed_ui:process_delivery(R,M).
