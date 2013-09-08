-module(review).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"review">>},{body, body()}]}.

body() ->
  Entries = lists:filter(fun(#entry{to=To})-> case To of {product, _}-> true; _-> false end end,
    kvs:all_by_index(entry, entry_id, case wf:qs(<<"id">>) of undefined -> -1; Id -> binary_to_list(Id) end)),

  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case Entries of [E=#entry{id=Eid, to={product, Prid}}|_] ->
        Product = case kvs:get(product, Prid) of {error,_}-> #product{}; {ok, P}-> P end,
%        error_logger:info_msg("Product review: ~p", [Product]),
      #panel{class=["row-fluid", dashboard], body=[
        #panel{class=[span2], body=[
            dashboard:section(profile:profile_info(wf:user(), E#entry.from, ""), "icon-user"),

            dashboard:section([
                #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Article">>},
                #p{class=[datestamp], body=[product_ui:to_date(E#entry.created)]},
                #p{class=[statistics], body=[
                  #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"?">>} ],
                    postback={read, entry, E#entry.id}},
                  #link{body=[ #i{class=["icon-comments-alt", "icon-large"]}, 
                    #span{class=[badge, "badge-info", ?ID_CM_COUNT(E#entry.entry_id)], body=[ list_to_binary(integer_to_list(kvs_feed:comments_count(entry, Eid)))] } 
                   ], postback={read, entry, E#entry.id}}
                ]}
            ], "icon-eye-open"),

            dashboard:section([
                #h3{class=[blue], body= <<"&nbsp;&nbsp;&nbsp;&nbsp;Game">>},
                #h4{body=[Product#product.title]},
                #p{body=[Product#product.brief]},

                #panel{class=["btn-toolbar", "text-center"], body=[
                  #button{class=[btn, "btn-inverse", "btn-info", "btn-buy"],
                    body= [<<"buy for ">>, #span{body= "$"++ float_to_list(Product#product.price/100, [{decimals, 2}]) }], postback={checkout, Product#product.id}},
                  #button{class=[btn, "btn-warning"], body= [#span{class=["icon-shopping-cart"]}, <<" add to cart ">>], postback={add_cart, Product}}
                ]}
            ], "icon-gamepad")
        ]},

        #panel{class=[span10], body=[
          dashboard:section(#feed_entry{entry=E, mode=detached}, "icon-file-text-alt"),

          [ case kvs:get(group, Group) of {error,_}->[];
            {ok, G}->
                {_, Gid} = lists:keyfind(feed, 1, element(#iterator.feeds, G)),
                #feed2{title= "More "++ G#group.name ++" reviews", icon="icon-tags", entry_type=entry, container=feed, container_id=Gid, selection=false, entry_view=review, table_mode=false}
                end
            || #group_subscription{where=Group} <- kvs_group:participate(Product#product.id)]
        ]}
      ]};
      [] -> index:error(<<"not_found">>) end }},
  #section{class=[section], body=#panel{class=[container, "text-center"], body=[
  ]}} ]++index:footer().

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({comment_entry, {Eid,_}, CFid, Csid, Parent, EditorId}) ->
    Comment = wf:q(CFid),
    Medias = case wf:session(medias) of undefined -> []; L -> L end,
    From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
%    error_logger:info_msg("=>comment entry ~p parent: ~p", [Eid, Parent]),

    Recipients = lists:map(fun(#entry{to={RoutingType, To}, feed_id=Fid, feeds=Feeds}) ->
        {_, CsFid} = lists:keyfind(comments, 1, Feeds),
        CommentFid = case Parent of undefined -> CsFid;
            Pid -> case kvs:get(comment, {Pid, {Eid, Fid}}) of {error,_}-> CsFid;
                {ok, C} -> case lists:keyfind(comments, 1, C#comment.feeds) of false -> CsFid; {_, PCid} -> PCid end end end,

        {RoutingType, To, {Eid, Fid, CommentFid}}
        end,
        [E || #entry{}=E <- kvs:all_by_index(entry, entry_id, Eid)]),
    %Recipients = lists:append(R1,lists:usort(R2)),
    error_logger:info_msg("Recipients: ~p", [Recipients]),
    Cid = kvs:uuid(),
    Created = now(),

    C = #comment{id = {Cid, {Eid, ?FEED(entry)}},
                from = From,
                comment_id = Cid,
                entry_id = {Eid, ?FEED(entry)},
                feed_id = ?FEED(comment),
                content = wf:js_escape(Comment),
                media = Medias,
                feeds=[{comments, kvs_feed:create()}],
                created = Created },

    [msg:notify([kvs_feed, RoutingType, To, comment, Cid, add],
        [#comment{id= {Cid, {EntryId, EntryFid}},
            from = From,
            comment_id = Cid,
            entry_id = {EntryId,EntryFid},
            feed_id = CommentsFid,
            content = wf:js_escape(Comment),
            media = Medias,
            feeds=[{comments, kvs_feed:create()}],
            created = Created }, Csid, EditorId])

        || {RoutingType, To, {EntryId, EntryFid, CommentsFid}} <- Recipients],

      msg:notify([kvs_feed, comment, register], [C, Csid, EditorId]);

event({comment_reply, {Cid, {Eid, Fid}}, CFid })->
  CommentId = wf:temp_id(),
  PanelId =wf:temp_id(),
  Dir = "static/" ++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:insert_before(CFid, #panel{id=PanelId, body=[
    #htmlbox{id=CommentId, root=?ROOT, dir=Dir, img_tool=gm, size=?THUMB_SIZE},
    #panel{class=["btn-toolbar"], body=[
      #link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, {Eid, Fid}, CommentId, Cid, Cid, PanelId}, source=[CommentId]},
      #link{class=[btn, "btn-large"], body= <<"Cancel">>, postback={comment_cancel, PanelId}}
    ]}
  ]});
event({comment_cancel, Id}) -> wf:remove(Id);
event({read, _, {Id,_}})-> wf:redirect("/review?id="++Id);
event({read, _, Id})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

%process_delivery([_,_,comment,_,add], [#comment{feed_id=undefined},_,_]) -> skip;
%process_delivery([comment,registered],_) -> skip;
process_delivery(R,M) -> error_logger:info_msg("[review] review -> feed | ~p", [R]),feed:process_delivery(R,M).
