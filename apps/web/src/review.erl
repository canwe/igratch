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
  Entries = lists:filter(fun(#entry{to=To})-> case To of {product, _}-> true; _-> false end end, kvs:all_by_index(entry, entry_id, case wf:qs(<<"id">>) of undefined -> -1; Id -> binary_to_list(Id) end)),
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case Entries of [E=#entry{id=Eid, to={product, Prid}}|_] ->
        Product = case kvs:get(product, Prid) of {error,_}-> #product{}; {ok, P}-> P end,
        error_logger:info_msg("Product review: ~p", [Product]),
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
            {ok, G}-> #feed_view{owner=G, feed=feed, title= "More "++ G#group.name ++" reviews", mode=review, icon="icon-tags"} end
            || #group_subscription{where=Group} <- kvs_group:participate(Product#product.id)]
        ]}
      ]};
      [] -> index:error(<<"not_found">>) end }},
  #section{class=[section], body=#panel{class=[container, "text-center"], body=[
  ]}} ]++index:footer().

event(init) -> wf:reg(?MAIN_CH),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({comment_entry, Eid, Cid, Csid, Parent, EditorId})->
  Comment = wf:q(Cid),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  From = case wf:user() of undefined -> "anonymous"; User -> User#user.email end,

  msg:notify([kvs_feed, entry, Eid, comment, kvs:uuid(), add], [From, Parent, Comment, Medias, Csid, EditorId]);

event({comment_reply, {Cid, {Eid, Fid}}})->
  CommentId = wf:temp_id(),
  PanelId =wf:temp_id(),
  Dir = "static/" ++case wf:user() of undefined -> "anonymous"; User -> User#user.email end,
  wf:insert_bottom(Cid, #panel{id=PanelId, body=[
    #htmlbox{id=CommentId, root=?ROOT, dir=Dir, img_tool=gm, size=?THUMB_SIZE},
    #panel{class=["btn-toolbar"], body=[
      #link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, {Eid, Fid}, CommentId, Cid, Cid, PanelId}, source=[CommentId]},
      #link{class=[btn, "btn-large"], body= <<"Cancel">>, postback={comment_cancel, PanelId}}
    ]}
  ]});
event({comment_cancel, Id}) -> wf:remove(Id);
event({read, _, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([_, {EntryId,_}=Eid, comment, Cid, add],
                 [From, Parent, Content, Medias, Csid, EditorId]) ->
  Entry = #entry_comment{comment=#comment{
      id={Cid, Eid},
      entry_id=Eid,
      comment_id=Cid,
      content=wf:js_escape(Content),
      media=Medias,
      parent=Parent,
      from=From,
      created=erlang:now()}},
  wf:insert_bottom(Csid, Entry),
  case EditorId of
    "" -> wf:wire(wf:f("$('#~s').parent().find('.mce-content-body').html('');", [Csid]));
    _ ->  wf:remove(EditorId)
  end,
    wf:wire(wf:f("$('.~s').html('~s');", [?ID_CM_COUNT(EntryId), integer_to_list(kvs_feed:comments_count(entry, Eid)) ])),
    wf:wire("Holder.run();");
process_delivery(R,M) -> feed:process_delivery(R,M).
