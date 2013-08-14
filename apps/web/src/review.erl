-module(review).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

main() -> #dtl{file="prod", bindings=[{title,<<"review">>},{body, body()}]}.

body() ->
  {_Tabs, Reviews} = reviews:reviews(),
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:all_by_index(entry, entry_id, case wf:qs(<<"id">>) of undefined -> -1; Id -> binary_to_list(Id) end) of [E|_] ->
      {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
      Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
      {From, Av} = case kvs:get(user, E#entry.from) of {ok, U} -> {U#user.display_name, U#user.avatar}; {error, _} -> {E#entry.from, <<"holder.js/150x150">>} end,
      #panel{class=["row-fluid", dashboard], body=[
        #panel{class=[span2], body=[
            #panel{id="review-meta", class=["row-fluid"], body=[
              #h3{class=["blue capital"], body= <<"action">>},
              #image{class=["img-polaroid"], alt= <<"The author">>, image=Av, width="150"},
              #p{class=[username], body= #link{body=From}},
              #p{class=[datestamp], body=[Date]},
              #p{class=[statistics], body=[
                  #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ], postback={read, entry, E#entry.id}},
                  #link{body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ], postback={read, entry, E#entry.id}}
              ]},
              #panel{class=["btn-toolbar", "text-center"], body=[
                #link{url= <<"#">>, class=[btn, "btn-large", "btn-warning"], body= <<"Buy it!">>}
              ]}
            ]}
        ]},
        #panel{class=[span10], body=[
          dashboard:section(#product_entry{entry=E, mode=full}, "icon-align-justify"),
          #h3{body= <<"more reviews">>, class=[blue, "text-center"]},
          #panel{class=["row-fluid"], body=reviews:all(Reviews)}
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
    #htmlbox{id=CommentId, root=?ROOT, dir=Dir, img_tool=gm, size=[{270, 124}, {200, 200} , {139, 80}]},
    #panel{class=["btn-toolbar"], body=[
      #link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, {Eid, Fid}, CommentId, Cid, Cid, PanelId}, source=[CommentId]},
      #link{class=[btn, "btn-large"], body= <<"Cancel">>, postback={comment_cancel, PanelId}}
    ]}
  ]});
event({comment_cancel, Id}) -> wf:remove(Id);
event({read, reviews, {Id,_}})-> wf:redirect("/review?id="++Id);
event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([_, Eid, comment, Cid, add],
                 [From, Parent, Content, Medias, Csid, EditorId])->
  error_logger:info_msg("comment add"),
  Entry = #entry_comment{comment=#comment{
      id={Cid, Eid},
      entry_id=Eid,
      comment_id=Cid,
      content=wf:js_escape(Content),
      media=Medias,
      parent=Parent,
      author_id=From,
      creation_time=erlang:now()}},
  wf:insert_bottom(Csid, Entry),
  case EditorId of
    "" -> wf:wire(wf:f("$('#~s').parent().find('.mce-content-body').html('');", [Csid]));
    _ ->  wf:remove(EditorId)
  end,
  wf:wire("Holder.run();");
process_delivery(_R, _M) -> skip.
