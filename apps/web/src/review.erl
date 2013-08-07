-module(review).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").

main() ->
    case wf:user() of
        undefined -> wf:redirect("/login");
        _ ->  #dtl{file="prod", bindings=[{title,<<"review">>},{body, body()}]} end.

body() ->
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:all_by_index(entry, #entry.entry_id, case wf:qs(<<"id">>) of undefined -> -1; Id -> binary_to_list(Id) end) of [E|_] ->
      {{Y, M, D}, _} = calendar:now_to_datetime(E#entry.created),
      Date = io_lib:format(" ~p ~s ~p ", [D, element(M, {"Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"}), Y]),
      {From, Av} = case kvs:get(user, E#entry.from) of {ok, U} -> {U#user.display_name, U#user.avatar}; {error, _} -> {E#entry.from, <<"holder.js/150x150">>} end,
      #panel{class=["row-fluid"], body=[
        #panel{class=[span3], body=[
          #panel{class=[sidebar], body=[
            #panel{id="review-meta", class=["row-fluid"], body=[
              #h3{class=["blue capital"], body= <<"action">>},
              #image{class=["img-polaroid"], alt= <<"The author">>, image=Av, width="150"},
              #p{class=[username], body= #link{body=From}},
              #p{class=[datestamp], body=[Date]},
              #p{class=[statistics], body=[
                  #link{body=[ #i{class=["icon-eye-open", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"1024">>} ], postback={read_entry, E#entry.id}},
                  #link{body=[ #i{class=["icon-comments-alt", "icon-large"]}, #span{class=[badge, "badge-info"], body= <<"10">>} ], postback={read_entry, E#entry.id}}
              ]},
              #panel{class=[], body=[
                  #link{url= <<"#">>, class=[btn, "btn-orange", capital], body= <<"Buy it!">>}
              ]}
            ]}
          ]}
        ]},
        #panel{class=[span9], body=[
          #product_entry{entry=E, mode=full}
        ]}
      ]};
      [] -> index:error(<<"not_found">>) end }},
  #section{class=[section], body=#panel{class=[container], body=[
    #h3{body= <<"more reviews">>, class=[blue, offset3]},
    #panel{class=["row-fluid"], body=more_article()}
  ]}}
  ]++index:footer().

more_article() ->
  #panel{class=["game-article","shadow-fix"], body=[
    #panel{class=["game-article-inner", clearfix], body=[
      #panel{class=[span3, "article-meta"], body=[
        #h3{class=[blue, capital], body= <<"Action">>},
        #p{class=[username], body= <<"John Smith">>}
        #p{class=[datestamp], body=[ <<"Yesterday">>, #span{body= <<"1:00pm">>}]},
        #p{class=[statistics], body=[
          #i{class=["icon-user"]},
          #span{body=[1,045]},
          #i{class=["icon-comment"]},
          #span{body= <<"25">>}
        ]}
      ]},
      #panel{class=[span3, shadow], body=#image{class=["border"], alt= <<"Row Four Image">>, image= <<"/static/img/row4.jpg">>}},
      #panel{class=[span6, "article-text"], body=[
        #h3{class=["light-grey"], body= <<"Lorem ipsum dolor sit amet">>},
        #p{body=[<<"Duis bibendum tortor at ligula condimentum sed dignissim elit tincidunt. Aliquam luctus ornare tortor ac hendrerit. Nam arcu odio, pretium et cursus nec, tempus ac massa. Nam eleifend quam eu justo adipiscing id eleifend tortor ullamcorper... ">>,
          #link{url= <<"#">>, body= <<"Read">>}
        ]}
      ]}
    ]}
  ]}.

event(init) -> wf:reg(product_channel),[];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event({comment_entry, Eid, Cid, Csid, Parent, EditorId})->
  Comment = wf:q(Cid),
  Medias = case wf:session(medias) of undefined -> []; L -> L end,
  User = wf:user(),

  msg:notify([kvs_feed, entry, Eid, comment, product:uuid(), add], [User#user.email, Parent, Comment, Medias, Csid, EditorId]);

event({comment_reply, {Cid, {Eid, Fid}}})->
  CommentId = wf:temp_id(),
  PanelId =wf:temp_id(),
  User =wf:user(),
  wf:insert_bottom(Cid, #panel{id=PanelId, body=[
    #htmlbox{id=CommentId, root=?ROOT, dir="static/"++User#user.email, img_tool=gm},
    #panel{class=["btn-toolbar"], body=[
      #link{class=[btn, "btn-large", "btn-info"], body= <<"Post">>, postback={comment_entry, {Eid, Fid}, CommentId, Cid, Cid, PanelId}, source=[CommentId]},
      #link{class=[btn, "btn-large"], body= <<"Cancel">>, postback={comment_cancel, PanelId}}
    ]}
  ]});
event({comment_cancel, Id}) -> wf:remove(Id);

event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).

process_delivery([_, Eid, comment, Cid, add],
                 [From, Parent, Content, Medias, Csid, EditorId])->
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
  end;
process_delivery(_R, _M) -> skip.
