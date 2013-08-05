-module(review).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

main() -> #dtl{file="dev", bindings=[{title,<<"review">>},{body, body()}]}.

body() ->
  Id = wf:qs(<<"id">>),
  index:header()++[
  #section{class=[section], body=#panel{class=[container], body=
    case kvs:all_by_index(entry, #entry.entry_id, binary_to_list(Id)) of [E|_] -> #product_entry{entry=E}; [] -> index:error(<<"not_found">>) end }}
  ]++index:footer().

event(init) -> [];
event(Event) -> error_logger:info_msg("[review]event: ~p", [Event]), [].
api_event(Name,Tag,Term) -> error_logger:info_msg("[review]api_event ~p, Tag ~p, Term ~p",[Name,Tag,Term]).
