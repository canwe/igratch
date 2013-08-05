-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

-define(PAGE_SIZE, case wf:session(page_size) of list -> 2; _ -> 8 end).

main()-> #dtl{file="dev", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()-> index:header() ++ [
  #section{class=[section, "main-no-slider"], body=[
    #panel{class=[container], body=[
      [#panel{class=["row-fluid"], body=[
        #h3{body=Name},
        [#product_entry{entry=E} || E <- lists:reverse(kvs_feed:entries(Fid, undefined, 10))]
      ]}|| #group{feed=Fid, name=Name} <-kvs:all(group)],

      #panel{class=["row-fluid"], body=[
%        #table{id=products, class=[table, "table-hover"], body=[list_products(1)] }
      ]}
%      #panel{class=[pagination, "pagination-large","pagination-centered"],body=[ #list{id=pagination, body=pagination(1)} ]}
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


event(init) -> [];
event(<<"PING">>) -> [];
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.
