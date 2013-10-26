-include_lib("kvs/include/kvs.hrl").

-define(ROOT, code:priv_dir(web)).
-define(CSS, "/static/css/igratch.css").
-define(LESS, "/static/igratch/igratch.less").
-define(REVIEWS_CSS, "/static/css/reviews.css").
-define(REVIEWS_LESS, "/static/igratch/reviews.less").
-define(BOOTSTRAP, "/static/js/bootstrap.min.js").
-define(REVIEWS_BOOTSTRAP, "/static/js/reviews-bs.min.js").
-define(INDEX_CSS, "/static/css/index.css").
-define(INDEX_LESS, "/static/igratch/index.less").
-define(INDEX_BOOTSTRAP, "/static/js/index-bs.min.js").
-define(PAGE_SIZE, 4).
-define(ID_COMMENTS(Id),Id++"c").
-define(ID_CM_COUNT(Id),Id++"cc").
-define(ID_FEED(Id),    wf:to_list(Id)++"es").
-define(ID_FEED_TOOL(Id),wf:to_list(Id)++"est").
-define(THUMB_SIZE,     [{139, 80}, {270, 124}, {180,180}, {200, 200}, {570, 570}, {716, 480}, {1170, 350}]).
%-define(CURRENCY,       [{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}]).
-define(CURRENCY,       [{<<"Dollar">>, <<"USD">>}]).
-define(BTN_INFO,       [btn, "btn-info"]).
-define(BTN_SUCCESS,    [btn, "btn-success"]).
-define(STACK_BASE,     ["icon-stack-base", "icon-circle"]).
-define(TOOLTIP,        [{<<"data-toggle">>,<<"tooltip">>}]).
-define(DATA_TAB,       [{<<"data-toggle">>,<<"tab">>}]).
-define(DATA_COLLAPSE,  [{<<"data-toggle">>, <<"collapse">>}, {<<"data-target">>, <<".nav-collapse">>}]).
-define(URL_PRODUCT(Id),"/product?id="++Id).
-define(MONTH(M),       element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"})).

-record(struct,         {lst=[]}).
-record(info_more,      {toolbar, category, fid, module, delegate, mode}).
-record(product_figure, {?ELEMENT_BASE(product_ui), product}).
-record(product_row,    {?ELEMENT_BASE(product_ui), product}).
-record(product_cart,   {?ELEMENT_BASE(product_ui), product}).
-record(product_line,   {?ELEMENT_BASE(product_ui), product, meta, controls}).
-record(product_hero,   {?ELEMENT_BASE(product_ui), product}).

-record(input,          {?ELEMENT_BASE(input),
                        state,
                        icon="icon-edit",
                        feed,
                        recipients="",
                        post_class=?BTN_INFO,
                        close_class=[btn],
                        cancel_class=[btn],
                        expand_class=?BTN_INFO}).

-record(feed_ui,        {?ELEMENT_BASE(feed_ui), state, icon="", icon_url, header=[], selection_ctl=[]}).
-record(feed_entry,     {?ELEMENT_BASE(feed_ui), entry, state}).
-record(row_entry,      {?ELEMENT_BASE(feed_ui), entry, state}).
-record(div_entry,      {?ELEMENT_BASE(feed_ui), entry, state}).
-record(entry_media,    {?ELEMENT_BASE(feed_ui), media, mode}).


-define(EN_ROW(Id),     wf:to_list(Id)++"row").
-define(EN_SEL(Id),     wf:to_list(Id)++"sel").
-define(EN_FROMSEL(Sel),lists:sublist(Sel,1, length(Sel) - length("sel"))).
-define(EN_MEDIA(Id),   wf:to_list(Id)++"media").
-define(EN_TITLE(Id),   wf:to_list(Id)++"t").
-define(EN_DESC(Id),    wf:to_list(Id)++"d").
-define(EN_TOOL(Id),    wf:to_list(Id)++"tb").
-define(EN_CM_COUNT(Id),wf:to_list(Id)++"cc").

-define(DIRECT_STATE(Id), ?FD_STATE(Id)#feed_state{
    view=direct,
    html_tag=panel,
    enable_selection=true,
    enable_traverse=true}).

-define(REVIEW_STATE(Id), ?FD_STATE(Id)#feed_state{
    view = review,
    html_tag= panel}).

-define(USR_CART(Id),   wf:to_list(erlang:phash2(Id))++"cart").
-define(USR_ORDER(Id),  wf:to_list(erlang:phash2(Id))++"order").
-define(FD_CHKOUT(Id),  wf:to_list(Id)++"chk").
