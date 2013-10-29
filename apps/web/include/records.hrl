-include_lib("feed_server/include/records.hrl").

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
-define(LOGIN_BOOTSTRAP, "/static/js/login-bs.min.js").
-define(PAGE_SIZE, 4).
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

-define(USR_CART(Id),   wf:to_list(erlang:phash2(Id))++"cart").
-define(USR_ORDER(Id),  wf:to_list(erlang:phash2(Id))++"order").
-define(FD_CHKOUT(Id),  wf:to_list(Id)++"chk").
