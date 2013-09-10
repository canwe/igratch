-include_lib("kvs/include/kvs.hrl").

-define(ROOT, code:priv_dir(web)).
-define(PAGE_SIZE, 4).
-define(ID_TITLE(Id),   Id++"t").
-define(ID_DESC(Id),    Id++"d").
-define(ID_TOOL(Id),    Id++"e").
-define(ID_MEDIA(Id),   Id++"m").
-define(ID_COMMENTS(Id),Id++"c").
-define(ID_CM_COUNT(Id),Id++"cc").
-define(ID_FEED(Id),    wf:to_list(Id)++"es").
-define(ID_FEED_TOOL(Id),    wf:to_list(Id)++"est").
-define(THUMB_SIZE,     [{139, 80}, {270, 124}, {200, 200}, {570, 570}, {1170, 350}]).
-define(CURRENCY,       [{<<"Dollar">>, <<"USD">>}, {<<"Euro">>, <<"EUR">>}, {<<"Frank">>, <<"CHF">>}]).
-define(BTN_INFO,       [btn, "btn-info"]).
-define(BTN_SUCCESS,    [btn, "btn-success"]).
-define(STACK_BASE,     ["icon-stack-base", "icon-circle"]).
-define(TOOLTIP,        [{<<"data-toggle">>,<<"tooltip">>}]).
-define(URL_PRODUCT(Id),"/product?id="++Id).

-record(struct,         {lst=[]}).
-record(info_more,      {toolbar, category, fid, module, delegate, mode}).
-record(product_figure, {?ELEMENT_BASE(product_ui), product}).
-record(product_row,    {?ELEMENT_BASE(product_ui), product}).
-record(product_cart,   {?ELEMENT_BASE(product_ui), product}).
-record(product_line,   {?ELEMENT_BASE(product_ui), product, meta, controls}).
-record(product_hero,   {?ELEMENT_BASE(product_ui), product}).
-record(entry_media,    {?ELEMENT_BASE(product_ui), media, fid, cid, mode}).

-record(input,          {?ELEMENT_BASE(input),
                        state,
                        feed_state,
                        icon="icon-edit",
                        collapsed=false,
                        feed,
                        recipients="",
                        placeholder_rcp="",
                        placeholder_ttl="Title",
                        placeholder_box="",
                        expand_btn="",
                        expand_class=?BTN_INFO}).


-record(feed2,          {?ELEMENT_BASE(feed2),
                        state,
                        icon = "",
                        header = [],
                        selection = false,
                        traverse_mode = true }).

-record(feed_entry2,    {?ELEMENT_BASE(feed2), entry, state}).

-record(feed_state, {
    view,
    mode            = table,
    entry_type      = entry,
    entry_id        = #iterator.id,
    container       = feed,
    container_id,
    feed_title      = wf:temp_id(),
    selection,
    selectall_ctl   = wf:temp_id(),
    select_all      = wf:temp_id(),
    delete_btn      = wf:temp_id(),
    prev            = wf:temp_id(),
    next            = wf:temp_id(),
    entries         = wf:temp_id(),
    page_label      = wf:temp_id(),
    select_toolbar  = wf:temp_id(),
    feed_toolbar    = wf:temp_id(),
    more_toolbar    = wf:temp_id(),
    close           = wf:temp_id(),
    full,
    start,
    total,
    current,
    start_element,
    last_element,
    page_size = ?PAGE_SIZE,
    selected_key = selected,
    recipients=[]}).

-record(input_state, {
    id          = wf:temp_id(),
    form_id     = wf:temp_id(),
    toolbar_id  = wf:temp_id(),
    recipients_id = wf:temp_id(),
    title_id    = wf:temp_id(),
    body_id     = wf:temp_id(),
    media_id    = wf:temp_id(),
    price_id    = wf:temp_id(),
    currency_id = wf:temp_id(),
    alert_id    = wf:temp_id(),
    upload_id   = wf:temp_id(),
    post_id     = wf:temp_id(),
    entry_type  = entry,
    show_recipients=true,
    show_title = true,
    show_media = true,
    groups = [],
    recipients = [] }).

-define(FD_TITLE(Id),       wf:to_list(Id)++"ft").
-define(FD_SELLALLCTL(Id),  wf:to_list(Id)++"sallctl").
-define(FD_SELLALL(Id),     wf:to_list(Id)++"all").
-define(FD_ENTRS(Id),       wf:to_list(Id)++"es").
-define(FD_STATE(Id), #feed_state{
        container_id    = Id,
        feed_title      = ?FD_TITLE(Id),
        selectall_ctl   = ?FD_SELLALLCTL(Id),
        select_all      = ?FD_SELLALL(Id),
        entries         = ?FD_ENTRS(Id)
    }).
-define(FD_STATE(Id, S), S#feed_state{
        container_id    = Id,
        feed_title      = ?FD_TITLE(Id),
        selectall_ctl   = ?FD_SELLALLCTL(Id),
        select_all      = ?FD_SELLALL(Id),
        entries         = ?FD_ENTRS(Id)
    }).

-define(EN_ROW(Id),     wf:to_list(Id)++"row").
-define(EN_SEL(Id),     wf:to_list(Id)++"sel").
-define(EN_MEDIA(Id),   wf:to_list(Id)++"media").
-define(EN_TITLE(Id),   wf:to_list(Id)++"t").
-define(EN_DESC(Id),    wf:to_list(Id)++"d").
-define(EN_TOOL(Id),    wf:to_list(Id)++"tb").

