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
                        icon="icon-edit",
                        collapsed=false,
                        show_recipients=true,
                        feed,
                        type,
                        recipients="",
                        placeholder_rcp="",
                        placeholder_ttl="Title",
                        placeholder_box="",
                        expand_btn=""}).
-record(ui_payload, {}).

-record(feed_entry,     {?ELEMENT_BASE(feed), entry, mode, category, controls=[], owner}).
-record(entry_comment,  {?ELEMENT_BASE(feed), comment}).

-record(feed2, {?ELEMENT_BASE(feed2), icon="", entry_type, container, container_id, page_size=?PAGE_SIZE, header=[], selection=false, entry_view, traverse_mode=true, table_mode=true}).
-record(feed_entry2, {?ELEMENT_BASE(feed2), entry, state, view}).

-record(feed_state, {
    view,
    entry_type,
    container,
    container_id,
    feed_title,
    selection,
    selectall_ctl,
    select_all,
    delete_btn,
    prev,
    next,
    entries,
    page_label,
    select_toolbar,
    feed_toolbar,
    more_toolbar,
    close,
    full, start,
    total,
    current,
    start_element,
    last_element,
    page_size,
    selected_key = selected }).

-record(input_state, {
    recipients,
    title,
    body,
    media
    }).
