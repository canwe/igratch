-define(ROOT, code:priv_dir(web)).
-define(PAGE_SIZE, 4).
-define(ID_TITLE(Id),   Id++"t").
-define(ID_DESC(Id),    Id++"d").
-define(ID_TOOL(Id),    Id++"e").
-define(ID_MEDIA(Id),   Id++"m").
-define(THUMB_SIZE, [{270, 124}, {200, 200}, {139, 80}, {1170, 380}]).

-record(struct,         {lst=[]}).
-record(info_more,      {entries, toolbar, category, fid, module, delegate}).
-record(product_figure, {?ELEMENT_BASE(product_ui), product}).
-record(product_row,    {?ELEMENT_BASE(product_ui), product}).
-record(product_cart,   {?ELEMENT_BASE(product_ui), product}).
-record(product_line,   {?ELEMENT_BASE(product_ui), product, meta, controls}).
-record(product_hero,   {?ELEMENT_BASE(product_ui), product}).
-record(product_entry,  {?ELEMENT_BASE(product_ui), entry, mode=brief, prod_id, category, controls=[]}).
-record(entry_media,    {?ELEMENT_BASE(product_ui), media, fid, cid, mode}).
-record(entry_comment,  {?ELEMENT_BASE(product_ui), comment}).
-record(feature_req,    {?ELEMENT_BASE(product_ui), entry}).

-record(input,          {?ELEMENT_BASE(controls), icon="icon-edit",
    recipients="",
    placeholder_rcp="",
    placeholder_ttl="",
    placeholder_box=""}).
