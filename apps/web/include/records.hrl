-define(ROOT, code:priv_dir(web)).
-define(MAIN_CH, games_ch).
-define(USR_CHUNK, [feed, direct, products, starred, pinned]).
-define(GRP_CHUNK, [feed, products]).
-define(PRD_CHUNK, [features, specs, gallery, videos, reviews, news, bundles]).
-define(PAGE_SIZE, 4).

-record(struct, {lst=[]}).
-record(info_more, {entries, toolbar, category, fid, module, delegate}).
-record(product_figure,{?ELEMENT_BASE(product_ui), product}).
-record(product_row,   {?ELEMENT_BASE(product_ui), product}).
-record(product_line,  {?ELEMENT_BASE(product_ui), product, meta, controls}).
-record(product_hero,  {?ELEMENT_BASE(product_ui), product}).
-record(product_entry, {?ELEMENT_BASE(product_ui), entry, mode=brief, prod_id, category}).
-record(entry_media,   {?ELEMENT_BASE(product_ui), media, fid, cid, mode}).
-record(entry_comment, {?ELEMENT_BASE(product_ui), comment}).

