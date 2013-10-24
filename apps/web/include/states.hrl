-include_lib("kvs/include/kvs.hrl").
%-include_lib("feed_server/include/records.hrl").

% My Games

-define(MYGAMES_FEED(Id), ?FD_STATE(Id)#feed_state{view=product,
                                                   html_tag=panel,
                                                   entry_id=#entry.entry_id,
                                                   enable_selection=true,
                                                   delegate=mygames}).

-define(MYGAMES_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                        fid = Id,
                                        role=group,
                                        entry_type=product,
                                        show_upload=true,
                                        show_price=true,
                                        simple_body=true,
                                        class= "feed-table-header",
                                        control_title = <<"Create">>,
                                        post_btn = <<"create">>,
                                        placeholder_rcp= <<"Categories">>,
                                        placeholder_ttl= <<"Game title">>,
                                        placeholder_box= <<"Brief description">> }).

% Store

-define(STORE_FEED(Id), ?FD_STATE(Id)#feed_state{view=store,
                                                 enable_selection=false,
                                                 delegate=store}).

-define(PRODUCTS_FEED, ?STORE_FEED(?FEED(product))#feed_state{entry_id = #product.id, entry_type=product}).

% Admin

-define(GROUPS_FEED, ?FD_STATE(?FEED(group))#feed_state{entry_type=group,
                                                        enable_selection=true,
                                                        enable_traverse=true,
                                                        html_tag=table}).

-define(GROUPS_INPUT, #input_state{ id=?FD_INPUT(?FEED(group)),
                                    fid = ?FEED(group),
                                    entry_type=group,
                                    control_title= <<"Add category">>,
                                    placeholder_ttl= <<"name">>, 
                                    placeholder_box= <<"description">>,
                                    show_recipients=false,
                                    show_scope=true,
                                    show_media=false,
                                    simple_body=true}).

-define(ACL_FEED(Id), ?FD_STATE(Id)#feed_state{ container=acl,
                                                entry_type=acl_entry,
                                                html_tag=table,
                                                enable_selection=false,
                                                enable_traverse=true}).

-define(USERS_FEED, ?FD_STATE(?FEED(user))#feed_state{entry_type=user,
                                                      entry_id=#user.username,
                                                      html_tag=table,
                                                      enable_traverse=true}).

-define(PRODUCTS_VIEW_FEED, ?FD_STATE(?FEED(product))#feed_state{entry_type=product,
                                                                 enable_selection=true,
                                                                 enable_traverse=true,
                                                                 html_tag=table}).
