-include_lib("kvs/include/kvs.hrl").
%-include_lib("feed_server/include/records.hrl").

% My Games

-define(MYGAMES_FEED(Id), ?FD_STATE(Id)#feed_state{view=product,
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
% My reviews
-define(MYREVIEWS_FEED(Id), ?FD_STATE(Id)#feed_state{view=review,
                                                     %entry_id=#entry.entry_id,
                                                     enable_selection=true,
                                                     delegate=reviews}).

-define(MYREVIEWS_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                          fid = Id,
                                          role=product,
                                          entry_type=review,
                                          control_title= <<"Submit review">>,
                                          placeholder_rcp= <<"Games">>,
                                          class= "feed-table-header"}).

% Index 

-define(ACTIVE_FEED, ?FD_STATE(?FEED(comment))#feed_state{  flat_mode=true,
                                                            view=comment,
                                                            entry_type=comment,
                                                            entry_id=#comment.comment_id}).

% Store

-define(STORE_FEED(Id), ?FD_STATE(Id)#feed_state{view=store, enable_selection=false, delegate=store}).

-define(PRODUCTS_FEED, ?STORE_FEED(?FEED(product))#feed_state{entry_id = #product.id, entry_type=product}).

% Reviews

-define(REVIEWS_FEED(Id), ?FD_STATE(Id)#feed_state{view=review, entry_id=#entry.entry_id, delegate=reviews}).

-define(ENTRIES_FEED, ?FD_STATE(?FEED(entry))#feed_state{view=review, entry_id=#entry.entry_id, delegate=reviews}).

% Review
-define(DETACHED_FEED(Id), ?FD_STATE(Id)#feed_state{view=detached, entry_id=#entry.entry_id, delegate=review}).

-define(COMMENTS_FEED(Id), ?FD_STATE(Id)#feed_state{view=comment,
                                                    entry_type=comment,
                                                    entry_id=#comment.comment_id,
                                                    show_title=false,
                                                    delegate=review}).

-define(COMMENTS_INPUT(Id, Recipients), #input_state{id=?FD_INPUT(Id),
                                                     fid = Id,
                                                     recipients=Recipients,
                                                     entry_type = comment,
                                                     show_recipients=false,
                                                     show_title = false,
                                                     show_media = false,
                                                     control_title = <<"Add your comment">>,
                                                     class = "comments-form"}).

-define(REPLY_INPUT(Id, Recipients), #input_state{id=?FD_INPUT(Id),
                                                  fid = Id,
                                                  recipients= Recipients,
                                                  entry_type = comment,
                                                  show_recipients = false,
                                                  show_title = false,
                                                  show_media = false,
                                                  collapsed = true,
                                                  post_collapse = true,
                                                  role=comment,
                                                  class= "comment-reply",
                                                  expand_btn= [<<"reply">>, #i{class=["icon-reply"]}]}).

% Cart

-define(CART_STATE(Id), ?FD_STATE(Id)#feed_state{view=cart,
                                                enable_selection=true,
                                                delegate=shopping_cart,
                                                delegate_sel=shopping_cart}).

% Product

-define(BLOG_STATE(Id), ?FD_STATE(Id)#feed_state{view=blog, html_tag=panel, delegate=product}).

-define(BLOG_INPUT(Id), #input_state{id=?FD_INPUT(Id),
                                    fid = Id,
                                    role=product,
                                    placeholder_ttl= <<"Title">>,
                                    class= "feed-table-header",
                                    show_recipients=false,
                                    collapsed=true}).

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
