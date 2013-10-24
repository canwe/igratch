-include_lib("kvs/include/kvs.hrl").
%-include_lib("feed_server/include/records.hrl").

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