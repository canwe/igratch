-module(db_update).
-compile(export_all).
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/groups.hrl").
-include_lib("kvs/include/feeds.hrl").

update() ->
  Ps = [  #product{
        id=Id,
        ext_id=ExtId,
        vendor_id=Vendor_id,
        categories=Categories,
        creator=Creator,
        owner=Owner,
        feeds=Feeds,
        title=Title,
        brief=Brief,
        cover=Cover,
        publish_start_date = Publish_start_date,
        publish_end_date = Publish_end_date,
        price = Price,
        currency = Currency,
        retailer_price = Retailer_price,
        our_price= Our_price,
        fee = undefined,
        enabled=Enabled_on_site,
        for_sale= true,
        creation_date=Creation_date,
        modify_date=Modify_date
    }
  ||
    {product,
        Id,
        ExtId,
        Vendor_id,
        Categories,
        Creator,
        Owner,
        Feeds,
        Title,
        Brief,
        Cover,
        Publish_start_date,
        Publish_end_date,
        Currency,
        Price,
        Retailer_price,
        Our_price,
        Enabled_on_site,
        Creation_date,
        Modify_date} <- kvs:all(product)],
  [kvs:put(P) || P<-Ps].

