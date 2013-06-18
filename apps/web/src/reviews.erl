-module(reviews).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").

main()-> #dtl{file="dev", bindings=[{title,<<"reviews">>},{body, body()}]}.

body()->
 [].