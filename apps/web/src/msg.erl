-module(msg).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

notify(EventPath, Data) -> wf:send(product_channel, {delivery, EventPath, Data}).

