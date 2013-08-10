-module(msg).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("records.hrl").

notify(EventPath, Data) -> wf:send(?MAIN_CH, {delivery, EventPath, Data}).

