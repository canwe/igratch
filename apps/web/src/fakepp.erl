-module(fakepp).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"fake">>},{body, body()}]}.

body() -> index:header() ++ [
  #section{body=#panel{class=[container], body=#panel{class=["row-fluid"], body=[
    #panel{class=[alert, "alert-info","alert-block"], body=[<<"Fake PayPal processing">>]},
    #panel{class=["btn-toolbar", "text-center"], body=[
      #link{class=[btn, "btn-large", "btn-success"], body= <<"approve">>, url="/checkout?status=approve"},
      #link{class=[btn, "btn-large", "btn-error"], body= <<"reject">>, url="/checkout?status=reject"}
    ]}
  ]} } } ] ++ index:footer().


event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(Event) -> error_logger:info_msg("Page event: ~p", [Event]), ok.

process_delivery(_R, _M) -> skip.
