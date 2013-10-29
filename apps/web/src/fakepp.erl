-module(fakepp).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("records.hrl").

main()-> #dtl{file="prod", bindings=[{title,<<"fake">>},{body, body()},{css,?CSS},{less,?LESS},{bootstrap,?BOOTSTRAP}]}.

body() -> index:header() ++ [
  #section{body=#panel{class=[container], body=#panel{class=["row-fluid"], body=[
    #panel{class=[alert, "alert-info","alert-block"], body=[<<"Fake PayPal processing">>]},
    #panel{class=["btn-toolbar", "text-center"], body=[
      #link{class=[btn, "btn-success"], body= <<"approve">>, url="/checkout?status=approve"},
      #link{class=[btn, "btn-error"], body= <<"reject">>, url="/checkout?status=reject"}
    ]}
  ]} } } ] ++ index:footer().


event(init) -> wf:reg(?MAIN_CH), [];
event(Event) -> ok.
