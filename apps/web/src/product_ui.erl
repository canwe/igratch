-module(product_ui).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/products.hrl").
-include_lib("kvs/include/payments.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("kvs/include/feeds.hrl").
-include("records.hrl").

render_element(#product_hero{product=P}) ->
    Bought = lists:any(fun(#payment{product_id=PrId}) -> P#product.id==PrId end,
        case wf:user() of undefined -> []; #user{email=Email}-> kvs_payment:payments(Email) end),
    Hero = #panel{class=["row-fluid"], body=[
    #panel{class=[span6], body=[
    #panel{class=["hero-unit", "product-hero"], body=[
        #h2{body=P#product.title},
        #p{body=P#product.brief},
        #panel{body=#span{class=["game-rating"], body=[#span{class=["star"]} || _ <- lists:seq(1,5)]}},
        #panel{class=["btn-toolbar", "text-center"], body=[
            if Bought ->
                case lists:keyfind(bundles, 1, P#product.feeds) of false -> [];
                {_, Fid} ->
                    case kvs:entries(kvs:get(feed, Fid), entry, 1) of [] -> [];
                    [#entry{media=[#media{url=Url}]}] ->
                        error_logger:info_msg("URL: ~p", [Url]),
                        #link{class=[btn, "btn-large", "btn-success"],
                                body=[#i{class=["icon-windows", "icon-large"]},<<" download">>],
                                url=Url,
                                download=[filename:basename(Url)]} end end;
            true ->
                #button{class=[btn, "btn-large", "btn-inverse", "btn-info", "btn-buy", win],
                    body= [<<"buy for ">>, #span{body= "$"++ float_to_list(P#product.price/100, [{decimals, 2}]) }],
                    postback={add_cart, P}} end
        ]}
      ]}
    ]},
    #panel{class=[span6, "text-center"], body=[
      #image{image=P#product.cover}
    ]}
  ]},
  element_panel:render_element(Hero).

timestamp_label({0, _}, Time) ->
  {_, H} = calendar:now_to_local_time(Time),
  io_lib:format("~2..0b:~2..0b:~2..0b", tuple_to_list(H));
timestamp_label({Days, _}, _) when Days < 7 -> io_lib:format("~p " ++ "days ago", [Days]);
timestamp_label({Days, _}, _) when Days < 31 -> io_lib:format("~p " ++ "weeks ago", [trunc(Days/7)]);
timestamp_label({Days, _}, _) when Days < 365 -> io_lib:format("~p " ++ "months ago", [trunc(Days/30)]);
timestamp_label({Days, _}, _) when Days > 365 -> io_lib:format("~p " ++ "years ago", [trunc(Days/365)]);
timestamp_label({Days, _}, _) -> io_lib:format("~p days ago", [Days]).


shorten(undefined) -> <<"">>;
shorten(Input) when is_list(Input) -> shorten(list_to_binary(Input));
shorten(Input) when is_binary(Input) ->
    R = [{"<img[^>]*>", ""}, {"<p></p>", ""},
        {"<br[\\s+]/>", ""}, {"^\\s*", ""}, {"\n+$", ""}],

    lists:foldl(fun({Pt, Re}, Subj) ->
        re:replace(Subj, Pt, Re, [global, {return, binary}]) end, Input, R).

to_price(Str)->
  PriceStr2 = case string:to_float(Str) of {error, no_float} -> Str; {F, _} -> float_to_list(F, [{decimals, 2}]) end,
  {P1, Rest} = case string:to_integer(PriceStr2) of {error, no_integer} -> {0, "0"}; {Pa, [_|Ra]} -> {Pa, Ra}; {Pa, Ra} -> {Pa, Ra} end,
  P2 = case string:to_integer(Rest) of {error,no_integer}-> 0; {Re,_} -> Re  end,
  P2+P1*100.

to_date(undefined) -> to_date(now());
to_date(Date)->
  {{Y, M, D}, {H,Mi,_}} = calendar:now_to_datetime(Date),
  io_lib:format("~s ~p, ~p at ~p:~p", [element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}), D, Y, H,Mi]).
