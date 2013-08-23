-module(gm).
-compile(export_all).

%
% -flatten  psd layers to png
%
make_thumb(Input, Width, Height, Output) ->
    error_logger:info_msg("Input ~p~n", [Input]),
    error_logger:info_msg("Output ~p~n", [Output]),
  W = integer_to_list(Width), H = integer_to_list(Height),
  Cmd = lists:concat(["gm convert \"", Input, "\"",
    " -background transparent -gravity center",
    " -extent ", W, "x", H,
    " -resize ", W, "x", H,
    " +profile \"*\" \"", Output, "\""]),
  case os:cmd(Cmd) of [] -> ok; Error -> error_logger:info_msg("No thumb. ~p failed.", [Cmd]), error_logger:info_msg("ERROR: ~p", [Error]),{error, Error} end.