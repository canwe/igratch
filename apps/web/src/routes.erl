-module (routes).
-author('Maxim Sokhatsky').
-behaviour (route_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
%    error_logger:info_msg("Routes path: ~p", [Path]),
    {Module, PathInfo} = route(Path),
    {ok, State, Ctx#context{path=PathInfo,module=Module}}.

route(<<"/">>) -> {index, []};
route(<<"/index">>) -> {index, []};
route(<<"/admin">>) -> {admin, []};
route(<<"/product">>) -> {product, []};
route(<<"/login">>) -> {login, []};
route(<<"/chat">>) -> {chat, []};
route(<<"/store">>) -> {store, []};
route(<<"/shopping_cart">>) -> {shopping_cart, []};
route(<<"/checkout">>) -> {checkout, []};
route(<<"/fakepp">>) -> {fakepp, []};
route(<<"/review">>) -> {review, []};
route(<<"/reviews">>) -> {reviews, []};
route(<<"/notifications">>) -> {notifications, []};
route(<<"/profile">>) -> {profile, []};
route(<<"/mygames">>) -> {mygames, []};
route(<<"/myreviews">>) -> {myreviews, []};
route(<<"/ws/">>) -> {index, []};
route(<<"/ws/admin">>) -> {admin, []};
route(<<"/ws/index">>) -> {index, []};
route(<<"/ws/product">>) -> {product, []};
route(<<"/ws/login">>) -> {login, []};
route(<<"/ws/chat">>) -> {chat, []};
route(<<"/ws/store">>) -> {store, []};
route(<<"/ws/shopping_cart">>) -> {shopping_cart, []};
route(<<"/ws/checkout">>) -> {checkout, []};
route(<<"/ws/fakepp">>) -> {fakepp, []};
route(<<"/ws/review">>) -> {review, []};
route(<<"/ws/reviews">>) -> {reviews, []};
route(<<"/ws/notifications">>) -> {notifications, []};
route(<<"/ws/profile">>) -> {profile, []};
route(<<"/ws/mygames">>) -> {mygames, []};
route(<<"/ws/myreviews">>) -> {myreviews, []};
route(<<"/favicon.ico">>) -> {static_file, []};
route(_) -> {index, []}.

