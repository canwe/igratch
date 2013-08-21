-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").

-define(HTTP_ADDRESS, case application:get_env(web, http_address) of {ok, A} -> A; _ -> "" end).
-define(FB_APP_ID, case application:get_env(web, fb_id) of {ok, Id} -> Id; _-> "" end).
-define(GPLUS_CLIENT_ID, case application:get_env(web, gplus_client_id) of {ok, Id} -> Id; _-> "" end).
-define(GPLUS_COOKIE_POLICY, case application:get_env(web, gplus_cookiepolicy) of {ok, P} -> P; _-> "" end).

main() ->
  twitter_callback(),
  [#dtl{file = "prod", ext="dtl", bindings=[{title,<<"Login">>},{body, body()}]} ].

body() ->
  wf:session(name, kvs:uuname()),
  index:header() ++ [
  #panel{id="content", role="main", class=["theme-pattern-lightmesh", alt], body=[
  #section{class=[section], id=promo, body=[
  #panel{class=[container], body=[
    #panel{class=[modal, "modal-login"], body=[ 
      #panel{class=["form-horizontal"], id=loginform, body=[
        #panel{class=["modal-header"], body=[
          #button{class=[close], data_fields=[{<<"data-dismiss">>,<<"modal">>}], aria_states=[{<<"aria-hidden">>, <<"true">>}], body= <<"&times;">>},
          #h3{body= <<"Log in to your account">>} ]},
        #panel{class=["modal-body"], body=[
          #panel{id=messages, body=[]},
          #h3{class=["text-center"], body= <<"Sign in with">>},
          #panel{class=["btn-toolbar", "text-center"], body=[
            login_btn(twitter),
            login_btn(facebook),
            login_btn(google) ]},
          #h3{class=["text-center"], body= <<"or">>},
          #panel{class=["control-group"], body=[
            #label{class=["control-label"], for=user, body= <<"Email">>},
            #panel{class=[controls], body=[
              #panel{class=["input-prepend"], body=[
                #span{class=["add-on"], body=#i{class=["icon-user"]}},
                #textbox{id=user, placeholder= <<"e-mail">>, data_fields=[{<<"data-original-title">>, <<"">>}]}
              ]}
            ]}
          ]},
          #panel{class=["control-group"], body=[
            #label{class=["control-label"], for=pass, body= <<"Password">>},
            #panel{class=[controls], body=[
              #panel{class=["input-prepend"], body=[
                #span{class=["add-on"], body=#i{class=["icon-lock"]}},
                #password{id=pass, data_fields=[{<<"data-original-title">>, <<"">>}]}
              ]}
            ]}
          ]},
          #panel{id=wf:session(name)}

        ]},
        #panel{class=["modal-footer"], body=[
          #link{class=["pull-left", "link-forgot", disabled], body= <<"forgot password?">>},
          #button{id=login, class=[btn, "btn-info", "btn-large"], body= <<"Sign in">>, postback=login, source=[user,pass]}
        ]} ]} ]} ]} ]} ]} ] 
    ++ index:footer() ++ [
      facebook_sdk(),
      gplus_sdk()
    ].

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(logout) -> wf:user(undefined), wf:redirect("/login");
event(login) -> login(email, [{<<"email">>, list_to_binary(wf:q(user))}, {<<"password">>, wf:q(pass)}]);
event(Ev) -> error_logger:info_msg("Event ~p",[Ev]), ok.

api_event(plusLogin, Args, _)-> JSArgs = n2o_json:decode(Args), login(googleplus_id, JSArgs#struct.lst);
api_event(fbLogin, Args, _Term)-> JSArgs = n2o_json:decode(Args), login(facebook_id, JSArgs#struct.lst);
api_event(Name,Tag,_Term) -> error_logger:info_msg("Login Name ~p~n, Tag ~p~n",[Name,Tag]).

login_user(User) ->
  error_logger:info_msg("Loin: ~p ", [User]),
  wf:user(User),
  msg:notify([kvs_user, login, user, User#user.email, update_status], {}),
  wf:redirect("/account").
login(Key, Args)-> case Args of [{error, E}|_Rest] -> error_logger:info_msg("oauth error: ~p", [E]);
    _ -> case kvs:get(user,email_prop(Args,Key)) of
              {ok,Existed} -> RegData = registration_data(Args, Key, Existed), login_user(RegData);
              {error,_} -> RegData = registration_data(Args, Key, #user{feeds=?USR_CHUNK, username=wf:session(name)}),
                  msg:notify([kvs_user, user, register], [RegData]) end end.
%                  case kvs_user:register(RegData) of
%                      {ok, U} -> msg:notify([kvs_user, user, init], [U#user.email, U#user.feeds]), login_user(U);
%                      {error, E} -> error_logger:info_msg("error: ~p", [E]) end end end.

process_delivery([user, registered], {ok,_,U}) -> login_user(U);
process_delivery([user, registered], {error, Id, E}) ->
  Name = wf:session(name),
  if Id == Name -> wf:update(wf:session(name), index:error(atom_to_list(E))); true-> ok end;
process_delivery(_,_) -> skip.

twitter_callback()->
  Token = wf:q(<<"oauth_token">>),
  Verifier =wf:q(<<"oauth_verifier">>),
  case wf:user() of
    undefined ->
      if (Token /= undefined) andalso ( Verifier/= undefined) ->
        case tw_utils:get_access_token(binary_to_list(Token), binary_to_list(Verifier)) of
          not_authorized -> skip;
          Props ->
            UserData = tw_utils:show(Props),
            login(twitter_id, UserData#struct.lst)
        end;
        true -> skip
      end;
    _ -> skip
  end.

registration_data(Props, facebook_id, Ori)->
  Id = proplists:get_value(<<"id">>, Props),
  UserName = binary_to_list(proplists:get_value(<<"username">>, Props)),
  BirthDay = case proplists:get_value(<<"birthday">>, Props) of
    undefined -> {1, 1, 1970};
    BD -> list_to_tuple([list_to_integer(X) || X <- string:tokens(binary_to_list(BD), "/")])
  end,
  Ori#user{
    display_name = UserName,
    avatar = "https://graph.facebook.com/" ++ UserName ++ "/picture",
    email = email_prop(Props, facebook_id),
    name = proplists:get_value(<<"first_name">>, Props),
    surname = proplists:get_value(<<"last_name">>, Props),
    facebook_id = Id,
    age = {element(3, BirthDay), element(1, BirthDay), element(2, BirthDay)},
    register_date = erlang:now(),
    status = ok
  };
registration_data(Props, googleplus_id, Ori)->
  Id = proplists:get_value(<<"id">>, Props),
  Name = proplists:get_value(<<"name">>, Props),
  GivenName = proplists:get_value(<<"givenName">>, Name#struct.lst),
  FamilyName = proplists:get_value(<<"familyName">>, Name#struct.lst),
  Image = proplists:get_value(<<"image">>, Props),
  Ori#user{
    display_name = proplists:get_value(<<"displayName">>, Props),
    avatar = lists:nth(1,string:tokens(binary_to_list(proplists:get_value(<<"url">>, Image#struct.lst)), "?")),
    email = email_prop(Props,googleplus_id),
    name = GivenName,
    surname = FamilyName,
    googleplus_id = Id,
    register_date = erlang:now(),
    sex = proplists:get_value(gender, Props),
    status = ok
  };
registration_data(Props, twitter_id, Ori)->
  Id = proplists:get_value(<<"id_str">>, Props),
  Ori#user{
    display_name = proplists:get_value(<<"screen_name">>, Props),
    avatar = proplists:get_value(<<"profile_image_url">>, Props),
    name = proplists:get_value(<<"name">>, Props),
    email = email_prop(Props,twitter_id),
    surname = [],
    twitter_id = Id,
    register_date = erlang:now(),
    status = ok
  };
registration_data(Props, email, Ori)->
  Email = binary_to_list(proplists:get_value(<<"email">>, Props)),
  Ori#user{
    display_name = Email,
    email = Email,
    register_date = now(),
    status = ok,
    password = proplists:get_value(<<"password">>,Props)
  }.

email_prop(Props, twitter_id) -> binary_to_list(proplists:get_value(<<"screen_name">>, Props)) ++ "@twitter.com";
email_prop(Props, _) -> binary_to_list(proplists:get_value(<<"email">>, Props)).

login_btn(google)-> #panel{id=plusloginbtn, class=["btn-group"], body=
  #link{class=[btn, "btn-google-plus", "btn-large"], body=[#i{class=["icon-google-plus", "icon-large"]}, <<"Google">>] }};
login_btn(facebook)-> #panel{class=["btn-group"], body=
  #link{id=loginfb, class=[btn, "btn-primary", "btn-large"], body=[#i{class=["icon-facebook", "icon-large"]}, <<"Facebook">>],  actions= "$('#loginfb').on('click', fb_login);" }};
login_btn(twitter) ->
  case tw_utils:get_request_token() of
    {RequestToken, _, _} -> #panel{class=["btn-group"], body=
      #link{id=twlogin, class=[btn, "btn-info", "btn-large"], body=[#i{class=["icon-twitter", "icon-large"]}, <<"Twitter">>], url=tw_utils:authenticate_url(RequestToken)}};
    {error, R} -> error_logger:info_msg("Twitter request failed:", [R]), []
  end.

gplus_sdk()->
  wf:wire(#api{name=plusLogin, tag=plus}),
  #dtl{bind_script=false, file="google_sdk", ext="dtl", bindings=[
    {loginbtnid, plusloginbtn},
    {clientid, ?GPLUS_CLIENT_ID},
    {cookiepolicy, ?GPLUS_COOKIE_POLICY}
  ]}.

facebook_sdk()->
  wf:wire(#api{name=setFbIframe, tag=fb}),
  wf:wire(#api{name=fbAutoLogin, tag=fb}),
  wf:wire(#api{name=fbLogin, tag=fb}),
  [#panel{id="fb-root"},
  #dtl{bind_script=false, file="facebook_sdk", ext="dtl", bindings=[
    {appid, ?FB_APP_ID},
    {channelUrl, ?HTTP_ADDRESS ++ "/static/channel.html"}
  ]}].
