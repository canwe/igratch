-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include_lib("feed_server/include/records.hrl").
-include("records.hrl").
-define(LOGIN,[facebook,google,twitter]).

main() ->
  avz:callbacks(?LOGIN),
  [#dtl{file = "prod", ext="dtl", bindings=[{title,<<"Login">>},{body, body()}]} ].

body() ->
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
                    #panel{class=["btn-toolbar", "text-center"], body=[avz:buttons(?LOGIN)]},
                    #h3{class=["text-center"], body= <<"or">>},
                    #panel{class=["control-group"], body=[
                        #label{class=["control-label"], for=user, body= <<"Email">>},
                        #panel{class=[controls], body=[
                            #panel{class=["input-prepend"], body=[
                                #span{class=["add-on"], body=#i{class=["icon-user"]}},
                                #textbox{id=user, placeholder= <<"e-mail">>, data_fields=[{<<"data-original-title">>, <<"">>}]}  ]} ]} ]},

                    #panel{class=["control-group"], body=[
                        #label{class=["control-label"], for=pass, body= <<"Password">>},
                        #panel{class=[controls], body=[
                            #panel{class=["input-prepend"], body=[
                                #span{class=["add-on"], body=#i{class=["icon-lock"]}},
                                #password{id=pass, data_fields=[{<<"data-original-title">>, <<"">>}]} ]} ]} ]} ]},

                #panel{class=["modal-footer"], body=[
                    #link{class=["pull-left", "link-forgot", disabled], tabindex="-1", body= <<"forgot password?">>}, avz:buttons([email]) ]} ]} ]} ]} ]} ]} ] ++ index:footer() ++ avz:sdk(?LOGIN).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(login) -> avz:login(email, [{<<"email">>, list_to_binary(wf:q(user))}, {<<"password">>, wf:q(pass)}]);
event({login, #user{}=User}) ->
    {ok, U} = kvs:get(user, User#user.email),
    Av = case string:str(wf:to_list(U#user.avatar), "static/"++U#user.email) of
        0 -> User#user.avatar; _ -> U#user.avatar end,
    msg:notify([kvs_user, login, user, User#user.email, update_status], {}),
    avz:login_user(User#user{avatar=Av});
event({register, #user{}=User}) -> msg:notify([kvs_user, user, register], [User#user{feeds=?USR_CHUNK}, #input_state{pid=pid_to_list(self())}, #feed_state{}]);
event(X) -> error_logger:info_msg("Event: ~p", [X]), avz:event(X).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).

process_delivery([user, login], {{ok,U},_,_}) -> event({login, U});
process_delivery(_,_) -> skip.
