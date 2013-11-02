-module(login).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/users.hrl").
-include("records.hrl").
-define(LOGIN,[facebook,google,twitter]).

main() ->
  avz:callbacks(?LOGIN),
  [#dtl{file = "prod", ext="dtl", 
        bindings=[{title,<<"Login">>},{body, body()},{css,?LOGIN_CSS},{less,?LESS},{bootstrap,?LOGIN_BOOTSTRAP}]} ].

body() ->
    index:header() ++ [
    #panel{id="content", role="main", class=["theme-pattern-lightmesh", alt], body=[
    #section{class=[section], body=[
    #panel{class=[container], body=[
        #panel{class=[modal, "modal-login"], body=[
            #panel{class=["form-horizontal"], body=[
                #h3{class=["modal-header"], body= <<"Log in to your account">>} ,

                #panel{class=["modal-body"], body=[
                    #panel{id=messages, body=[]},
                    #h3{class=["text-center"], body= <<"Sign in with">>},
                    #panel{class=["btn-toolbar", "text-center"], body=[avz:buttons(?LOGIN)]},
                    #h3{class=["text-center"], body= <<"or">>},
                    #panel{class=["control-group"], body=[
                        #label{class=["control-label"], for=user, body= <<"Email">>},
                        #panel{class=[controls], body=[
                            #panel{class=["input-prepend"], body=[
                                #span{class=["add-on","icon-user"]},
                                #textbox{id=user, placeholder= <<"e-mail">>}  ]} ]} ]},

                    #panel{class=["control-group"], body=[
                        #label{class=["control-label"], for=pass, body= <<"Password">>},
                        #panel{class=[controls], body=[
                            #panel{class=["input-prepend"], body=[
                                #span{class=["add-on", "icon-lock"]},
                                #password{id=pass} ]} ]} ]} ]},

                #panel{class=["modal-footer"], body=[avz:buttons([email]) ]} ]} 
        ]} ]} ]} ]}
    ] ++ index:footer() ++ avz:sdk(?LOGIN).

event(init) -> wf:reg(?MAIN_CH), [];
event({delivery, [_|Route], Msg}) -> process_delivery(Route, Msg);
event(login) -> avz:login(email, [{<<"email">>, list_to_binary(wf:q(user))}, {<<"password">>, wf:q(pass)}]);
event({login, #user{}=User}) ->
    {ok, U} = kvs:get(user, User#user.email),
    Av = case string:str(wf:to_list(U#user.avatar), "static/"++U#user.email) of
        0 -> User#user.avatar; _ -> U#user.avatar end,
    msg:notify([kvs_user, login, user, User#user.email, update_status], {}),
    avz:login_user(User#user{avatar=Av});
event({register, #user{}=User}) ->
    msg:notify([kvs_user, user, create], [User#user{feeds=?USR_CHUNK}]);
event(X) -> error_logger:info_msg("Event: ~p", [X]), avz:event(X).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).

process_delivery([user, created], [#user{}=U]) -> event({login, U});
process_delivery([user, created], [{error,E}]) -> wf:update(messages, index:error(E));
process_delivery(_,_) -> skip.
