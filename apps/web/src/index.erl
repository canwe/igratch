-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

main() -> 
%    case wf:user() of
%         undefined -> wf:redirect("login");
%         _ -> 
    Title = "Title",
    Body = "Body",
    [ #dtl{file = "dev", bindings=[{title,Title},{body,Body}]} ].

body() -> %% area of http handler
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    wf:wire(#api{name=apiOne,tag=d1}),
  [
    #span { body= <<"Your chatroom name: ">> }, 
    #textbox { id=userName, body= wf:user() }, #button{body="Logout",postback=logout}, #br{},
    #panel { id=chatHistory },
    #button{id=but,body= <<"Click Me!">>,postback=change_me},
    #button{id=replace,body= <<"Replace Body">>,postback=replace},
    "<a onclick=\"document.apiOne('Hello')\" name='1'>API</a>",
    #textbox { id=message },
    #button { id=sendButton, body= <<"Chat">>, postback={chat,Pid}, source=[userName,message] },
    #panel { id=n2ostatus }
 ].

header() -> [
  #panel{class=[navbar, "navbar-fixed-top", "sky-navbar"], body=[
    #panel{class=["navbar-inner"], body=[
      #panel{class=[container], body=[
        #link{class=[btn, "btn-navbar"], data_fields=[{<<"data-toggle">>, <<"collapse">>}, {<<"data-target">>, <<".nav-collapse">>}], url="javascript:void(0)",
          body=[#span{class=["icon-bar"]}||_I<-lists:seq(1,3)]},

        #h1{class=[brand], body=#link{url="/login", body= <<"iGrach">>, name="top" }},
        #panel{class=["nav-collapse", "collapse"], body=[
          #list{class=[nav], body=[
            #li{body=#link{url="/chat",body=[ #i{class=["fui-chat", "icon-comment"]}, #span{class=["badge badge-info"], body="10"} ]}},
            #li{body=#link{url="/chat?mode=mail",body=[ #i{class=["fui-mail", "icon-envelope"]}, #span{class=["badge badge-info"], body="21"} ]} },
            #li{body=#link{body=[ #i{class=["icon-search"]} ]}},
            #li{body=#link{body= <<"Home">>,url="#"}},
            #li{body=#link{body= <<"Games">>,url="/store2"}},
            #li{body=#link{body= <<"Review">>}}]},
          #list{class=["nav", "pull-right"], body=[
            #li{class=["dropdown"], body=[
              #link{class=["dropdown-toggle"], data_fields=[{<<"data-toggle">>, <<"dropdown">>}], url="javascript:void(0)", body=[
                case wf:user() of undefined -> <<"Log in">>; A -> A end,
                #b{class=["caret"]} ]} ,
                #list{class=["dropdown-menu"], body=[
                  #li{body=#link{body=[#i{class=["icon-cog", "fui-gear"]},  <<" Preferences">>]}},
                  #li{body=#link{postback=chat,body=[#i{class=["icon-cog", "fui-gear"]},  <<" Notifications">>]}},
                  case wf:user() of
                       undefined -> #li{body=#link{postback=to_login,body=[#i{class=["icon-off"]}, <<" Login">> ]}};
                       A -> #li{body=#link{postback=logout,body=[#i{class=["icon-off"]}, <<" Logout">> ]}} end ]} ]} ]} ]} ]} ]} ]} ].

footer()-> [
  #footer{class=["sky-footer"], body=
      #panel{class=[container],body=[
        #panel{class=["row-fluid"], body=[
          #panel{class=[span4], body=[
            #h3{body= <<"Contact us">>},
            #p{body = <<"Each line of code is driven by intention to make our world better and to avoid suffering and causes of suffering. We guarantee that. We believe that making good software is impossible without good intention. So keep calm with us for the sake of great good :-)">>},
            #list{class=[icons], body=[
              #li{body=[#i{class=["icon-envelope"]}, #link{url= <<"mailto:maxim@synrc.com">>, body= <<" Maksym Sokhatskyi">>}]},
              #li{body=[#i{class=["icon-envelope"]}, #link{url= <<"mailto:doxtop@synrc.com">>, body= <<" Andrii Zadorozhnii">>}]},
              #li{body=[#i{class=["icon-twitter"]}, #link{url= <<"https://twitter.com/5HT">>, body= <<" @5HT">>}]},
              #li{body=[#i{class=["icon-facebook"]}, #link{url= <<"https://www.facebook.com/synrc">>, body= <<" synrc">>}]},
              #li{body=[#i{class=["icon-google-plus"]}, #link{url= <<"https://plus.google.com/114626316186565874650">>, body= <<" synrc">>}]}
            ]}
          ]},
          #panel{class=[span4], body=[
            #h3{body= <<"Recent news">>},
            #list{class=[icons], body=[
              #li{body=[#p{body=[#i{class=["icon-twitter"]},<<" Never saw Windows Phone slowness even on slow and cheap phones.">>,#small{body=[<<"by ">>, #link{url= <<"https://twitter.com/5HT">>, body= <<"maxim">>}]} ]} ]},
              #li{body=[#p{body=[#i{class=["icon-twitter"]},<<" N2O now is more popular than Erlyvideo-old on Github #FuryN2O In Top #30 Erlang Projects with Rank #26">>, #small{body=[<<"by ">>, #link{url= <<"https://twitter.com/5HT">>, body= <<"maxim">>}]} ]} ]},
              #li{body=[#p{body=[#i{class=["icon-twitter"]},<<" KVS supports Mnesia and Riak out of the box #EKVS: https://github.com/synrc/kvs">>, #small{body=[<<"by ">>, #link{url= <<"https://twitter.com/5HT">>, body= <<"maxim">>}]} ]} ]}
            ]}
          ]},
          #panel{class=[span4], body=[
            #h3{body= <<"Latest posts">>},
            #list{class=["unstyled"], body=[
              #li{body=[#h4{body=[#link{url= <<"http://maxim.livejournal.com/409763.html">>, body= <<"DHARMA LICENSE">>}]}, #p{body=[#small{body= <<"May 12, 2013">>}]}]},
              #li{body=[#h4{body=[#link{url= <<"http://maxim.livejournal.com/407956.html">>, body= <<"Web Frameworks Great Congregation">>}]}, #p{body=[#small{body= <<"May 6, 2013">>}]}]},
              #li{body=[#h4{body=[#link{url= <<"http://maxim.livejournal.com/411719.html">>, body= <<"N2O FAQ">>}]}, #p{body=[#small{body= <<"June 8, 2013">>}]}]}
            ]}
          ]}
        ]},
        #hr{},
        #panel{class=["row-fluid"], body=[
          #panel{class=[span12], body=[
            #panel{class=["span8"], body=[
              #link{class=["btn btn-link"],body= <<"About">>},
              #link{class=["btn btn-link"],body= <<"Help">>},
              #link{class=["btn btn-link"],body= <<"Terms of Use">>},
              #link{class=["btn btn-link"],body= <<"Privacy">>},
              #link{class=["btn btn-link"],body= <<"RSS">>} ]},
            #panel{class=["span4"], body=[
              #link{class=["btn btn-link"],url="http://synrc.com", body=[
                #span{class=["label", "label-transparent"], body= <<"&copy;">>}, <<"synrc.com">>]}
            ]} ]} ]} ]}} ].


api_event(Name,Tag,Term) -> error_logger:info_msg("Name ~p, Tag ~p, Term ~p",[Name,Tag,Term]), event(change_me).

event(init) ->
  User = wf:user(),
   error_logger:info_msg("User: ~p",[User]),
  [ begin
          Terms = [ #span { body= [[208,188,208,176,208,186,209,129,208,184,208,188]] }, ": ",
                      #span { body=integer_to_list(N) }, #br{} ],
            wf:insert_bottom(chatHistory, Terms)
            end || N <- lists:seq(1,3) ];

event(change_me) ->
    wf:replace(but,
        #link{
            url= <<"http://erlang.org">>,
            body= <<"Here's Erlang">>,
            actions=#show{effect=fade}
        }
    );

event(replace) ->
    wf:wire(#redirect{url="hello",nodrop=false});

event(logout) -> wf:user(undefined), wf:redirect("login");
event(login) -> login:event(login);
event({chat,Pid}) -> %% area of websocket handler
    error_logger:info_msg("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
    Terms = [ #span { body= <<"Message sent">> }, #br{} ],
    wf:insert_bottom(chatHistory, Terms),
    wf:wire("$('#message').focus(); $('#message').select(); "),
    wf:reg(room),
    Pid ! {message, Username, Message};

event(Event) -> error_logger:info_msg("Event: ~p", [Event]).

chat_loop() -> %% background worker ala comet
    receive 
        {message, Username, Message} ->
            Terms = [ #span { body=Username }, ": ",
                      #span { body=Message }, #br{} ],
            wf:insert_bottom(chatHistory, Terms),
            wf:wire("$('#chatHistory').scrollTop = $('#chatHistory').scrollHeight;"),
            wf:flush(room); %% we flush to websocket process by key
        Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
