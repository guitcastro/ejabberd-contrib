%% @author guilhermetorres

-module(mod_meetapp).

-behaviour(gen_mod).

-export([start/2,
     init/2,
     stop/1,
     send_notice/3,
     handle_out_subscription/4,
     handle_in_subscription/6]).

-define(PROCNAME, ?MODULE).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").


start(Host, Opts) ->
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

%% noinspection ErlangUnresolvedFunction
init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    %ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, handle_in_subscription, 10),
    %ejabberd_hooks:add(roster_out_subscription, Host, ?MODULE, handle_out_subscription, 10),        
    ok.

%% noinspection ErlangUnresolvedFunction
stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host,
              ?MODULE, send_notice, 10),
    %ejabberd_hooks:delete(roster_in_subscription, Host,
    %          ?MODULE, handle_in_subscription, 10),
    %ejabberd_hooks:delete(roster_out_subscription, Host,
    %          ?MODULE, handle_out_subscription, 10),                            
    ok.
    
handle_in_subscription(Acc, User, Server, JID, SubscriptionType, _Reason) ->
  To = jlib:make_jid(User, Server, <<"">>),
  Item = get_roster_item(JID,To),

  if (Item /= none) ->
    Match = in_state_change(Item#roster.subscription,Item#roster.ask,SubscriptionType),
    maybe_send_match_msg(JID, To, Item#roster.name,Match);
    true ->
      ok
  end,

  Acc.

handle_out_subscription(User, Server, JID, SubscriptionType) ->
  To = jlib:make_jid(User, Server, <<"">>),
  Item = get_roster_item(JID,To),

  if (Item /= none) ->
    Match = out_state_change(Item#roster.subscription,Item#roster.ask,SubscriptionType),
    maybe_send_match_msg(JID, To, Item#roster.name,Match);
    true ->
      ok
  end,
  ok.

maybe_send_match_msg(JID, To, To_name,true) ->
  send_match_msg(JID,To, To_name);
maybe_send_match_msg(_JID, _To, _To_name,false) ->
  ok.

send_match_msg(JID,To,To_name) ->
  ?INFO_MSG("Match To ~p", [To_name]),
  Body = <<" and you like each other!">>,
  MSG = <<To_name/binary, Body/binary>>,
  ejabberd_router:route(JID, To,
    #xmlel{name = <<"message">>,
      attrs = [{<<"type">>, <<"match">>}],
      children =
      [#xmlel{name = <<"subject">>,
        attrs = [],
        children =
        [{xmlcdata, <<"match">>}]},
        #xmlel{name = <<"body">>,
          attrs = [],
          children =
          [{xmlcdata, MSG}]}]}).


in_state_change(none, out, subscribe) -> true;
in_state_change(from, none, subscribed) -> true;
in_state_change(from, out, subscribed) -> true;
in_state_change(_, _, _) -> false.

out_state_change(none, in, subscribe) -> true;
out_state_change(to, none, subscribed) -> true;
out_state_change(to, in, subscribed) -> true;
out_state_change(_, _, _) -> false.

%% noinspection ErlangUnresolvedFunction
send_notice(From, To, Packet) ->
  Server = To#jid.lserver,
  User = To#jid.luser,

  ModOffline = get_offlinemsg_module(Server),
  QueueLen = integer_to_list(get_offlinemsg_length(ModOffline, User, Server) + 1),
  Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
  Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
  MSG = create_msg_body(Body, From, Type),
  if 
     MSG /= none ->
        send_push(Server, User, From#jid.luser, MSG, QueueLen);
     true ->
        none
  end.

create_msg_body(Body, From, <<"chat">>) ->
  Nickname = get_nick_name(From),
  Separator = <<": ">>,
  _MSG = <<Nickname/binary, Separator/binary, Body/binary>>,
  create_msg_body(_MSG,[]);

create_msg_body(Body, _From , <<"match">>) ->
  create_msg_body(Body,[]);
  
%% ignore any other msg type  
create_msg_body(_Body, _From , _type) ->
  none.  

create_msg_body(Body, []) ->
  binary_to_list(Body).

get_nick_name(From) ->
  ModVCard = get_vcard_module(From#jid.lserver),
  IQr = get_from_vcard(ModVCard, From),
  [Vcard] = IQr#iq.sub_el,
  xml:get_path_s(Vcard,[{elem, <<"NICKNAME">>}, cdata]).


    
create_post(Server, Subscriber, From, Msg )-> 
    Service = gen_mod:get_module_opt(Server, ?MODULE, service, fun(S) -> iolist_to_binary(S) end, <<"">>),
    Sep = "&",
    ["subscriber=", binary_to_list(Subscriber), Sep,
     "from=", binary_to_list(From), Sep,
     "msg=", url_encode(Msg), Sep,
     "service=", binary_to_list(Service)].
            
create_post(Server, Subscriber, From, Msg, Badge )-> 
    Post = create_post(Server, Subscriber, From, Msg), 
    Post ++ ["&","badge=",Badge].
                
%send_push(Server, Subscriber, From, Msg) ->   
%    Post = create_post(Server, Subscriber, From, Msg),
%    send_push(Server, Post).
    
send_push(Server, Subscriber, From, Msg, Badge )->
    Post = create_post(Server, Subscriber, From, Msg, Badge),
    send_push(Server, Post).
    
send_push(Server, Post) ->
    PostUrl = gen_mod:get_module_opt(Server, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, <<"">>),
    
    ?INFO_MSG("Sending post request ~p~n",[Post] ),
    Response = httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
    ?INFO_MSG("Response ~p",[Response]),
    ok.    

match_from(From,{LUser, _LServer , _ }) ->
    match_from(From,{LUser, _LServer});  
match_from(From,{LUser, LServer }) ->
    if 
       (From#jid.luser == LUser) and (From#jid.lserver == LServer)->
           true;
        true -> 
           false
     end.    
    
get_roster_item(From, To) ->
    Items = mod_roster:get_roster(To#jid.luser, To#jid.lserver),
    ItemsFiltered = lists:filter(
                        fun (Item) ->
                            match_from(From,Item#roster.jid) end,Items),
    case ItemsFiltered of
      [] -> none;
      [I] -> I
    end.
    	       
get_offlinemsg_length(ModOffline, User, Server) ->
    case ModOffline of
      none -> <<"disabled">>;
      _ ->
     ModOffline:get_queue_length(User, Server)
    end.

get_offlinemsg_module(Server) ->
    case gen_mod:is_loaded(Server, mod_offline) of
      true -> mod_offline;
      false -> none
    end.

get_from_vcard(ModVCard, User) ->
    case ModVCard of
      none -> <<"disabled">>;
      _ ->
         %JID = {jid, User, Server, "", User, Server, ""},
         IQ = #iq{type = get, xmlns = ?NS_VCARD},
         ModVCard:process_sm_iq(User, User, IQ)
    end.

get_vcard_module(Server) ->
    case gen_mod:is_loaded(Server, mod_vcard) of
      true ->   mod_vcard;
      false -> none
    end.
    
%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).


