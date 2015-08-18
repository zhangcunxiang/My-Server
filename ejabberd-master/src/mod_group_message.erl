%% @author Archer
%% @doc @todo Add description to mod_group_message.


-module(mod_group_message).

-behaviour(gen_mod).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1,get_group_msg/3,receive_packet/4,
		 process_iq/3,pop_offline_group_message/3,get_group_offline_msg_len/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("information.hrl").

-define(NS_GET_MSG_STATE,<<"jabber:iq:msg:state">>).
-define(NS_GROUP_READ_RECEIPT,<<"jabber:iq_group_receipt">>).

%% copy from mod_group
-record(group_user,{gid  ::integer()|'_' ,
					us = {<<"">>,<<"">>} ,
					nick :: binary(),
					rtype :: mod_group:rtype() | '_',  
%% relation_type ask: request| allow: in | invited: be invieted | exit : sign out |kicked : kick out |forbidden  forbiddened
					ask = <<"">>,
					role :: atom() | '_',  
%% role of group    asker | (member | creater | admin) | invited | exiter | kicked   %% if role is kicked then forbidden his request
					last_action :: {mod_group:rtype(),mod_group:usors()}|'_'    
%% last action : ask by us | allow by us| invited by us| exit by us | kicked by us
				   }).

-record(session, {sid, usr, us, priority, info}).
-record(group_msg,{msgid ::binary(),
				   gid  ::integer(),
				   expire :: erlang:timestamp(),
				   sender :: jid(),
				   timestamp :: erlang:timestamp(),
				   packet :: #xmlel{},
				   sendernick :: binary()
				   }).


-record(gread_state,{msgid :: binary(),
				   receiver :: {binary(),binary()},
				   readstate ::integer()
				  }).

%% ====================================================================
%% Internal functions
%% ====================================================================
start(Host,Opts) ->
	io:format("start mod_group_message ...", []),
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	case gen_mod:db_type(Opts) of
		mnesia ->
			mnesia:create_table(group_msg,
								[{disc_copies, [node()]},
								 {record_name,group_msg},
								 {attributes,record_info(fields,group_msg)}
								 ]),
			mnesia:create_table(gread_state, 
								[{disc_only_copies,[node()]},
								 {type,bag},
								 {attributes,record_info(fields,gread_state)}
								]),
			mnesia:add_table_index(gread_state, receiver);
		_ -> ok
	end,
	mod_disco:register_feature(Host,?NS_GET_MSG_STATE),
	mod_disco:register_feature(Host,?NS_GROUP_READ_RECEIPT),
	ejabberd_hooks:add(user_receive_packet,Host,?MODULE,receive_packet,50),
	ejabberd_hooks:add(resend_group_messages_hook,Host,
					   ?MODULE,pop_offline_group_message,50),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GET_MSG_STATE,?MODULE,process_iq,IQDisc),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_READ_RECEIPT,?MODULE,process_iq,IQDisc),
	ok.

stop(Host) ->
	mod_disco:unregister_feature(Host,?NS_GET_MSG_STATE),
	mod_disco:unregister_feature(Host,?NS_GROUP_READ_RECEIPT),
	ejabberd_hooks:delete(user_receive_packet,Host,?MODULE,process_iq,50),
	ejabberd_hooks:delete(resend_group_messages_hook,Host,
					   ?MODULE,pop_offline_group_message,50),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GET_MSG_STATE),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_READ_RECEIPT),
	ok.
			

process_iq(From,_To,#iq{type=Type,xmlns=?NS_GET_MSG_STATE} = IQ) ->
	ok;
process_iq(From,_To,#iq{type=Type,xmlns=?NS_GROUP_READ_RECEIPT} = IQ)->
	ok.
	

get_group_msg(FromJID,ToJID,Packet) ->
	BinGid = xml:get_tag_attr_s(<<"gid">>,Packet),
	IntGid = erlang:binary_to_integer(BinGid),
	BinMsgId = xml:get_tag_attr_s(<<"id">>,Packet),
	TimeStamp = now(),
	FromUser = FromJID#jid.luser,
	FromServer = FromJID#jid.lserver,
	SenderNick = mod_vcard:get_user_nickname(FromUser,FromServer),
	NewPacket = xml:replace_tag_attr(<<"subtype">>,<<"group">>,Packet),
	mnesia:dirty_write(#group_msg{msgid = BinMsgId,
								  gid = IntGid,
								  timestamp = TimeStamp,
								  sendernick = SenderNick,
								  expire = never,
								  sender = FromJID,
								  packet = NewPacket}),
	UserList = mod_group:get_group_users(IntGid),
	List = case UserList of
						   non_exist_group -> [];
						   L when is_list(L) ->L
					   end,
	lists:foreach(fun(#group_user{us={User,Server} = ToUS}) ->
						  if User == FromUser -> ok;
							 true ->
								 send_group_msg(FromJID,ToUS,NewPacket)
						  end
						  end,List),
	ok.


%% mnesia:write(#msg_state{msgid = BinMsgId,
%% 															  receiver = ToJID,
%% 															  readstate = 0}),
%% 									  ejabberd_router:route(FromJID,ToJID,Packet)

pop_offline_group_message(Ls,LUser,LServer) ->
	User = jlib:nodeprep(LUser),
    Server = jlib:nameprep(LServer),
	pop_offline_group_message(Ls,User,Server,
							  gen_mod:db_type(Server,?MODULE)).


pop_offline_group_message(Ls,LUser,LServer,mnesia) ->
	US = {LUser,LServer},
	Receiver = jlib:make_jid(LUser,LServer,<<>>),
	case mnesia:dirty_match_object(#gread_state{receiver=US,readstate=0,_='_'}) of
		Rs when is_list(Rs) ->
			GroupMsgList = lists:map(fun(A) ->
											 #gread_state{msgid=MsgId} = A,
											 F = fun() ->
														 mnesia:delete_object(A),
														 Msg = A#gread_state{readstate=1},
														 mnesia:write(Msg)
												 end,
											 mnesia:transaction(F),	 
									  		 [MsgObject] = mnesia:dirty_read({group_msg,MsgId}),
											 MsgObject
										 end,Rs),
			GroupMsgList2 = lists:keysort(#group_msg.timestamp, GroupMsgList),
			Ls ++ lists:map(fun(#group_msg{}=GroupMsg) ->
							  Packet  = GroupMsg#group_msg.packet,
							  #xmlel{children = Els} = Packet,
							  NewPacket = Packet#xmlel{children = 
											Els ++ 
												[jlib:timestamp_to_xml(
												   calendar:now_to_universal_time(
													 GroupMsg#group_msg.timestamp),utc,
												   jlib:make_jid(<<"">>,LServer,<<"">>),
												   <<"Offline Storage">>),
												 jlib:timestamp_to_xml(
                     								calendar:now_to_universal_time(
													  GroupMsg#group_msg.timestamp))
													 ]},
							  {route,GroupMsg#group_msg.sender,Receiver,NewPacket}
							  end,GroupMsgList2);
		_ -> Ls
	end;
pop_offline_group_message(_Ls,_LUser,_LServer,odbc) ->
	ok.



get_group_offline_msg_len(LUser,LServer) ->
	Receiver = {LUser,LServer},
	case mnesia:dirty_match_object(#gread_state{receiver=Receiver,readstate=0,_='_'}) of
		Rs when is_list(Rs) ->
			length(Rs);
		[] -> 0;
		_ -> 0
	end.



send_group_msg(FromJID,ToUS,#xmlel{attrs=_Attrs,children=_Els} = Packet) ->
	{ToUser,ToServer} = ToUS,
	ToJID = jlib:make_jid(ToUser,ToServer,<<"">>),
	BinMsgId = xml:get_tag_attr_s(<<"id">>,Packet),
	case mnesia:dirty_index_read(session, ToUS, #session.us) of
		[] ->  %%session is empty     --means user is offline
			F = fun() ->
						mnesia:write(#gread_state{msgid = BinMsgId,
												receiver = ToUS,
												readstate = 0})
				end,
			mnesia:transaction(F),
			ejabberd_hooks:run(send_group_push_hook,ToServer,[FromJID,ToJID,Packet]);
		Ss -> 
			F = fun() ->
						mnesia:write(#gread_state{msgid = BinMsgId,
												receiver = ToUS,
												readstate = 1})
				end,
			mnesia:transaction(F),
			lists:foreach(fun(#session{sid=Sid,usr=USR}) ->
								  Pid = element(2,Sid),
								  RealTo = jlib:make_jid(USR),
								  Pid ! {route,FromJID,RealTo,Packet}
								  end,Ss)
	end.
			


receive_packet(_JID,From,To,Packet) ->
	ok.




















