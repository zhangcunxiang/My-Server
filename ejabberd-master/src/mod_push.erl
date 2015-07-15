%% coding: utf-8
%% For this file we have chosen encoding = utf-8
%% -*- coding: utf-8 -*-
%% @author Archer
%% @doc @todo Add description to mod_http_offline.


%% added --zhangcunxiang   all push methods
-module(mod_push).

-behaviour(gen_mod).

-export([start/2,stop/1,do_alert_push/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("information.hrl").


start(_Host,_Opt) ->
	ejabberd_hooks:add(send_push_hook, _Host, ?MODULE,do_alert_push,50),
	ok.

stop(_Host)->
	ejabberd_hooks:delete(send_push_hook, _Host, ?MODULE,do_alert_push,50),
	ok.


do_alert_push(From,To,Packet) ->
	LUser = To#jid.luser,
	LServer = To#jid.lserver,
	LResource = To#jid.lresource,
	US = {LUser,LServer},
	case mod_device:get_device_token(US) of
		[] -> ok;
		<<"(null)">> -> ok;
		Token -> 
			DeviceType = mod_device:get_device_type(US),
			send_device_type_push(From,To,Packet,DeviceType)
	end.


%% 1. text 2. picture 3. voice 4.location 10.combined    
send_device_type_push(From,To,Packet,DeviceType) ->   % MM message
	LUser = To#jid.luser,
	LServer = To#jid.lserver,
	US = {LUser,LServer},
	Language = mod_device:get_device_language(US),
	ToUser = binary_to_list(LUser),
	PhoneToken = mod_device:get_device_token(US),
	Token = binary_to_list(PhoneToken),
	MsgId = xml:get_tag_attr_s(<<"id">>,Packet),
	ListMsgId = binary_to_list(MsgId),
	#jid{luser=LFromUser,lserver=_LFromServer} = From,
	ListFromUser = erlang:binary_to_list(LFromUser),
	NickName = mod_vcard:get_user_nickname(LFromUser,LServer),
	ListFromRealName = case NickName of
						   <<"">> -> LFromUser;
						   _ -> NickName
					   end,
	Type = xml:get_subtag_cdata(Packet,<<"type">>),
	ListAlert = case Language of 
		<<"en">> ->
			case Type of 
				<<"1">> -> 
					?Message_text_EN(ListFromRealName);
				<<"2">> ->
					?Message_picture_EN(ListFromRealName);
				<<"3">> -> 
					?Message_voice_EN(ListFromRealName);
				<<"4">> -> 
					?Message_location_EN(ListFromRealName);
				<<"10">> -> 
					?Message_combine_EN(ListFromRealName);
				_ -> 
					?Message_message_EN(ListFromRealName)
			end;
		_ -> 
			case Type of 
				<<"1">> -> 		
					?Message_text_CH(ListFromRealName);
				<<"2">> ->
					?Message_picture_CH(ListFromRealName);
				<<"3">> -> 
					?Message_voice_CH(ListFromRealName);
				<<"4">> ->
					?Message_location_CH(ListFromRealName);
				<<"10">> -> 
					?Message_combine_CH(ListFromRealName);
				_ -> 
					?Message_message_CH(ListFromRealName)
			end
	end,
	Alert = unicode:characters_to_binary(ListAlert, utf8),
	case DeviceType of
		<<"ios">> ->
			RegId = [PhoneToken],
			Message = [{<<"platform">>,<<"ios">>},
					   {<<"notification">>,[{<<"ios">>,
											 [{<<"alert">>,Alert},
											  {<<"sound">>,<<"default">>},
											  {<<"badge">>,<<"+1">>},
											  {<<"extras">>,[{<<"to">>,LUser},
															{<<"from">>,LFromUser},
															{<<"message_id">>,MsgId}]}]
											}]
					   },
					   {<<"options">>,[{<<"apns_production">>,<<"false">>}]
					   }],
			public_function:send_jpush_push(RegId,Message);
		<<"android">> -> 
			RegId = [PhoneToken],
			Message = [{<<"platform">>,<<"android">>},
					   {<<"notification">>,[{<<"android">>,
											 [{<<"alert">>,Alert},
											  {<<"extras">>,[{<<"to">>,LUser},
															{<<"from">>,LFromUser},
															{<<"message_id">>,MsgId}]}]
											}]
					   }],
			public_function:send_jpush_push(RegId,Message)
	end,
	ok.
	
	
	
	