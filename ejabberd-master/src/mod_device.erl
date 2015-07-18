%% @author Archer
%% @doc @todo Add description to mod_device.


%% added --zhangcunxiang
-module(mod_device).
-author("zhang.cunxiang").

-behavior(gen_mod).

-include("jlib.hrl").
-include("ejabberd.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1,
		 process_device_iq/3,
		 get_device_info/1,
		 insert_device_info/5,
		 remove_device_token/1,
		 get_device_language/1,
		 get_device_token/1,
		 get_device_type/1,
		 remove_user/2,
		 get_resource_device/1,
		 get_resource_token/1
		]).

-record(device_info,{us = {<<"">>,<<"">>}::{binary(),binary()}|'$1',
					 deviceType = <<"">>::binary()|'_',
					 token= <<"">> ::binary()|'_',
					 language = <<"">> ::binary()|'_',
					 deviceNum = <<"">> ::binary()|'_' 
					 }).

%% -record(device_green,{us= {<<"">>,<<"">>} :: {binary(),binary()}|'$1',
%% 					  deviceType = <<"">> :: binary()|'_',
%% 					  token = <<"">> :: binary()|'_',
%% 					  language = <<"">> :: binary()|'_',
%% 					  deviceNum = <<"">> :: binary()|'_'
%% 					  }).

%% ====================================================================
%% Internal functions
%% ====================================================================
start(Host,Opts) ->
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	mnesia:create_table(device_info,
			[{disc_copies, [node()]},
			 {record_name, device_info},
			 {attributes, record_info(fields, device_info)}]),
	ejabberd_hooks:add(remove_user,Host,?MODULE,remove_user,50),
	ejabberd_hooks:add(process_device_iq_hook,Host,?MODULE,process_device_iq,50),
	mod_disco:register_feature(Host,?NS_DEVICE),
	gen_iq_handler:add_iq_handler(ejabberd_local,Host,
								  ?NS_DEVICE,?MODULE,process_device_iq,IQDisc),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_DEVICE,?MODULE,process_device_iq,IQDisc),
	ok.

stop(Host)->
	ejabberd_hooks:delete(remove_user,Host,?MODULE,remove_user,50),
	mod_disco:unregister_feature(Host,?NS_DEVICE),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_DEVICE),
	gen_iq_handler:remove_iq_handler(ejabberd_local,Host,
								  ?NS_DEVICE),
	ok.

process_device_iq(From,To,#iq{type = Type,xmlns = ?NS_DEVICE,sub_el = SubEl} = IQ) ->
	case Type of
		set ->
			Resource = From#jid.lresource,
			UserName = From#jid.luser,
			Server = From#jid.lserver,
			DeviceNs = [{<<"xmlns">>,?NS_DEVICE}],
			SubSubEl = xml:get_subtag(SubEl,<<"device">>),
			DeviceType = xml:get_tag_attr_s(<<"type">>,SubSubEl),
			DeviceLang = xml:get_tag_attr_s(<<"language">>,SubSubEl),
			DeviceNum = xml:get_tag_attr_s(<<"deviceNum">>,SubSubEl),
			Token = xml:get_tag_cdata(SubSubEl),
			USR = {UserName,Server,Resource},
			TransR = insert_device_info(USR,DeviceType,Token,DeviceLang,DeviceNum),
			case TransR of 
				ok -> 
					IQ#iq{type=result,sub_el=[#xmlel{name= <<"device">>,
													 attrs=DeviceNs,
													 children=[{xmlcdata,<<"ok">>}]}]},
					ListUser = binary_to_list(UserName),
					ListDeviceTag = binary_to_list(DeviceNum),
					ListToken = binary_to_list(Token),
					public_function:send_device_token(ListUser,ListDeviceTag,ListToken);
				{atomic,ok} ->
					IQ#iq{type=result,sub_el=[#xmlel{name= <<"device">>,
													 attrs=DeviceNs,
													 children=[{xmlcdata,<<"ok">>}]}]},
					ListUser = binary_to_list(UserName),
					ListDeviceTag = binary_to_list(DeviceNum),
					ListToken = binary_to_list(Token),
					public_function:send_device_token(ListUser,ListDeviceTag,ListToken);
				_ -> 
					IQ#iq{type=error,sub_el=[#xmlel{name= <<"device">>,
													attrs=DeviceNs,
													children=[{xmlcdata,<<"error">>}]}]}
			end;
		_ -> 
			IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.


%% get resource mapping device
get_resource_device(USR)->
	{LUser,LServer,LResource} = USR,
	US = {LUser,LServer},
	get_device_info(US).


get_resource_token(USR) ->
	{LUser,LServer,LResource} = USR,
	US = {LUser,LServer},
	get_device_token(US).

get_device_info(US)->
	mnesia:dirty_read(device_info,US).

%% get device token blue and green
get_device_token(US)->
	case get_device_info(US) of 
		[] -> [];
		[#device_info{us = _1,deviceType = _ODeviceType,token = OToken,language=_Language,deviceNum=_}] ->
			OToken
	end.

%% get device type language
get_device_language(US) ->
	case get_device_info(US) of 
		[] ->[];
		[#device_info{us = _1,deviceType = _ODeviceType,token = _OToken,language=Language,deviceNum=_}]->
			Language
	end.


get_device_type(US)->
	case get_device_info(US) of 
		[]-> [];
		[#device_info{us = _1,deviceType = ODeviceType,token = _OToken,language=_Language,deviceNum=_}]-> 
			ODeviceType
	end.

%%insert or update token
insert_device_info(USR,DeviceType,Token,Language,DeviceNum)->
	{User,Server,Resource} = USR,
	US = {User,Server},
	MatchHead = #device_info{deviceNum=DeviceNum,us='$1',_='_'},
	Result = ['$1'],
	SelectRes = mnesia:dirty_select(device_info, [{MatchHead,[],Result}]),
	case SelectRes of
		[] -> ok;
		NewUS -> lists:foreach(fun(UserServer) ->
									remove_device_token(UserServer)
									end, NewUS)
	end,
	MatchHead1 = #device_info{token=Token,us='$1',_='_'},
	Result1 = ['$1'],
	SelectRes1 = mnesia:dirty_select(device_info, [{MatchHead1,[],Result1}]),
	case SelectRes1 of
		[]->ok;
		NewUS1 -> lists:foreach(fun(UserServer1)->
									   remove_device_token(UserServer1)
							   end, NewUS1)
	end,
	F = fun() ->
			mnesia:write(#device_info{us = US,deviceType=DeviceType,token=Token,language= Language,deviceNum=DeviceNum})
		end,
	case get_device_info(US) of
		[] -> mnesia:transaction(F);
		[#device_info{us=_1,deviceType=ODeviceType,token=OToken,language=OLanguage,deviceNum=ODeviceNum}] ->
			if (ODeviceType == DeviceType) and (OToken == Token) and (OLanguage == Language) and (ODeviceNum==DeviceNum)-> ok;
			   true -> mnesia:transaction(F)
			end
	end.
				
	
	

remove_device_token(US) -> 
	case get_device_info(US) of 
		[] ->ok;
		[#device_info{us = _1,deviceType = ODeviceType,token = _OToken,language=_OLanguage,deviceNum=_ODeviceNum}] ->
			F = fun() ->
				mnesia:write(#device_info{us=US,deviceType = ODeviceType,token= <<"">>,deviceNum= <<"">>,language=_OLanguage})
			end,
			mnesia:transaction(F)
	end.



remove_user(LUser,LServer) ->
	US = {LUser,LServer},
	F = fun() ->
			mnesia:delete({device_info,US})
		end,
	mnesia:transaction(F).


