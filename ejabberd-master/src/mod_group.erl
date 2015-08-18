%% @author Archer
%% @doc @todo Add description to mod_group.


-module(mod_group).

-behaviour(gen_mod).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1,create_group/3,is_group_exist/1,process_iq/3,get_group_users/1,
		 get_group_creater/1,dispatcher_all_group_user/3]).

-export_type([rtype/0,usors/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("information.hrl").

-define(BUYER_MAX_USER,200).
-define(NO_BUYER_MAX_USER,100).
-define(DEFAULT_G_TYPE,1).

-define(DEFAULT_MAX_GROUP,5).

%% type : set|get      set: new group user relationship  | get : get_group_users 
-define(NS_GROUP_ROSTER,<<"jabber:iq:group:roster">>).
%% type : set 			set: new group
-define(NS_GROUP,<<"jabber:iq:group">>).
%% type : get 			get: get user's groups
-define(NS_GROUP_R,<<"jabber:iq:group:r">>).

-type stype() :: binary(). %% <<"request">> | <<"join">> |<<"exit">> |<<"kicked">>


-type rtype() :: atom().  %%ask | allow | deny | invited

-type(usors() :: {binary(),binary()} | binary()) .  %% us or server
%% ====================================================================
%% Internal functions
%% ====================================================================

-record(group_info,{gid  :: integer(),
					gcreater :: {binary(),binary()},
					gcreatedate :: erlang:timestamp(),
					gmaxuser :: integer(),
					gadministrator :: tuple()| '_',
					gtype :: binary() 
				   }).
-record(group_user,{gid  ::integer()|'_' ,
					us = {<<"">>,<<"">>} ,
					nick :: binary(),
					rtype :: rtype() | '_',  
%% relation_type ask: request| allow: in | invited: be invieted | exit : sign out |kicked : kick out |forbidden  forbiddened
					ask = <<"">>,
					role :: atom() | '_',  
%% role of group    asker | (member | creater | admin) | invited | exiter | kicked   %% if role is kicked then forbidden his request
					last_action :: {rtype(),usors()}|'_'    
%% last action : ask by us | allow by us| invited by us| exit by us | kicked by us
				   }).


start(Host,Opts) ->
	io:format("start mod_group",[]),
	IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
	case gen_mod:db_type(Opts) of
		mnesia -> 
			mnesia:create_table(group_info,
								[{disc_copies, [node()]},
								 {record_name,group_info},
								 {attributes,record_info(fields,group_info)}
								]),
			mnesia:create_table(group_user,
								[{disc_only_copies,[node()]},
								 {type,bag},
								 {attributes,record_info(fields,group_user)}
								 ]),
			mnesia:add_table_index(group_user, us);
		_ -> ok
	end,
	mod_disco:register_feature(Host,?NS_GROUP_ROSTER),
	mod_disco:register_feature(Host,?NS_GROUP),
	mod_disco:register_feature(Host,?NS_GROUP_R),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_ROSTER,?MODULE,process_iq,IQDisc),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP,?MODULE,process_iq,IQDisc),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_R,?MODULE,process_iq,IQDisc),
	gen_iq_handler:add_iq_handler(ejabberd_local,Host,
								  ?NS_GROUP_ROSTER,?MODULE,process_iq,IQDisc),
	ok.


stop(Host) ->
	mod_disco:unregister_feature(Host,?NS_GROUP_ROSTER),
	mod_disco:unregister_feature(Host,?NS_GROUP),
	mod_disco:unregister_feature(Host,?NS_GROUP_R),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_ROSTER),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_R),
	gen_iq_handler:remove_iq_handler(ejabberd_local,Host,
								  ?NS_GROUP_ROSTER),
	ok.

process_iq(From,_To,#iq{type=Type,xmlns=?NS_GROUP_ROSTER,sub_el=SubEl} = IQ) ->
	case Type of
		get -> 
			Gid = xml:get_tag_attr_s(<<"gid">>,SubEl),
			IntGid = binary_to_integer(Gid),
			FromUS = {From#jid.luser,From#jid.lserver},
			case is_user_in_group(IntGid,FromUS) of
				true -> 
					case get_group_users(IntGid) of
						non_exist_group ->  
							IQ#iq{type=error,sub_el=[?ERR_NO_GROUP]};
						UserList -> 
							Items = user_list_to_xml(UserList,[]),
							IQ#iq{type=result,sub_el=[#xmlel{name = <<"query">>,
															 attrs = [{<<"xmlns">>,?NS_GROUP_ROSTER},
																	  {<<"gid">>,Gid}],
															 children =  Items }]}
					end;
				false -> IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
			end;
		_ -> IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
	end;
process_iq(From,_To,#iq{type=Type,xmlns=?NS_GROUP,sub_el=SubEl}  = IQ) ->
	case Type of
		set ->
			Rtype = xml:get_tag_attr_s(<<"rtype">>,SubEl),
			case Rtype of
				<<"create">> ->
					Creater = {From#jid.luser,From#jid.lserver},
					EGroup = get_user_create_group_num(Creater),
					case mod_expiration_date:user_is_buyer(From#jid.luser,From#jid.lserver) of
						   true -> 
							   MaxUser = ?BUYER_MAX_USER,
							   Maxgroup = gen_mod:get_module_opt(global,?MODULE,
													  buyer,
													  fun(A) when is_integer(A) -> A end,
													  ?DEFAULT_MAX_GROUP);
						   false -> 
							   MaxUser = ?NO_BUYER_MAX_USER,
							   Maxgroup = gen_mod:get_module_opt(global,?MODULE,
													  not_buyer,
													  fun(B) when is_integer(B) -> B end,
													  ?DEFAULT_MAX_GROUP)
					   end,	
					if EGroup < Maxgroup ->
						   Gtype = xml:get_tag_attr_s(<<"gtype">>,SubEl),
						   {ok,Gid} = create_group(Creater,Gtype,MaxUser),
						   BinMaxUser = integer_to_binary(MaxUser),
						   BinGid = integer_to_binary(Gid),
						   IQ#iq{type=result,sub_el=[#xmlel{name = <<"query">>,
															attrs = [{<<"xmlns">>,?NS_GROUP},
																	 {<<"maxuser">>,BinMaxUser},
																	 {<<"gid">>,BinGid}
																	],
															children = [] 
														   }]};
					   true -> 
						   IQ#iq{type=error,sub_el=[?ERR_MAX_GROUP]}
					end;
				<<"admin">> ->
					BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
					IntGid = erlang:binary_to_integer(BinGid),
					FromUser = From#jid.luser,
					FromServer = From#jid.lserver,
					FromUS = {FromUser,FromServer},
					case mnesia:dirty_read({group_info,IntGid}) of
						[] -> IQ#iq{type=error,sub_el=[?ERR_NO_GROUP]};
						[#group_info{gcreater = Creater} = GroupInfo] when Creater == FromUS ->
							AdminUser = xml:get_tag_attr_s(<<"admin">>,SubEl),
							AdminUS = {AdminUser,FromServer},
							Operation = xml:get_tag_attr_s(<<"otype">>,SubEl),
							MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
							case Operation of
								<<"set">> ->
									case mnesia:dirty_match_object(#group_user{gid=IntGid,us=AdminUS,_='_'}) of
										[#group_user{role=Role} = A] ->
											if Role == member ->
												   NewAdministrator = case GroupInfo#group_info.gadministrator of
													   					  [] -> [AdminUS];
																		    S -> S ++ [AdminUS]
																	  end,
												   NewGroupInfo  = GroupInfo#group_info{gadministrator = NewAdministrator},
												   mnesia:dirty_write(NewGroupInfo),
												   F = fun() ->
															 mnesia:delete_object(A),
															 NewA = A#group_user{role = admin,rtype= admined,last_action = {admined,FromUS}},
															 mnesia:write(NewA)
													   end,
												   mnesia:transaction(F),
												   UserList = get_group_users(IntGid),
												   Packet = #xmlel{name= <<"message">>,
																   attrs= [{<<"xmlns">>,?NS_GROUP},
																		   {<<"id">>,MsgId},
																		   {<<"gid">>,BinGid},
																		   {<<"type">>,<<"groupauth">>},
																		   {<<"stype">>,<<"admin">>},
																		   {<<"otype">>,<<"set">>},
																		   {<<"admin">>,AdminUser}],
																  children = []},
												   lists:foreach(fun(#group_user{us={ToUser,ToServer}}) ->
																		 InformTo = jlib:make_jid(ToUser,ToServer,?RESOURCE),
																		 ejabberd_router:route(From,InformTo,Packet)
																		 end, UserList),
												   IQ#iq{type=result,sub_el=[#xmlel{name= <<"query">>,
																					attrs = [{<<"xmlns">>,?NS_GROUP}],
																					children = [{xmlcdata,<<"ok">>}]
																				   }]};
											   Role == admin -> 
												   IQ#iq{type=result,sub_el=[#xmlel{name= <<"query">>,
																					attrs = [{<<"xmlns">>,?NS_GROUP}],
																					children = [{xmlcdata,<<"ok">>}]
																				   }]};
											   true -> 
												   IQ#iq{type=error,sub_el=[?ERR_FAILED_OPERATION]}
											end;
										[] -> IQ#iq{type=error,sub_el=[?ERR_FAILED_OPERATION]}
									end;
								<<"cancel">> ->
									case mnesia:dirty_match_object(#group_user{gid=IntGid,us=AdminUS,_='_'}) of
										[] -> IQ#iq{type=error,sub_el=[?ERR_FAILED_OPERATION]};
										[#group_user{role=Role} = A] -> 
											if Role == member ->
												   IQ#iq{type=result,sub_el=[#xmlel{name= <<"query">>,
																					attrs = [{<<"xmlns">>,?NS_GROUP}],
																					children = [{xmlcdata,<<"ok">>}]
																				   }]};
											   Role == admin ->
												   F = fun() ->
															   mnesia:delete_object(A),
															   NewA = A#group_user{role = member,rtype= cancel,last_action ={cancel,FromUS}},
															   mnesia:write(NewA)
													   end,
												   mnesia:transaction(F),
												   UserList = get_group_users(IntGid),
												   Packet = #xmlel{name= <<"message">>,
						                                           attrs= [{<<"xmlns">>,?NS_GROUP},
						                                                   {<<"id">>,MsgId},
						                                                   {<<"gid">>,BinGid},
						                                                   {<<"type">>,<<"groupauth">>},
						                                                   {<<"stype">>,<<"admin">>},
						                                                   {<<"otype">>,<<"cancel">>},
						                                                   {<<"admin">>,AdminUser}],
						                                           children = []},
												   lists:foreach(fun(#group_user{us={ToUser,ToServer}}) ->
																		 InformTo = jlib:make_jid(ToUser,ToServer,?RESOURCE),
																		 ejabberd_router:route(From,InformTo,Packet)
																		 end,UserList),
												   IQ#iq{type=result,sub_el=[#xmlel{name= <<"query">>,
																					attrs = [{<<"xmlns">>,?NS_GROUP}],
																					children = [{xmlcdata,<<"ok">>}]
																				   }]};
											   true ->IQ#iq{type=error,sub_el=[?ERR_FAILED_OPERATION]}
											end
									end;
								_ -> IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
							end;
						_ -> IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
					end;
				_ -> 
					IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
			end;
		get -> 
			BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
			if BinGid == <<>> orelse BinGid == false -> 
				   IQ#iq{type = error, sub_el = [?ERR_NO_GROUP]};
			   true -> 
				   IntGid = erlang:binary_to_integer(BinGid),
				   case mnesia:dirty_read({group_info,IntGid}) of 
					   [] -> IQ#iq{type = error, sub_el = [?ERR_NO_GROUP]};
					   [Group] -> 
						   GName = mod_group_vcard:get_group_name(IntGid),
						   MaxUser = Group#group_info.gmaxuser,
						   BinMaxUser = erlang:integer_to_binary(MaxUser),
						   {Creater,_Server} = Group#group_info.gcreater,
						   IQ#iq{type = result,sub_el = [#xmlel{name = <<"query">>,
																attrs = [{<<"xmlns">>,?NS_GROUP},
																		 {<<"maxuser">>,BinMaxUser},
																		 {<<"gid">>,BinGid},
																		 {<<"gname">>,GName},
																		 {<<"creater">>,Creater}],
																children = []
															   }]}
				   end
			end;
		_ -> IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
	end;
process_iq(From,_To,#iq{type=Type,xmlns=?NS_GROUP_R,sub_el=SubEl}= IQ) ->
	case Type of
		get ->
			User = From#jid.luser,
			Server = From#jid.lserver,
			case get_user_groups(User,Server) of
				no_exist_user -> IQ#iq{type = error, sub_el = [?ERR_NO_RIGHT]};
				no_exist_group -> 
					IQ#iq{type = result,sub_el = [#xmlel{name = <<"query">>,
													     attrs = [{<<"xmlns">>,?NS_GROUP_R}],
													     children = [] }]};
				GroupList -> 
					Items = group_list_to_xml(GroupList,[]),
					IQ#iq{type = result,sub_el = [#xmlel{name = <<"query">>,
														 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
														 children = Items }]}
			end;
		set -> 
			BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
			BinRtype = xml:get_tag_attr_s(<<"rtype">>,SubEl),
			case BinRtype of
				<<"ask">> -> 
					Ask = xml:get_tag_attr_s(<<"ask">>,SubEl),
					AskMsg = case Ask of
								 false -> <<>>;
								 S -> S
							 end,
					IntGid = erlang:binary_to_integer(BinGid),
					User = From#jid.luser,
					Server = From#jid.lserver,
					US = {User,Server},
					case ask_orignal_role_check(IntGid,US) of
						forbidden -> IQ#iq{type = error,sub_el = [?ERROR_ALLREADY_BE]};
						ok -> 
							create_relation(IntGid,US,ask,AskMsg,asker,{ask,US}),
							AskFrom = jlib:make_jid(User,Server,?RESOURCE),
							NickName = mod_vcard:get_user_nickname(User,Server),
							MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
							AskPacket = #xmlel{name = <<"message">>,
											   attrs=[{<<"type">>,<<"groupauth">>},
													  {<<"id">>,MsgId},
													  {<<"nick">>,NickName},
													  {<<"stype">>,<<"request">>},
													  {<<"gid">>,BinGid},
													  {<<"ask">>,AskMsg}],
											   children = []
											  },
							dispatcher_manage_group_user(IntGid,AskFrom,AskPacket),
							IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																 attrs= [{<<"xmlns">>,?NS_GROUP_R}],
																 children = [{xmlcdata,<<"ok">>}]
																}
														 ]}
					end;
				<<"allow">> -> 
					AType = xml:get_tag_attr_s(<<"allowtype">>,SubEl),
					case AType of 
						false -> IQ#iq{type = error,sub_el = [?ERR_RQUIRE_ALLOW_TYPE]};
						<<>>  -> IQ#iq{type = error,sub_el = [?ERR_RQUIRE_ALLOW_TYPE]};
						<<"in">> ->
							BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl), 
							ByUS = {From#jid.luser,From#jid.lserver},
							Gid = erlang:binary_to_integer(BinGid),
							AskMsg = case xml:get_tag_attr_s(<<"ask">>,SubEl) of
										 false -> <<>>;
										 S -> S
									 end,
							case group_allow_right(Gid,ByUS) of
								true -> 
									Joiner = xml:get_tag_attr_s(<<"joiner">>,SubEl),
									Server = From#jid.lserver,
									JoinerUS = {Joiner,Server},
									NickName = mod_vcard:get_user_nickname(Joiner,Server),
									case in_orignal_role_check(Gid,JoinerUS) of
										forbidden -> IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																				 		  attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																				 		  children = [{xmlcdata,<<"ok">>}]
																						  }
																		 		  ]};
										ok -> 
											case create_relation(Gid, JoinerUS, allow, AskMsg, member, {allow,ByUS}) of
												{atomic,ok} -> 
													MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
													InPacket = #xmlel{name = <<"message">>,
																	  attrs = [{<<"type">>,<<"groupauth">>},
																			   {<<"id">>,MsgId},
																			   {<<"nick">>,NickName},
																			   {<<"gid">>,BinGid},
																			   {<<"stype">>,<<"join">>}],
																	  children = []
																	 },
													InFrom = jlib:make_jid(Joiner,Server,?RESOURCE),
													dispatcher_all_group_user(Gid, InFrom, InPacket),
													IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																				 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																				 children = [{xmlcdata,<<"ok">>}]
																				}
																		 ]};
												_ -> IQ#iq{type = error,sub_el = [?ERR_FAILED_OPERATION]}
											end
									end;
								false -> 
									IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
							end;
						<<"invite">> ->
							BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl), 
							IntGid = erlang:binary_to_integer(BinGid),
							JoinUser  = From#jid.luser,
							JoinServer = From#jid.lserver,
							JoinerUS = {JoinUser,JoinServer},
							InviteUser = xml:get_tag_attr_s(<<"inviter">>,SubEl),
							InviteUS = {InviteUser,JoinServer},
							AskMsg = case xml:get_tag_attr_s(<<"ask">>,SubEl) of
										 false -> <<>>;
										 Msg when is_binary(Msg) -> Msg;
										 _ -> <<>>
									 end,
							case in_orignal_role_check(IntGid,JoinerUS) of
								ok -> 
									case create_relation(IntGid, JoinerUS, allow, AskMsg, member, {invite,InviteUS}) of
										{atomic,ok} -> 
											MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
											NickName = mod_vcard:get_user_nickname(JoinUser,JoinServer),
											InPacket = #xmlel{name = <<"message">>,
															  attrs = [{<<"type">>,<<"groupauth">>},
																	   {<<"id">>,MsgId},
																	   {<<"gid">>,BinGid},
																	   {<<"stype">>,<<"join">>},
																	   {<<"nick">>,NickName}],
															  children = []
															 },
											InFrom = jlib:make_jid(JoinUser,JoinServer,?RESOURCE),
											dispatcher_all_group_user(IntGid, InFrom, InPacket),
											IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																				 attrs = [{<<"xmlns">>,?NS_GROUP_R},
																						  {<<"ask">>,AskMsg}],
																				 children = [{xmlcdata,<<"ok">>}]
																				}
																		 ]};
										_ -> IQ#iq{type = error,sub_el = [?ERR_FAILED_OPERATION]}
									end;
								forbidden -> IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																				  attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																				  children = [{xmlcdata,<<"ok">>}]
																				}
																		 ]}
							end;
						_ -> IQ#iq{type = error,sub_el = [?ERR_RQUIRE_ALLOW_TYPE]}
					end;
				<<"exit">> ->
					ExitUser = From#jid.luser,
					ExitServer = From#jid.lserver,
					ExitUS = {ExitUser,ExitServer},
					BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
					IntGid = erlang:binary_to_integer(BinGid),
					MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
					NickName = mod_vcard:get_user_nickname(ExitUser,ExitServer),
					case mnesia:dirty_read({group_info,IntGid}) of
						[] -> IQ#iq{type = error, sub_el = [?ERR_NO_GROUP]};
						[#group_info{gcreater = Creater}] when Creater == ExitUS ->  %% if is the group creater
							DisPacket = #xmlel{name= <<"message">>,
											   attrs = [{<<"id">>,MsgId},
														{<<"type">>,<<"groupauth">>},
														{<<"stype">>,<<"disband">>},
														{<<"gid">>,BinGid},
														{<<"nick">>,NickName},
														{<<"user">>,ExitUser}],
											   children = []
												},
							dipatcher_disband_packet(IntGid,From,DisPacket,disband),
							IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																 children = [{xmlcdata,<<"ok">>}]
																}]};
						_ -> 			%% if not the gorup creater
							DisPacket = #xmlel{name= <<"message">>,
											   attrs = [{<<"id">>,MsgId},
														{<<"type">>,<<"groupauth">>},
														{<<"stype">>,<<"exit">>},
														{<<"gid">>,BinGid},
														{<<"nick">>,NickName},
														{<<"user">>,ExitUser}],
											   children = []
												},
							dipatcher_disband_packet(IntGid,From,DisPacket,exit),
							IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																 children = [{xmlcdata,<<"ok">>}]
																}]}
						end;
				<<"kickout">> ->
					FromUser = From#jid.luser,
					FromServer = From#jid.lserver,
					FromUS = {FromUser,FromServer},
					BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
					OutUser = xml:get_tag_attr_s(<<"outer">>,SubEl),
					OutUS = {OutUser,FromServer},
					IntGid = erlang:binary_to_integer(BinGid),
					MsgId = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
					NickName = mod_vcard:get_user_nickname(OutUser,FromServer),
					case group_allow_right(IntGid,FromUS) of
						true ->
							case mnesia:dirty_match_object(#group_user{gid=IntGid,us=OutUS,_='_'}) of
								[] -> IQ#iq{type = error,sub_el = [?ERR_NO_USER]};
								[#group_user{role=_Role} = O] ->
									Packet = #xmlel{name= <<"message">>,
											   attrs = [{<<"id">>,MsgId},
														{<<"type">>,<<"groupauth">>},
														{<<"stype">>,<<"kickout">>},
														{<<"gid">>,BinGid},
														{<<"nick">>,NickName},
														{<<"user">>,OutUser}],
											   children = []
												},
									dispatcher_all_group_user(IntGid,From,Packet),
									F = fun() ->
												mnesia:delete_object(O)
										end,
									mnesia:transaction(F),
									IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																 children = [{xmlcdata,<<"ok">>}]
																}]}
							end;
						false -> 
							IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
					end;
				<<"invite">> ->
					BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
					IntGid  = erlang:binary_to_integer(BinGid),
					InviterUser = From#jid.luser,
					InviterServer = From#jid.lserver,
					InviterUS = {InviterUser,InviterServer},
					case group_allow_right(IntGid,InviterUS) of
						true -> %% have the right to invite others 
							Beinviteds = xml:get_tag_attr_s(<<"beinviteds">>,SubEl),
							UserList = filter_inviteds(IntGid,Beinviteds,InviterServer),
							if UserList == [] -> 
								   ok;
							   true -> 
								    FromJID = jlib:make_jid(InviterUser,InviterServer,?RESOURCE),
									MsgId =  erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
									NickName =  mod_vcard:get_user_nickname(InviterUser,InviterServer),
									AskMsg = case xml:get_tag_attr_s(<<"ask">>,SubEl) of
												 false -> <<>>;
												 Msg when is_binary(Msg) -> Msg;
												 _ -> <<>>
											 end,
									GroupName = mod_group_vcard:get_group_name(IntGid),
									InvitePacket = #xmlel{name= <<"message">>,
														   attrs = [{<<"type">>,<<"groupauth">>},
																	{<<"id">>,MsgId},
																	{<<"stype">>,<<"invite">>},
																	{<<"gid">>,BinGid},
																	{<<"nick">>,NickName},
																	{<<"ask">>,AskMsg},
																	{<<"gname">>,GroupName}],
														   children = []},
%% 									#xmlel{name = <<"request">>,
%% 																						  attrs = [{<<"xmlns">>,?NS_RECEIPTS}],
%% 																						  children = []}
%% 																	  
									dispatcher_invite_msg(IntGid,FromJID,UserList,AskMsg,InvitePacket)
							end,
							IQ#iq{type = result,sub_el = [#xmlel{name= <<"query">>,
																 attrs = [{<<"xmlns">>,?NS_GROUP_R}],
																 children = [{xmlcdata,<<"ok">>}]
																}
														  ]};
						false -> 
							IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
					end
			end;
		_ -> IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
	end;
process_iq(_From,_To,#iq{sub_el= _SubEl} = IQ) ->
	IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}.



filter_inviteds(IntGid,Beinviteds,InviterServer) ->
	BeinvitedList = re:split(Beinviteds, ","),
	TmpList = lists:filter(fun(BeinvitedUser) ->
									 case ejabberd_auth:is_user_exists(BeinvitedUser,InviterServer) of
										 false -> false;
										 true -> 
											 BeinvitedUS = {BeinvitedUser,InviterServer},
											 case invite_orignal_role_check(IntGid,BeinvitedUS) of
												 ok -> true;
												 forbidden -> false
											 end
									 end
							end, BeinvitedList),
	if TmpList == [] -> [];
	   true -> lists:map(fun(A) ->
									 {A,InviterServer}
							end,
							TmpList)
	end.
	


%% @spec {binary(),binary}, binary() ,binary(), binary() -> {ok,}
create_group(Creater,Gtype,MaxUser) ->
	Gid = generate_gid(),
	{_User,Server} = Creater,
	RMaxUser = case MaxUser of
				   false -> ?NO_BUYER_MAX_USER;
				   <<>> -> ?NO_BUYER_MAX_USER;
				   _ when is_binary(MaxUser) -> erlang:binary_to_integer(MaxUser);
				   _ -> MaxUser
			   end,
	RGtype = case Gtype of
				 false ->  ?DEFAULT_G_TYPE;
				 <<>> -> ?DEFAULT_G_TYPE;
				 _  when is_binary(Gtype) -> erlang:binary_to_integer(Gtype);
				 _ -> Gtype
			 end,
	Now = now(),
	F = fun () -> mnesia:write(#group_info{gid=Gid,
										   gcreater = Creater,
										   gcreatedate = Now,
										   gmaxuser = RMaxUser,
										   gtype = RGtype,
										   gadministrator = [] 
										   })
		end,
	mnesia:transaction(F),
	_Res = create_relation(Gid,Creater,allow,<<"">>,creater,{allow,Server}),
	{ok,Gid}.


%% send message to all group users
dispatcher_all_group_user(IntGid,From,Packet) ->
	case get_group_users(IntGid) of
		non_exist_group -> ok;
		UserList when is_list(UserList) ->
			lists:foreach(fun (#group_user{us = {ToUser,ToServer}}) ->
								   ToJID = jlib:make_jid(ToUser,ToServer,?RESOURCE),
								   ejabberd_router:route(From,ToJID,Packet)
								end,UserList)
	end.



%% integer ,jid,iq
dispatcher_manage_group_user(Gid,AskFrom,AskPacket) ->
	Creater = get_group_creater(Gid),
	{CreaterUser,CreaterServer} = Creater,
	CreaterJID = jlib:make_jid(CreaterUser,CreaterServer,?RESOURCE),
	ejabberd_router:route(AskFrom,CreaterJID,AskPacket),
	case get_group_admin(Gid) of
		[] -> ok;
		GadminList when is_list(GadminList) -> 
			lists:foreach(fun (AskTo) ->
						    {ToUser,ToServer} = AskTo,
							ToJid = jlib:make_jid(ToUser,ToServer,?RESOURCE),
							ejabberd_router:route(AskFrom,ToJid,AskPacket)
				  		  end, GadminList)
	end.

	
%% (jid,[{user,server}|{user,server}],#xmlel{}) 
dispatcher_invite_msg(IntGid,FromJID,UserList,AskMsg,InvitePacket) ->
	lists:foreach(fun({User,Server}) ->
						  FromUS = {FromJID#jid.luser,FromJID#jid.lserver},
						  create_relation(IntGid, {User,Server}, invited, AskMsg, invited, {invite,FromUS}),
						  ToJID = jlib:make_jid(User,Server,?RESOURCE),
						  ejabberd_router:route(FromJID,ToJID,InvitePacket)
				  end, UserList).


dipatcher_disband_packet(IntGid,From,DisPacket,disband) ->
	case mnesia:dirty_match_object(#group_user{gid=IntGid,_='_'}) of
		[] -> ok;
		UserList when is_list(UserList) ->
			case mnesia:dirty_read({group_info,IntGid}) of
				[] -> ok;
				[Group] -> Fs = fun() -> 
								   mnesia:delete_object(Group)
							   end,
						   mnesia:transaction(Fs)
			end,
			lists:foreach(fun(A) ->
								  #group_user{us={User,Server},role = Role} = A,
								  F = fun() ->
											  mnesia:delete_object(A)
									  end,
								  if(Role == asker orelse Role == exiter orelse Role == kicked) ->
										ok;
									true ->
										To = jlib:make_jid(User,Server,?RESOURCE),
						  		  		ejabberd_router:route(From,To,DisPacket)
								  end,
								  mnesia:transaction(F)
								  end, UserList)
	end;
dipatcher_disband_packet(IntGid,From,DisPacket,exit) ->
	case get_group_users(IntGid) of
		non_exist_group -> ok;
		UserList when is_list(UserList) ->
			ExitUS = {From#jid.luser,From#jid.lserver},
			case mnesia:dirty_match_object(#group_user{gid=IntGid,us=ExitUS,_='_'}) of
				[] -> ok;
				[A] -> 
					F = fun() -> mnesia:delete_object(A) end,
					lists:foreach(fun(#group_user{us={User,Server},role=Role}) 
							   when (Role == member orelse Role == admin orelse Role == creater)->
								  To = jlib:make_jid(User,Server,?RESOURCE),
								  ejabberd_router:route(From,To,DisPacket),
								  mnesia:transaction(F);
									 (_) -> mnesia:transaction(F)
								  end, UserList)
			end
	end.
		


%% {<<"">>,<<"">>} -> integer
get_user_create_group_num(US) -> 
	length( mnesia:dirty_match_object(#group_info{gcreater = US,_='_'}) ).
	

%% -> integer
generate_gid() ->
	TmpGid = public_function:get_random_num(999999999),
	case is_group_exist(TmpGid) of
		false -> TmpGid;
		true -> generate_gid()
	end.


%% integer -> boolean
is_group_exist(Gid) ->
	case mnesia:dirty_read({group_info,Gid}) of
		[] -> false;
		_ -> true
	end.

%% integer,{<<>>,<<>>} -> true | false
group_allow_right(Gid,ByUS) -> 
	case mnesia:dirty_match_object(#group_info{gid=Gid,gcreater=ByUS,_='_'}) of
		[] -> case mnesia:dirty_read({group_info,Gid}) of
				  [] -> false;
				  [#group_info{gadministrator = Gadmin}] when is_list(Gadmin) -> 
					  lists:member(ByUS, Gadmin);
				  _ -> false
			  end;
		[_] -> true
	end.


ask_orignal_role_check(IntGid,US) -> 
	case mnesia:dirty_match_object(#group_user{gid= IntGid,us= US,_='_'})  of
		[] -> ok;
		[#group_user{role = Role}] -> %% asker | (member | creater | admin) | invited | exiter | kicked
			if (Role == asker orelse Role == exiter orelse Role == invited) ->
				   ok;
			   (Role == member orelse Role == creater orelse Role == admin ) ->
				   forbidden;
			   true -> forbidden
			end
	end.


in_orignal_role_check(IntGid,US) ->
	case mnesia:dirty_match_object(#group_user{gid=IntGid,us=US,_='_'}) of
		[] -> forbidden;
		[#group_user{role=Role}] -> 
			if Role == asker -> ok;
			   Role == invited -> ok;
			   true -> forbidden
			end
	end.


invite_orignal_role_check(IntGid,US) ->
	case mnesia:dirty_match_object(#group_user{gid=IntGid,us=US,_='_'}) of
		[] -> ok;
		[#group_user{role = Role}] ->
			if Role == asker -> ok;
			   Role == invited -> ok;
			   Role == exiter -> ok;
			   Role == kicked -> ok;
			   true -> forbidden
			end
	end.

is_user_in_group(IntGid,US) ->
	case mnesia:dirty_match_object(#group_user{gid=IntGid,us=US,_='_'}) of
		[] -> false;
		[#group_user{role = Role}] ->
			if Role == asker -> false;
			   Role == invited -> false;
			   Role == exiter -> false;
			   Role == kicked -> false;
			   true -> true
			end
	end.

get_group_creater(Gid) ->
	case mnesia:dirty_read({group_info,Gid}) of
		[] -> false;
		[#group_info{gcreater = Creater}] -> Creater
	end.

get_group_admin(Gid) ->
	case mnesia:dirty_read({group_info,Gid}) of
		[] -> [];
		[#group_info{gadministrator = Gadmin}] when Gadmin ==undefined -> [];
		[#group_info{gadministrator = Gadmin}] -> Gadmin
	end.



%% binary(),US,atom(),atom(),binary(),{atom(),US}
create_relation(Gid,Creater,RType,AskMsg,Role,LastAction) ->
	case mnesia:dirty_match_object(#group_user{gid=Gid,us=Creater,_='_'}) of
		[] -> F =  fun() ->
						   mnesia:write(#group_user{gid=Gid,us=Creater,rtype=RType,ask = AskMsg,role = Role,last_action=LastAction})
				   end,
			  mnesia:transaction(F);
		[R] -> F = fun() ->
						   mnesia:delete_object(R),
						   mnesia:write(#group_user{gid=Gid,us=Creater,rtype=RType,ask = AskMsg,role = Role,last_action=LastAction})
				   end,
			   mnesia:transaction(F)
	end.

	

get_user_groups(User,Server) ->
	US = {User,Server},
	case ejabberd_auth:is_user_exists(User,Server) of
		true -> 
			Pat = #group_user{us=US,_='_'},
			Result = mnesia:dirty_match_object(Pat),
			case Result of
				[] -> no_exist_group;
				GroupList when erlang:is_list(GroupList) ->
					lists:filter(fun(#group_user{role = member}) -> true;
									(#group_user{role = admin}) -> true;
									(#group_user{role = creater }) -> true;
									(_) -> false
								 end,GroupList)
			end;
		_ -> no_exist_user
	end.



%% integer -> list
get_group_users(Gid) ->
	case is_group_exist(Gid) of
		false -> non_exist_group;
		true -> case mnesia:dirty_read({group_user,Gid}) of
					[] -> non_exist_group;
					UserList when erlang:is_list(UserList) -> 
						lists:filter(fun(#group_user{role = asker}) -> false;
										(#group_user{role = invited}) -> false;
										(#group_user{role = exiter })-> false;
										(#group_user{role = kicked })-> false;
										(_) -> true
									 end, UserList)
				end
	end.

user_list_to_xml([],Acc) ->
	Acc;
user_list_to_xml([#group_user{us={User,Server},role = Role}|Tail],Acc) ->
	NickName = mod_vcard:get_user_nickname(User,Server),
	BinRole = erlang:atom_to_binary(Role,latin1),
	user_list_to_xml(Tail,[#xmlel{name = <<"item">>,
								 attrs = [{<<"user">>,User},{<<"nick">>,NickName},{<<"role">>,BinRole}],
								 children=[]}|Acc]).
	


group_list_to_xml([],Acc) ->
	Acc;
group_list_to_xml([#group_user{gid=Gid,role=Role} | Tail],Acc) ->
	GroupName = mod_group_vcard:get_group_name(Gid),
	BinRole = erlang:atom_to_binary(Role, latin1),
	BinGid = erlang:integer_to_binary(Gid),
	Version = mod_group_vcard:get_group_photo_version(Gid),
	group_list_to_xml(Tail,[#xmlel{name = <<"item">>,
										 attrs = [{<<"gid">>,BinGid},
												  {<<"gname">>,GroupName},
												  {<<"role">>,BinRole},
												  {<<"photo">>,Version}],
										 children = [] }|Acc]).
	











