%% @author Archer
%% @doc @todo Add description to mod_group_vcard.


-module(mod_group_vcard).


-behaviour(gen_mod).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1,process_iq/3,delete_group_vcard/1,
		 get_group_name/1,get_group_photo_version/1]).

-include("jlib.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").

-define(NS_GROUP_VCARD,<<"jabber:group:vcard">>).
-define(GROUP_VCARD_TEMP,<<"group:vcard-temp">>).
-define(GROUP_VCARD_TEMP_UPDATE,<<"group:vcard-temp:update">>).

-record(group_vcard,{gid :: integer(),
					 vcard = #xmlel{}:: xmlel()}).

%% ====================================================================
%% Internal functions
%% ====================================================================
start(Host,Opts) ->
	?INFO_MSG("start group vcard mod ...",[]),
	IQDisc = gen_mod:get_opt(iqdisc,Opts,fun gen_iq_handler:check_type/1,
							 one_queue),
	case gen_mod:db_type(Opts) of
		mnesia ->
			mnesia:create_table(group_vcard,
								[{disc_copies,[node()]},
								 {record_name,group_vcard},
								 {attributes,record_info(fields,group_vcard)}
								]);
		_ -> ok
	end,
	mod_disco:register_feature(Host,?NS_GROUP_VCARD),
	gen_iq_handler:add_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_VCARD,?MODULE,process_iq,IQDisc),
	ok.

stop(Host) ->
	mod_disco:unregister_feature(Host,?NS_GROUP_VCARD),
	gen_iq_handler:remove_iq_handler(ejabberd_sm,Host,
								  ?NS_GROUP_VCARD),
	ok.

process_iq(From,_To,#iq{type=Type,xmlns=?NS_GROUP_VCARD,sub_el=SubEl} = IQ) ->
	case Type of
		set ->
			BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
			IntGid = erlang:binary_to_integer(BinGid),
			FromUser = From#jid.luser,
			FromServer = From#jid.lserver,
			FromUS = {FromUser,FromServer},
			case mod_group:is_group_exist(IntGid) of
				false -> IQ#iq{type=error,sub_el=[?ERR_NO_GROUP]};
				true -> 
					Creater = mod_group:get_group_creater(IntGid),
					case is_vcard_exist(IntGid) of
						true ->   %% if vcard is exist update vcard
							if Creater == FromUS ->
							    Photo = xml:get_subtag_cdata(SubEl,<<"photo">>),
								GroupName = xml:get_subtag_cdata(SubEl,<<"gname">>), 
						    	if Photo == <<"">> orelse Photo == false ->   %% only change the nickname
									   [GroupVcard] = mnesia:dirty_read({group_vcard,IntGid}),
									   Vcard = GroupVcard#group_vcard.vcard,
									   OldPhoto = xml:get_subtag(Vcard,<<"PHOTO">>),
									   NewVcard = #xmlel{name= <<"vCard">>,
														 attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
														 children = [#xmlel{name= <<"GNAME">>,
																			 attrs = [],
																			 children = [{xmlcdata,GroupName}]},
																	 OldPhoto]},
									   NewGroupVCard = GroupVcard#group_vcard{vcard = NewVcard},
									   F = fun() ->
												   mnesia:delete_object(GroupVcard),
												   mnesia:write(NewGroupVCard)
										   end,
								   	   mnesia:transaction(F);
								   true -> %% change the nickname and photo
									   [GroupVcard1]= mnesia:dirty_read({group_vcard,IntGid}),
									   ImgType = xml:get_subtag_cdata(SubEl,<<"imgtype">>),
									   NewVcard = #xmlel{name= <<"vCard">>,
														 attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
														 children = [#xmlel{name= <<"GNAME">>,
																			attrs = [],
																			children = [{xmlcdata,GroupName}]},
																	 #xmlel{name= <<"PHOTO">>,
																			attrs = [],
																			children = [#xmlel{name= <<"TYPE">>,
																							   attrs = [],
																							   children = [{xmlcdata,ImgType}]},
																						#xmlel{name= <<"BINVAL">>,
																							   attrs = [],
																							   children = [{xmlcdata,Photo}]}]
																		   }]
														},
									   NewGroupVcard = GroupVcard1#group_vcard{vcard = NewVcard},
									   F = fun() -> 
												   mnesia:delete_object(GroupVcard1),
												   mnesia:write(NewGroupVcard)
										   end,
									   mnesia:transaction(F)
								end,
								Version = get_group_photo_version(IntGid),
								VCardPacket = #xmlel{name= <<"presence">>,
													 attrs = [{<<"xmlns">>,<<"jabber:client">>}],
													 children = [#xmlel{name= <<"x">>,
																		attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP_UPDATE},
																				 {<<"gid">>,BinGid}],
																		children = [#xmlel{name= <<"photo">>,
																						   attrs = [],
																						   children = [{xmlcdata,Version}]
																						  },
																					#xmlel{name = <<"name">>,
																						   attrs = [],
																						   children = [{xmlcdata,GroupName}]}
																				   ]
																	   }] },
								mod_group:dispatcher_all_group_user(IntGid,From,VCardPacket),
								IQ#iq{type=result,sub_el = [#xmlel{name= <<"query">>,
																   attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP},
																			{<<"photo">>,Version}],
																   children = [{xmlcdata,<<"ok">>}]
																  }]};
							   true -> IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
							end;
						false ->  %% vcard is false    new a vcard record
							if Creater == FromUS ->
								   Photo = xml:get_subtag_cdata(SubEl,<<"photo">>),
								   GroupName = xml:get_subtag_cdata(SubEl,<<"gname">>),
								   if Photo == <<"">> orelse Photo == false ->
										  Vcard = #xmlel{name= <<"vCard">>,
														 attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
														 children = [#xmlel{name= <<"GNAME">>,
																			 attrs = [],
																			 children = [{xmlcdata,GroupName}]}
																	 ]},
										  F = fun() ->
													  mnesia:write(#group_vcard{gid=IntGid,vcard=Vcard})
											  end;
									  true -> 
										  ImgType = xml:get_subtag_cdata(SubEl,<<"imgtype">>),
										  Vcard = #xmlel{name= <<"vCard">>,
														 attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
														 children = [#xmlel{name= <<"GNAME">>,
																			attrs = [],
																			children = [{xmlcdata,GroupName}]},
																	 #xmlel{name= <<"PHOTO">>,
																			attrs = [],
																			children = [#xmlel{name= <<"TYPE">>,
																							   attrs = [],
																							   children = [{xmlcdata,ImgType}]},
																						#xmlel{name= <<"BINVAL">>,
																							   attrs = [],
																							   children = [{xmlcdata,Photo}]}]
																		   }]
														},
										  F = fun() ->
													  mnesia:write(#group_vcard{gid=IntGid,vcard=Vcard})
											  end
								   end,
								   mnesia:transaction(F),
								   IQ#iq{type=result,sub_el = [#xmlel{name= <<"query">>,
																   attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
																   children = [{xmlcdata,<<"ok">>}]
																  }]};
							   true -> IQ#iq{type = error,sub_el = [?ERR_NO_RIGHT]}
							end
					end
			end;
		get ->
			BinGid = xml:get_tag_attr_s(<<"gid">>,SubEl),
			IntGid = erlang:binary_to_integer(BinGid),
			case mod_group:is_group_exist(IntGid) of
				false -> IQ#iq{type = error,sub_el = [?ERR_NO_GROUP]};
				true -> 
					case mnesia:dirty_read({group_vcard,IntGid}) of
						[] -> IQ#iq{type=result,sub_el = [#xmlel{name= <<"query">>,
																   attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP}],
																   children = [{xmlcdata,<<"no_card">>}]
																  }]};
						[#group_vcard{vcard=VCard}] -> 
%% 							Version = get_group_photo_version(IntGid),
							IQ#iq{type=result,sub_el=[#xmlel{name= <<"query">>,
															 attrs = [{<<"xmlns">>,?GROUP_VCARD_TEMP},
																	  {<<"gid">>,BinGid}],
															 children = [VCard]
															}]}
					end
			end;
		_ -> IQ#iq{type=error,sub_el=[?ERR_FEATURE_NOT_IMPLEMENTED]}
	end.


get_group_name(IntGid) ->
	case mnesia:dirty_read({group_vcard,IntGid}) of
		[] -> <<>>;
		[#group_vcard{vcard=VCard}] ->
			xml:get_path_s(VCard,[{elem,<<"GNAME">>},cdata])
	end.

get_group_photo_version(IntGid) ->
	case mnesia:dirty_read({group_vcard,IntGid}) of
		[] -> <<>>;
		[#group_vcard{vcard=VCard}] ->
			case xml:get_path_s(VCard,[{elem,<<"PHOTO">>},
									   {elem,<<"BINVAL">>},
									   cdata]) of
				<<>> -> <<"">>;
				BinVal -> p1_sha:sha(jlib:decode_base64(BinVal))
			end
	end.




is_vcard_exist(IntGid) ->
	case mnesia:dirty_read({group_vcard,IntGid}) of
		[] -> false;
		_ -> true
	end.

delete_group_vcard(IntGid) ->
	case mnesia:dirty_read({group_vcard,IntGid}) of
		[] -> ok;
		[A] -> mnesia:delete_object(A)
	end.
			
			

	

























