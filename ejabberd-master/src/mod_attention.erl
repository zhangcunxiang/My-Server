%% @author Alfred
%% @doc @todo Add description to mod_attention.


-module(mod_attention).
-behaviour(gen_mod).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1, process_local_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").



%% ====================================================================
%% Internal functions
%% ====================================================================
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_ATTENTION, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_ATTENTION).


process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
	Username = _From#jid.luser,
	Server = _From#jid.lserver,
	case Type of
      set ->
	  	  Subscribed = xml:get_subtag_cdata(SubEl,<<"jid">>),
		  SubscribeType = xml:get_tag_attr_s(<<"type">>,SubEl),
		  case jlib:string_to_jid(Subscribed) of
	  		   error -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	  		   Rs -> 
				   SubscribedUser = Rs#jid.luser,
				   Subscriber = _From#jid.luser,
				   Server = _From#jid.lserver,
				   case ejabberd_auth:is_user_exists(SubscribedUser,Server) of
					   false -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
					   true -> 
						   case ejabberd_auth:is_user_exists(Subscriber,Server) of
							   false -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
							   true ->
								   set_attention(Subscriber,Subscribed,Server),
								   IQ#iq{type = result, sub_el = []}
						   end
				   end
		  end;
      get ->
		  case ejabberd_auth:is_user_exists(Username, Server) of
			  true ->
				  case get_attention(Username,Server) of
					  ok -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
					  S when is_list(S) ->
						  Result = IQ#iq{type = result,sub_el=[#xmlel{name= <<"query">>,
															  attrs= [{<<"xmlns">>,?NS_ATTENTION}],
															  children = S }] },
						  Result
				  end;
			  false ->
				 IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		  end
	end.


get_attention(User,Server) ->
	get_attention(User,Server,gen_mod:db_type(Server, ?MODULE)).

set_attention(User,Subscribed,Server) ->
	set_attention(User,Server,Subscribed,gen_mod:db_type(Server, ?MODULE)).


get_attention(User,Server,mnesia) ->
	ok;
get_attention(User,Server,odbc)->
	Username = ejabberd_odbc:escape(User),
    Server = ejabberd_odbc:escape(Server),
	sql_get_attention(Server,Username).


set_attention(User,Server,Subscribed,mensia) ->
	ok;
set_attention(User,Server,Subscribed,odbc) ->
	Subscriber = ejabberd_odbc:escape(User),
	Server = ejabberd_odbc:escape(Server),
	SubscribedJid = ejabberd_odbc:escape(Subscribed),
	sql_set_attention(Server,Subscriber,SubscribedJid).


sql_get_attention(LServer,LUser)->
	case catch odbc_queries:get_attention(LServer, LUser) 
	of 
		{selected,[<<"jid">>],RItems} ->
			List = lists:map(fun([Item])->
								 #xmlel{name= <<"item">>,
										attrs=[{<<"jid">>,Item}],
										children = []}
					  		 end, RItems),
			List;
		_ -> ok
	end.

	
sql_set_attention(Server,Subscriber,SubscriberdJid) ->
	odbc_queries:add_attention_list(Subscriber,SubscriberdJid,Server).
		



