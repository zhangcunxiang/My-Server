%% @author Alfred
%% @doc @todo Add description to mod_search.


-module(mod_search).

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
				  ?NS_SEARCH, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_SEARCH).

process_local_iq(_From, _To,
		 #iq{type = Type, sub_el = SubEl} = IQ) ->
	case Type of
      set ->
	  	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
		  IQ#iq{type = result,
			sub_el =
			    [#xmlel{name = <<"query">>,
				    attrs = [{<<"xmlns">>, ?NS_SEARCH}],
				    children =
					[#xmlel{name = <<"tzo">>, attrs = [],
						children = [{xmlcdata, TZO_diff}]}
					]}]}
	end.


