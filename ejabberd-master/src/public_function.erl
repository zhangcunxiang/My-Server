%%% ====================================================================
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%%
%%% The Initial Developer of the Original Code is ProcessOne.
%%% Portions created by ProcessOne are Copyright 2006-2015, ProcessOne
%%% All Rights Reserved.''
%%% This software is copyright 2006-2015, ProcessOne.
%%%
%%%
%%% @copyright 2006-2015 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%% important note:
%% new/1 and free/2 MUST be called inside a transaction bloc

-module(public_function).
-author('zcx').
-include("jlib.hrl").
-include("logger.hrl").

-define(JPUSH_URL,"api.jpush.cn/v3/push").
-define(JPUSH_APP_KEY,"139007f13b8e4e6bf157c64f").
-define(JPUSH_MASTER_SECRET,"f346b3f8adb6b03eac2ac992").
%%-define(UPLOAD_TOKEN_URL,"http://8085.shuimin.myazure.org/refresh_jpush_token").
-define(UPLOAD_TOKEN_URL,"http://localhost:8085/refresh_jpush_token").


-export([send_jpush_push/2,send_jpush_test/0,send_device_token/3]).

%% string ,string,[binary],json
send_jpush_push(RegId,Message) ->
	RequestURL = "https://" ++ ?JPUSH_APP_KEY ++ ":" ++ ?JPUSH_MASTER_SECRET ++ 
					 "@"?JPUSH_URL,
	JpushRequest = jsx:encode([{<<"audience">>,[{<<"registration_id">>,RegId}]}|Message]),
	Request = {RequestURL, [] ,"application/json",JpushRequest},
	try httpc:request(post,Request,[],[]) of
		{ok, {{_, 200, _}, Headers, Response}} -> ok;
		Result -> Result
	catch 
		Exception ->
			lager:error("exception ~p in call to URL: ~p~n", [Exception, ?JPUSH_URL]),
			{error,Exception}
	end.

send_jpush_test() ->
	RegId = [<<"041f9ca8bb8">>],
	Message = [{<<"platform">>,<<"ios">>},{<<"notification">>,[{<<"ios">>,[{<<"alert">>,<<"你好，这是张存祥">>},{<<"extras">>,[{<<"my_key">>,<<"a value">>}]}]}]}],
	send_jpush_push(RegId,Message).

send_device_token(User,DeviceTag,JpushId)->
	?INFO_MSG("user is ~s,deviceTag is ~s,token is ~s",[User,DeviceTag,JpushId]),
	Params = lists:concat(["user=" ,User ,"&device=" ,DeviceTag,"&token=",JpushId]),
	?INFO_MSG("params is ~s",[Params]),
	inets:start(),
    ssl:start(),
    case httpc:request(post,{?UPLOAD_TOKEN_URL,  
        [],"application/x-www-form-urlencoded", 
		lists:concat(["user=" ,edoc_lib:escape_uri(User),"&device=",edoc_lib:escape_uri(DeviceTag),"&token=",edoc_lib:escape_uri(JpushId)])},[],[]) of
        {ok, {{_,200,_},_,Response}}-> Response;
        {error, Reason}->io:format("error cause ~p~n",[Reason]);
		{ok,{Error}} -> Error
    end.  
