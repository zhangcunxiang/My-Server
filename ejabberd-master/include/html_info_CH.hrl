%% coding: utf-8
%% For this file we have chosen encoding = utf-8
%% -*- coding: utf-8 -*-
%% @author Archer
%% @doc


%% MM 单人消息推送
-define(Message_text_CH(UserName),io_lib:format("~ts发来文字消息。",[UserName])).
-define(Message_picture_CH(UserName),io_lib:format("~ts发来图片消息。",[UserName])).
-define(Message_voice_CH(UserName),io_lib:format("~ts发来语音消息。",[UserName])).
-define(Message_location_CH(UserName),io_lib:format("~ts发来地址消息。",[UserName])).
-define(Message_combine_CH(UserName),io_lib:format("~ts发来复合消息。",[UserName])).
-define(Message_message_CH(UserName),io_lib:format("~ts发来消息。",[UserName])).

%% MM 多人消息推送 （群组/讨论组)
-define(Group_Msg_CH(GName),io_lib:format("~ts(群)有新消息", [GName])).
-define(Topic_Msg_CH(TName),io_lib:format("~ts(讨论组)有新消息", [TName])).

