%% coding: utf-8
%% For this file we have chosen encoding = utf-8
%% -*- coding: utf-8 -*-
%% @author Archer
%% @doc



%% MM 单人消息推送
-define(Message_text_EN(UserName),io_lib:format("Text from ~ts", [UserName])).
-define(Message_picture_EN(UserName),io_lib:format("Photo from ~ts", [UserName])).
-define(Message_voice_EN(UserName),io_lib:format("Voice message from ~ts", [UserName])).
-define(Message_location_EN(UserName),io_lib:format("Location from ~ts", [UserName])).
-define(Message_combine_EN(UserName),io_lib:format("Multi-type message from ~ts", [UserName])).
-define(Message_message_EN(UserName),io_lib:format("Message from ~ts", [UserName])).

%% MM 多人消息推送 （群组/讨论组)
-define(Group_Msg_EN(GName),io_lib:format("New message from ~ts(group)", [GName])).
-define(Topic_Msg_EN(TName),io_lib:format("New message from ~ts(topic)", [TName])).

