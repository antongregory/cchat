%% @author antongregory
%% @doc @todo Add description to channel.


-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").



%% ====================================================================
%% Internal functions
%% ====================================================================

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{channelname=ChannelName}.


handle(St, Request) ->
	io:fwrite("Trying Joining ~p~n",[Request]),
	 case Request of 
		{join,_}->
			io:fwrite("Joining "),
			{reply,ok,St};
		_ ->
			{reply,ok,St}
		end.
