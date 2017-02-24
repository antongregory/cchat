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




handle(St, {join,Request}) ->
	io:fwrite("Joining "),
	{From,ChannelName} = Request,
	io:fwrite("Joining ~p~n" ,[ChannelName]),
 	case lists:keymember(From, 1, St#channel_st.users) of
    	false ->
			io:fwrite("Flase"),
      		NewState = St#channel_st{users = [ From | St#channel_st.users ]},
			io:fwrite("Flase ~p~n",[NewState]),
      		{reply,ok, NewState};
    	true ->
			io:fwrite("True"),
			{reply, {error, user_already_joined, "User joined"}, St}
		
 	end;


handle(St, {leave,ChannleName}) ->
	io:fwrite("Joining "),
	{reply,ok,St}.

