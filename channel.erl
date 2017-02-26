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
	case lists:member(From,St#channel_st.users) of
    	false ->
			io:fwrite("Flase"),
      		NewState = St#channel_st{users = [ From | St#channel_st.users ]},
			io:fwrite("State of chanel: ~p~n",[NewState]),
      		{reply,ok, NewState};
    	true ->
			io:fwrite("True"),
			{reply, {error, user_already_joined, "User joined in channel already"}, St}
		
 	end;


handle(St, {msg_from_GUI,Message}) ->
	io:fwrite("Message received in channel ~p~n",[St]),
	{From,Nick,Channel,Msg}=Message,
	 case lists:member(From,St#channel_st.users) of
    	true ->
			Response=sendmessagetousers(St,Message),
			Response;

    	false ->
			io:fwrite("User does not exist in channel"),
			{reply, {error, user_not_joined, "User not found"}, St}
		
 	end;




handle(St, {leave,Info}) ->
	io:fwrite("Leaving"),
	{From,ChannelName} = Info,
	io:fwrite("Joining ~p~n" ,[St#channel_st.users]),
 	case lists:member(From,St#channel_st.users) of
    	true ->
			NewState=St#channel_st{users = [Pid || Pid <- St#channel_st.users, Pid =/= From]},
			io:fwrite("State of chanel: ~p~n",[NewState]),
      		{reply,ok, NewState};
    	false ->
			io:fwrite("User does not exist in channel"),
			{reply, {error, user_not_joined, "User not found"}, St}
		
 	end.


sendmessagetousers(St,Request) ->
	io:fwrite("Inititiate sending"),
	{From,Nick,Msg}=Request,
	Recipients=lists:delete(From,St#channel_st.users),
	io:fwrite("rec ~p~n",[Recipients]),
	Channel=St#channel_st.channelname,
	MessageForGui={incoming_msg, Channel, Nick, Msg},
	spawn(fun() ->broadcast(Recipients, MessageForGui) end),
	{reply,ok,St}.



%% broadcast the message until the list is over
broadcast([], _) ->
  io:fwrite("Inside sending main ~n"),	
  ok;

broadcast([ Pid | T ],Msg) ->
  io:fwrite("Inside sending 1 ~p~n",[Pid]),
  {incoming_msg, Channel, Nick, Message}=Msg,
  genserver:request(Pid,Msg),
  broadcast(T,Msg).

