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


%% ====================================================================
% Handles all the incoming request to the channel process
%% ====================================================================
handle(St, {join,Request}) ->
	io:fwrite("Joining "),
	{From,ChannelName} = Request,
	io:fwrite("Joining ~p~n" ,[ChannelName]),
	case lists:member(From,St#channel_st.users) of
    	false ->
			io:fwrite("Flase"),
      		NewState = St#channel_st{users = [ From | St#channel_st.users ]},		% adds the user who issued join command
			io:fwrite("State of chanel: ~p~n",[NewState]),
      		{reply,ok, NewState};
    	true ->
			io:fwrite("True"),
			{reply, {error, user_already_joined, "User joined in channel already"}, St}
		
 	end;
%% ======================================================================================
%%  Handles all the request for message sent from gui to the other users in the channel
%% Checks if the from id of the message exist in the channel , if so broadcasts the message
%% to the users in the channel
%% ======================================================================================

handle(St, {msg_from_GUI,Message}) ->
	io:fwrite("Message received in channel ~p~n",[Message]),
	{From,_,_}=Message,
	 case lists:member(From,St#channel_st.users) of
    	true ->
			Response=sendmessagetousers(St,Message),
			io:fwrite("Response for the caller ~p~n",[Response]),
			Response;

    	false ->
			io:fwrite("User does not exist in channel"),
			{reply, {error, user_not_joined, "User not found"}, St}
		
 	end;

%% ======================================================================================
%% Handles all the request for removing the user from the channle
%% Checks if the from user exist in the channel 
%% ======================================================================================

handle(St, {leave,Info}) ->
	io:fwrite("Leaving"),
	{From,_} = Info,
	io:fwrite("Joining ~p~n" ,[St#channel_st.users]),
 	case lists:member(From,St#channel_st.users) of
    	true ->
			NewState=St#channel_st{users = [Pid || Pid <- St#channel_st.users, Pid =/= From]},   	% removes the user who issued leave command
      		{reply,ok, NewState};
    	false ->
			io:fwrite("User does not exist in channel"),
			{reply, {error,user_not_joined, "User not found"}, St}
		
 	end.
%% ======================================================================================
%% Function to send message to the users in the channel
%% ======================================================================================
sendmessagetousers(St,Request) ->
	io:fwrite("Inititiate sending"),
	{From,Nick,Msg}=Request,
	Recipients=lists:delete(From,St#channel_st.users),
	io:fwrite("rec ~p~n",[Recipients]),
	Channel=St#channel_st.channelname,
	MessageForGui={incoming_msg, Channel, Nick, Msg},
	spawn(fun() ->broadcast(Recipients, MessageForGui) end),			% creates a new process to broadcast the message
	{reply,ok,St}.



%% ======================================================================================
%% sends the message to the recipients in the list until the list is over
%% input-> list of recipients ; message- message to be broadasted
%% ======================================================================================
broadcast([], _) ->
  io:fwrite("Inside sending main ~n"),	
  ok;

broadcast([ Pid | T ],Msg) ->
  io:fwrite("Inside sending 1 ~p~n",[Pid]),
  genserver:request(Pid,Msg),
  broadcast(T,Msg).

