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
			io:fwrite("State of chanel: ~p~n",[NewState]),
      		{reply,ok, NewState};
    	true ->
			io:fwrite("True"),
			{reply, {error, user_already_joined, "User joined"}, St}
		
 	end;


handle(St, {msg_from_GUI,Message}) ->
	io:fwrite("Message received in channel ~p~n",[St]),
	Response=sendmessagetousers(St,Message),
	{reply,Response,St};




handle(St, {leave,ChannleName}) ->
	io:fwrite("Joining "),
	{reply,ok,St}.

sendmessagetousers(St,Request) ->
	io:fwrite("Inititiate sending"),
	{From,Nick,Msg}=Request,
	Recipients=lists:delete(From,St#channel_st.users),
	io:fwrite("rec ~p~n",[Recipients]),
	Channel=St#channel_st.channelname,
	MessageForGui={incoming_msg, Channel, Nick, Msg},
	spawn(fun() ->broadcast(Recipients, MessageForGui) end),
	io:fwrite("MessageForGui of ~p~n",[MessageForGui]),
	%ResponseFromChannel=genserver:request(Head,MessageForGui),
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

