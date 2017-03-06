-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{servername=ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
%% ---------------------------------------------------------------------------

assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

handle(St, {send_job, {Fun,From,List}}) ->
    io:fwrite("in server ~n"),
    Users = St#server_st.users,
	InputList=createInputList(List),
	NewState=St#server_st{inputs = InputList},
	io:fwrite("input list ~p~n",[NewState#server_st.inputs]),
    AssignedTasks = assign_tasks(Users, InputList),
    io:fwrite("Assigned tasks to users ~p ~n", [AssignedTasks]),

	ServerId=self(),
    [spawn (fun() -> samplefunctiontosend(Pid,From,ServerId,Fun,Element) end) || {{Pid, _}, Element} <- AssignedTasks],
    %[spawn (fun() -> genserver:request(ChannelUserID, Request) end) || ChannelUserID <- St#channel_st.users, ChannelUserID =/= From],
    {reply, {job_ans,ok}, NewState};	


handle(St, {connect, Message}) ->
    io:fwrite("Server receivedthe connection request: ~p~n", [Message]),
    connectionhandler(St,Message);



%% --------------------------------------------------------------------------------------
% Handles the disconnect request from the client
% returns ok on sucessful disconnection and error in case of failure
%% --------------------------------------------------------------------------------------
handle(St,{disconnect,From}) ->
	io:fwrite("state of server ~p~n", [St#server_st.users]),
	case lists:keymember(From, 1, St#server_st.users) of
		true ->
			NewState=St#server_st{users = [{Pid,Nick} || {Pid,Nick} <- St#server_st.users, Pid =/= From]}, 		% removes the user from the list in the server state
			io:fwrite("Disconnected list ~p~n",[NewState]),
      		io:fwrite("User disconnected ~n"),
			{reply,ok,NewState};
		false ->
			{reply,error,St}
	end;

%% --------------------------------------------------------------------------------------
% Handles the join request from the client
% returns ok on sucessful joining to the channel requested and error in case of failure
%% --------------------------------------------------------------------------------------

handle(St,{join,Request})->
	{From,ChannelName}=Request,	
	io:fwrite("Handling channel request for channel name ~p~n",[St#server_st.channels]),
	Channels=list_to_atom(ChannelName),
	List=lists:member(Channels, St#server_st.channels),
	MessageForChannel={join,Request},
	case lists:member(ChannelName,St#server_st.channels) of
		
		false->
			genserver:start(list_to_atom(ChannelName), channel:initial_state(ChannelName), fun channel:handle/2),
			ServerState=St#server_st{channels = [ ChannelName | St#server_st.channels]},
			Response=genserver:request(list_to_atom(ChannelName),MessageForChannel),
			{reply, Response, ServerState};
		true ->
			io:fwrite("Channel is alive ~n"),
			ResponseFromChannel=genserver:request(list_to_atom(ChannelName),MessageForChannel),
			io:fwrite("response in join ve ~p~n",[ResponseFromChannel]),
			{reply,ResponseFromChannel, St}
	end;



handle(St,{new_state,FromId,Result})->
%% 	io:fwrite("Within new stat ~p~n",[Result]),
	NewState = St#server_st{output = [ Result | St#server_st.output ]},
%% 	io:fwrite("Within new state ~p~n",[NewState#server_st.output]),
	LenOutput=length(NewState#server_st.output)-1,
	LenInput=length(NewState#server_st.inputs),
	case LenOutput of 
		LenInput->
			NewList=lists:delete({}, NewState#server_st.output),
			SortedList=lists:keysort(1,NewList),
		    FinalList=[Value || {_,Value} <- SortedList],
			io:fwrite("sorted list ~p~n",[FinalList]),
			spawn(fun() ->genserver:request(FromId,FinalList) end),
		
			{reply,ok, St#server_st{output = [{}]}};
		_ ->
%% 			io:fwrite("not equal ..waiting for other responses ~n"),
			{reply,ok, NewState}
	end;
 	
%% --------------------------------------------------------------------------------------
% Handles the leave request from the client
% returns ok on sucessful leaving to the channel requested and error in case of failure
%% --------------------------------------------------------------------------------------
handle(St,{leave,Request})->
	{_,ChannelName}=Request,	
	io:fwrite("Handling channel request for leaving channel name ~p~n",[St#server_st.channels]),
	MessageForChannel={leave,Request},
	case lists:member(ChannelName,St#server_st.channels) of
		false->
			{reply,error, St};
		true ->
			io:fwrite("Channel is alive ~n"),
			ResponseFromChannel=genserver:request(list_to_atom(ChannelName),MessageForChannel),
			io:fwrite("response in leave"),
			{reply,ResponseFromChannel, St}
	end.


%% --------------------------------------------------------------------------------------
% function to handle the connection requests received in the server
%% {reply, Response, State}, where Reply is the reply to be sent to the client
%% and State is the  state of the server.
%% --------------------------------------------------------------------------------------
connectionhandler(St,Message) ->
	{From,Nick,_}=Message,
	User={From,Nick},
	io:fwrite("Connection handler"),
	io:fwrite("Existing users ~p~n ",[St#server_st.users]),
	case lists:keymember(From, 1, St#server_st.users) of
		false ->
			case checkNickExist(St,Nick) of 
				true->
					{reply,{error,nick_taken},St};
				false ->
					ServerState=St#server_st{users = [ User | St#server_st.users]},
					io:fwrite("fab Users in list ~p~n",[ServerState]),
					{reply,ok,ServerState}
			end;
		true ->
      		io:fwrite("User already connected"),
			{reply,{error,user_already_connected},St}
			
	end.
%% --------------------------------------------------------------------------------------
%% checks if the nick exist already in the server
%% returns true if exist and false if not
%% --------------------------------------------------------------------------------------
checkNickExist(St,Nick) ->
	case lists:keymember(Nick, 2, St#server_st.users) of
		true ->
			true;
		false ->
			false
	end.



samplefunctiontosend(Pid,From,ServerId,Fun,Element) ->
	Response=genserver:request(Pid, {apply_function, {Fun,ServerId,From, Element}}),
	Response.

createInputList(Input)->
	N=length(Input),
	IndexList=lists:seq(1,N),
	lists:zip(IndexList,Input).


