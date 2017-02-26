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

handle(St, {connect, Message}) ->
	io:fwrite("state of server ~p~n", [St#server_st.servername]),
    io:fwrite("Server received: ~p~n", [Message]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    connectionhandler(St,Message);

handle(St,{disconnect,From}) ->
	io:fwrite("state of server ~p~n", [St#server_st.users]),
    Response = "disconectin ",
	case lists:keymember(From, 1, St#server_st.users) of
		true ->
			NewUsers=lists:keydelete(From, 1,St#server_st.users),
			NewState=St#server_st{users = [{Pid,Nick} || {Pid,Nick} <- St#server_st.users, Pid =/= From]},
			io:fwrite("Disconnected list ~p~n",[NewState]),
      		io:fwrite("User disconnected ~n"),
			{reply,ok,NewState};
		false ->
			{reply,error,St}
	end;


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

handle(St,{leave,Request})->
	{From,ChannelName}=Request,	
	io:fwrite("Handling channel request for leaving channel name ~p~n",[St#server_st.channels]),
	Channels=list_to_atom(ChannelName),
	MessageForChannel={leave,Request},
	case lists:member(ChannelName,St#server_st.channels) of
		false->
			{reply,error, St};
		true ->
			io:fwrite("Channel is alive ~n"),
			ResponseFromChannel=genserver:request(list_to_atom(ChannelName),MessageForChannel),
			io:fwrite("response in leave"),
			{reply,ResponseFromChannel, St}
	end;

handle(St, {msg_from_GUI,Request}) ->
	{From,Nick,Channel,Msg}=Request,
	ChannelRequest={From,Nick,Msg},
	io:fwrite("GUI ~p~n",[Channel]),
	Response=genserver:request(list_to_atom(Channel),{msg_from_GUI,ChannelRequest}),
    {reply, Response, St};

handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} .


% function to handle the connection requests received in the server
%% {reply, Response, State}, where Reply is the reply to be sent to the client
%% and State is the  state of the server.
connectionhandler(St,Message) ->
	{From,Nick,Server}=Message,
	User={From,Nick},
	io:fwrite("Connection handler"),
	UserList=[User],
	io:fwrite("Format ~p~n ",[UserList]),
	Response="failure",
	CurrentUsers=St#server_st.users,
	Sample=list_to_atom("lol"),
	io:fwrite("Existing users ~p~n ",[St#server_st.users]),
	case lists:keymember(From, 1, St#server_st.users) of
		true ->
      		io:fwrite("User already connected"),
			{reply,error,St};
		false ->
			ServerState=St#server_st{users = [ User | St#server_st.users]},
			io:fwrite("fab Users in list ~p~n",[ServerState]),
			{reply,ok,ServerState}
	end.


% Sample function trying to send message from server to client

sendresponse(St,From) ->
	Response="sample",
	From ! {disconnect,Response},
	io:fwrite("Msg summ"),
    Ref = make_ref(),
	Data=disconnect,
    io:fwrite("connection check ~p ~p ~p ~p ~p ~n",[From,request,self(),Ref,Data]),
    From!{request, self(), Ref, Data},
 	%genserver:request(From,disconnect,100000),
	io:fwrite("Msg sent as response").
