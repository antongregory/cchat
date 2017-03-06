-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.
%%



%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName,nick=Nick }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client
%% @returns the response received from ther server or channel based on what the function is about

handle(St, {apply_function, {Fun,Pid, List}}) ->
     io:fwrite("in client ~p ~p ~n",[self(),List]),
	 {Index,Element}=List,
%% 	 In=[Elements|{Index,Element}<-List],
	 io:fwrite("input received in client ~p~n",[Element]),
 	 io:fwrite("function in client ~p ~p ~n",[self(),Fun(Element)]),
	 Result={Index,Fun(Element)},
	 genserver:request(Pid,{new_state,Result}),
	 
	%io:fwrite("in client ~p~n",Fun(List)),
       {reply, {result,self(),Result}, St};

handle(St, {send_job, {Fun, L}}) ->
    io:fwrite("hej");


%% Connection request is sent to the server
	handle(St, {connect, Server}) ->
	io:fwrite("St#client_st.status ~p~n" ,[St#client_st.status]),
	case St#client_st.status of
		connected ->
			{reply, {error, connection_exist, "User is connected already"}, St};
		disconnected ->
			Response =connectionhandler(St,{connect,Server}),
			io:fwrite("Response of  connection ~p~n",[Response]),
			Response;
		_ ->
			io:fwrite("Invalid client status"),
			{reply, {error, connection_error, "Error while connecting"}, St}
	end;
		
   

%% ---------------------------------------------------------------------------
%% Disconnect from server 
%% All the processing related to disconnecting from server are handled here
%% If the client is connected to channels then the client is not allowed to disconnect until the client leaves the channels
%% If the user is not connected , send back the error message to gui else send ok
%% ---------------------------------------------------------------------------


handle(St, disconnect) ->
	io:fwrite("leng of list ~p~n",[length(St#client_st.channels)]),
	case length(St#client_st.channels) of 
		 0 ->
			 Response=clientrequesthandler(St,{disconnect,self()}),
			io:fwrite("disconnected ~p~n",[Response]),
			case Response of 
				ok->
					NewState=St#client_st{status=disconnected},
					{reply, ok, NewState};
				{error,user_not_connected,Msg}->
					{reply, {error, user_not_connected, Msg}, St} ;
				error->
					{reply, {error, error_in_disconnecting, "Error in disconnecting"}, St}
			end;
		 _ ->
			 {reply,{error,leave_channels_first,"Leave channels first"}, St}
	 	end;

%% ---------------------------------------------------------------------------
% Join channel
%% {join,Channel} join is the pattern here and Channel is the name of the channel requested by the user
%% Sends a request to the server to join the channel
%  ok indicates sucessful joining and error indicates failure

%% ---------------------------------------------------------------------------
handle(St, {join, Channel}) ->
	io:fwrite("Channel name ~p~n",[Channel]),
	Request = {self(),Channel},
	ServerAtom=St#client_st.server,
	io:fwrite("Format ~p~n",[ServerAtom]),
	Response=clientrequesthandler(St,{join,Request}),
	io:fwrite("Resoinse of join request ~p~n",[Response]),
	case Response of 
		ok->
			ClientState=St#client_st{channels = [ Channel | St#client_st.channels]},
			{reply, ok, ClientState};
		{error,user_not_connected,Msg}->
			 {reply, {error, user_not_connected,Msg}, St} ;
		{error,user_already_joined,Msg}->
			 {reply, {error, user_already_joined, Msg}, St} 

	end;

%% ---------------------------------------------------------------------------
%% Leave channel
%% {leave,Channel} leave is the pattern here and Channel is the name of the channel requested by the user
%% Sends a request to the server to leave the channel
%  ok indicates sucessful leaving  and error indicates failure in request
%% ---------------------------------------------------------------------------
handle(St, {leave, Channel}) ->
	Request={self(),Channel},
	Response=clientrequesthandler(St,{leave,Request}),
	io:fwrite("User leave Format ~p~n",[Response]),
	case Response of 
		ok->
			NewState=St#client_st{channels = [CName || CName <- St#client_st.channels, CName =/= Channel]},
			{reply, ok, NewState};
		{error,connection_error,Msg}->
			 {reply, {error, user_not_connected, Msg}, St} ;
		{error,user_not_joined,Message}->
			 {reply, {error,user_not_joined, Message}, St} 

	end;





%%  ---------------------------------------------------------------------------
%% Sending messages
%% Sends a request to the send message to the users connected to the channel

%% Input : {msg_from_GUI,Channel,Msg}  pattern for sending msgs from gui,Channel- Name of the cn
%% ok indicates sucessful sending of msgs  and error indicates failure in request
%%  ---------------------------------------------------------------------------

handle(St, {msg_from_GUI, Channel, Msg}) ->
	io:fwrite("Message from gui "),
	Request={self(),St#client_st.nick,Msg},
	MessageForChannel={msg_from_GUI,Request},
	Response=channelrequesthandler(St,Channel,MessageForChannel),
	io:fwrite("Format ~p~n",[Response]),
	case Response of 
		ok->
			{reply, ok, St};
		{error,connection_error,Msg}->
			 {reply, {error, no_connection, "No connection"}, St} ;
		{error,user_not_joined,Message}->
			 {reply, {error, user_not_joined, Message}, St} ;
		error->
			 {reply, {error, not_implemented, "Not implemented"}, St} 

	end;

%%  ---------------------------------------------------------------------------
%% Get current nick
%% returns the nick name of the user
%%  ---------------------------------------------------------------------------
handle(St, whoami) ->
 	io:fwrite("Requested whoami ~n"),
    {reply, St#client_st.nick, St} ;


%%  ---------------------------------------------------------------------------
%% Change the nickname of the user
%% Cant change the nickname if the user is connected

%% input nick -> nickname requested by the user
%% returns ok-> indicates sucessful sending of msgs  and error-> indicates failure in request 
%%  ---------------------------------------------------------------------------
handle(St, {nick, Nick}) ->
	io:fwrite("Format ~p~n ",[ St#client_st.status]),
	case St#client_st.status of
		disconnected ->
			State=St#client_st{nick = Nick},
			{reply, ok,State};	 	
		connected ->
			 {reply, {error, user_already_connected, "User connected already"}, St} 
	end;
	
	
   

%%  ---------------------------------------------------------------------------
%% Incoming message from channel to the gui

%% input nick -> nickname of the user that sends message , Channel -> intended channel of the message
%%		 Msg- Msg send by the user connected in the channel	
%% returns ok-> indicates sucessful sending of msgs  and error-> indicates failure in request 
%%  ---------------------------------------------------------------------------
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Nick, Msg}) ->
	io:fwrite("Inside client"),
	MessageToGui=Nick++ "> "++Msg,
	io:fwrite("Incoming msg ~p~p~p ~p~n",[Channel,Nick,MessageToGui,GUIName]),
	sendToGui(GUIName, Channel, MessageToGui),
    {reply, ok, St}.

%% Calls the function in gen_server module to display the message in the channel

sendToGui(GUI,Channel,Msg)->
	gen_server:call(list_to_atom(GUI), {msg_to_GUI, Channel,Msg}).


%%  ---------------------------------------------------------------------------
%% Utility function to send the request from the client

%% @param ServerRequest - Request to send to the server
%% checks if the client is connected or not before making a request
%%  ---------------------------------------------------------------------------
clientrequesthandler(St,ServerRequest)->
	ServerAtom=St#client_st.server,
	case St#client_st.status of
		connected ->
			io:fwrite("Request sent to server ~p~n",[ServerRequest]),
			genserver:request(ServerAtom,ServerRequest);
		disconnected ->
			{error,user_not_connected,"Connect to server before proceeding"}

end.

%%  ---------------------------------------------------------------------------
%% Utility function to send the request from the client to the channel

%% @param Channel - Name of the channel for which the message should be sent
%% checks if the client is connected or not before making a request
%%  ---------------------------------------------------------------------------
channelrequesthandler(St,Channel,Message)->
	case St#client_st.status of
		connected ->
			io:fwrite("Request sent to channel ~p~n",[Message]),
			genserver:request(list_to_atom(Channel),Message);
		disconnected ->
			{error,user_not_connected,"Connect to server before proceeding"}

end.



%%  ---------------------------------------------------------------------------
%% Utility function to handle the connection request from the client to the server
%% @param Server - Name of the server requested by the client to connect
%% checks if the server is registered before intiating the request
%%  ---------------------------------------------------------------------------

connectionhandler(St,{connect,Server}) ->
	From=self(),
	Nick=St#client_st.nick,
    Request = {connect,{From,Nick,Server}},
	io:fwrite("Request to send ~p~n", [Request]),
	ServerAtom=list_to_atom(Server),
	case lists:member(ServerAtom, registered()) of 
		true ->
			waitforconnection(St,ServerAtom,Request); 
		false ->	
			{reply, {error, server_not_reached, "Server not reached"}, St}
	end.

%%  ---------------------------------------------------------------------------
%% Utility function to handle the timeout error when sening request to the server
%% @param ServerAtom - Name of the server requested by the client to connect
%% catches and handles the timeout exception exits
%%  ---------------------------------------------------------------------------
waitforconnection(St,ServerAtom,Request) ->
	try genserver:request(ServerAtom, Request) of
		error ->
			{reply, {error, connection_exist, "User is connected already"}, St};
		ok->
			NewState=St#client_st{server=ServerAtom,status = connected},
			{reply, ok, NewState};
		{error,nick_taken} ->
			{reply, {error, nick_taken, "Nick name exist already"}, St}
	catch
     	exit:"Timeout" -> 
			{reply, {error, server_not_reached, "Server not reached"}, St}

	end.

