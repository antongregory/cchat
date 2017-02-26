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
%% requesting process and NewState is the new state of the client.

	%% Connect to server
	handle(St, {connect, Server}) ->
	io:fwrite("St#client_st.status ~p~n" ,[St#client_st.status]),
	case St#client_st.status of
		connected ->
			{reply, {error, connection_exist, "User is connected already"}, St};
		disconnected ->
			connectionhandler(St,{connect,Server});
		_ ->
			io:fwrite("Nothing happened"),
			{reply, {error, connection_error, "Error while connecting"}, St}
	end;
		
   

%% Disconnect from server
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
			{error,connection_error,Msg}->
				{reply, {error, connection_error, Msg}, St} ;
			error->
				{reply, {error, error_in_disconnecting, "Error in disconnecting"}, St}
		end;
	 _ ->
		 {error,leave_channels_first,"Leave channels first"}
 	end;


% Join channel
handle(St, {join, Channel}) ->
	io:fwrite("Channel name ~p~n",[Channel]),
	Request = {self(),Channel},
	ServerAtom=St#client_st.server,
	io:fwrite("Format ~p~n",[ServerAtom]),
	Response=clientrequesthandler(St,{join,Request}),
	case Response of 
		ok->
			ClientState=St#client_st{channels = [ Channel | St#client_st.channels]},
			{reply, ok, ClientState};
		{error,connection_error,Msg}->
			 {reply, {error, user_not_connected,Msg}, St} ;
		{error,user_already_joined,Msg}->
			 {reply, {error, user_already_joined, Msg}, St} 

	end;


    %{reply, {error, not_implemented, "Not implemented"}, St} ;



%% Leave channel
handle(St, {leave, Channel}) ->
	Request={self(),Channel},
	Response=clientrequesthandler(St,{leave,Request}),
	io:fwrite("User leave Format ~p~n",[Response]),
	case Response of 
		ok->
			NewState=St#client_st{channels = [CName || CName <- St#client_st.channels, CName =/= Channel]},
			{reply, ok, St};
		{error,connection_error,Msg}->
			 {reply, {error, user_not_connected, Msg}, St} ;
		{error,user_not_joined,Msg}->
			 {reply, {error, user_not_joined, Msg}, St} 

	end;






% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
	io:fwrite("Message from gui "),
	Request={self(),St#client_st.nick,Channel,Msg},
	Response=clientrequesthandler(St,{msg_from_GUI,Request}),
	io:fwrite("Format ~p~n",[Response]),
	case Response of 
		ok->
			{reply, ok, St};
		{error,connection_error,Msg}->
			 {reply, {error, no_connection, "No connection"}, St} ;
		error->
			 {reply, {error, not_implemented, "Not implemented"}, St} 

	end;

    %{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
 	io:fwrite("Requested whoami ~n"),
    {reply, St#client_st.nick, St} ;


%% Change nick
handle(St, {nick, Nick}) ->
	State=St#client_st{nick = Nick},
	case St#client_st.status of
		connected ->
			{reply, ok,State};
			 %{reply, {error, user_connected, "User connected already"}, St} ;			 	
		disconnected ->
			 %register(Nick,self()),
			 {reply, ok,State}
	end;
	
	
   

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Nick, Msg}) ->
	io:fwrite("Inside client"),
	MessageToGui=Nick++ ": "++Msg,
	io:fwrite("Incoming msg ~p~p~p ~p~n",[Channel,Nick,MessageToGui,GUIName]),
	spawn(fun() ->sendToGui(GUIName, Channel, MessageToGui) end),
    %Response=gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel,Msg}),
	%io:fwrite("res ~p~n",[Response]),
    {reply, ok, St}.


sendToGui(GUI,Channel,Msg)->
	gen_server:call(list_to_atom(GUI), {msg_to_GUI, Channel,Msg}).

clientrequesthandler(St,ServerRequest)->
	ServerAtom=St#client_st.server,
	case St#client_st.status of
		connected ->
			io:fwrite("Request sent to server ~p~n",[ServerRequest]),
			genserver:request(ServerAtom,ServerRequest);
		disconnected ->
			{error,connection_error,"Connect to server before proceeding"}

end.



connectionhandler(St,{connect,Server}) ->
	From=self(),
	Nick=St#client_st.nick,
    Request = {connect,{From,Nick,Server}},
	io:fwrite("Request to send ~p~n", [Request]),
	ServerAtom=list_to_atom(Server),
	case lists:member(ServerAtom, registered()) of 
		true ->
			Response = genserver:request(ServerAtom, Request),
			io:fwrite("Client received: ~p~n", [Response]),
				case Response of
					error ->
						{reply, {error, connection_exist, "User is connected already"}, St};
					ok->
						{reply, ok, St#client_st{server=ServerAtom,status = connected}}
				end;
		false ->	
			{reply, {error, server_error, "Start server"}, St}
	end.

