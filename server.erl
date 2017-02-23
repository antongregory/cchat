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

handle(St, Request) ->
	io:fwrite("state of server ~p~n", [St#server_st.servername]),
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
	case Request of
      {connect,_} ->
		 {connect,Message}=Request,
         io:fwrite("Received for connection ~p~n", [Request]),
         connectionhandler(St,Message);
       {disconnect,_} ->
         io:fwrite("Received for connection ~p~n", [Request]),
		 {reply, Response, St};
        _ ->
         io:fwrite("Nothing happened ~p~n", [Request]),
		 {reply, Response, St}
    end.


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
			FailureResponse="User already connected",
			{reply,FailureResponse,St};
		false ->
		    %UpdatedList=lists:append(CurrentUsers,[User]),
			io:fwrite("Users in list ~p~n",[User]),
			%ServerState=St#server_st{users = [ UserList]},
 			%NewState=St#server_st{servername="ServerName"},
			%io:fwrite("fab Users in list ~p~n",[NewState]),
			ServerState=St#server_st{users = [ User | St#server_st.users]},
			io:fwrite("fab Users in list ~p~n",[ServerState]),
			SucessResponse="Connection established",
			{reply,SucessResponse,ServerState}
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
