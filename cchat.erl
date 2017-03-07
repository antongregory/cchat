% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0, send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% =========================================================================================
%% Sends the function to be performed to the server
%% Checks if the server is a registered process before sending , else displays error message
%% The return status of from the server is checked to know if all the clients are connected
%% else throws error
%% =========================================================================================
send_job(Server, Fun, L) ->
    ServerAtom = list_to_atom(Server),
    Request = {send_job, {Fun,self(), L}},
	case lists:member(ServerAtom, registered()) of 
		true->
			{job_ans,Status}=genserver:request(ServerAtom, Request),
			io:fwrite("Status of assign job request to server: ~p~n",[Status]),	
				case Status of 
					ok->
						Response=receiveResponse(),
						io:fwrite("Response from the server: ~p~n",[Response]),
						Response;
					{error,Msg}->
						Msg
				end;
    	false->
			"Error:Server Not Reachable"
			
	end.
		

	
%% ===================================================================================================
%% This function receives the message sent by the server
%% ===================================================================================================
receiveResponse()->
	io:fwrite("==============================================~n"),
	receive
		{request,_,_,Result}->
			Result
	end.	

