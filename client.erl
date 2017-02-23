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
	From=self(),
	Nick=St#client_st.nick,
    Request = {connect,{From,Nick,Server}},
	io:fwrite("Request to send ~p~n", [Request]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Request),
    io:fwrite("Client received: ~p~n", [Response]),
	case Response of
		"User already connected" ->
			{reply, {error, connection_exist, "User is connected already"}, St};
		"Connection established"->
			{reply, ok, St#client_st{status = disconnected}}
	end;
   

%% Disconnect from server
handle(St, disconnect) ->
	io:fwrite("disconnected ~n"),
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;



%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
 	io:fwrite("Requested whoami ~n"),
    {reply, St#client_st.nick, St} ;
    %{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
	 io:fwrite("Incoming msg"),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.