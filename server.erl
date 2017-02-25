-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName, users = [], channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, Request) ->
    case Request of
        {connect, Info} ->
            connectHandler(St, Info);
        {disconnect, Info} ->
            disconnectHandler(St, Info);
        {join, Info} ->
            joinHandler(St, Info)
    end.

connectHandler(St, {From, Nick, Server}) ->
    case lists:member(Nick, St#server_st.users) of
        true ->
            {reply, "User already connected", St};
        false ->
            NewSt = St#server_st{users = [Nick | St#server_st.users]},
            {reply, "Connection established", NewSt}
    end.

disconnectHandler(St, {From, Nick, ClientId}) ->
    case ClientId#client_st.channels =:= [] of
        true ->
            case lists:member(Nick, St#server_st.users) of
                true ->
                    NewSt = St#server_st{users = [Name || Name <- St#server_st.users, Name =/= Nick]},
                    {reply, "User disconnected", NewSt};
                false ->
                    {reply, "User not connected", St}
            end;
        false ->
            {reply, "Leave channels first", St}
    end.

joinHandler(St, {From, User, Channel}) ->
    case lists:member(Channel, St#server_st.channels) of
        true ->
            Request = {From, User, Channel},
            Response = genserver:request(list_to_atom(Channel), {join, Request}),
            {reply, Response, St};
        false ->
            genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
            NewSt = St#server_st{channels = [Channel | St#server_st.channels]},
            Request = {From, User, Channel},
            Response = genserver:request(list_to_atom(Channel), {join, Request}),
            {reply, Response, NewSt}
    end.


