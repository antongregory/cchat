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

handle(St, {connect, {From, Nick}}) ->
    case lists:keymember(From, 1, St#server_st.users) of
        true ->
            {reply, connection_exists, St};
        false ->
            case lists:keymember(Nick, 2, St#server_st.users) of
                true ->
                    {reply, nick_taken, St};
                false ->
                    User = {From, Nick},
                    NewSt = St#server_st{users = [User | St#server_st.users]},
                    {reply, ok, NewSt}
            end
    end;

handle(St, {disconnect, {Nick, ClientId}}) ->
    case ClientId#client_st.channels =:= [] of
        true ->
            NewSt = St#server_st{users = [Name || Name <- St#server_st.users, Name =/= Nick]},
            {reply, ok, NewSt};
        false ->
            {reply, leave_channel_first, St}
    end;

handle(St, {join, {From, Channel}}) ->
    case lists:member(Channel, St#server_st.channels) of
        true ->
            Response = genserver:request(list_to_atom(Channel), {join, {From}}),
            {reply, Response, St};
        false ->
            genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
            NewSt = St#server_st{channels = [Channel | St#server_st.channels]},
            Response = genserver:request(list_to_atom(Channel), {join, {From}}),
            {reply, Response, NewSt}
    end.

