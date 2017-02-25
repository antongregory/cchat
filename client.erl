-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { nick = Nick, gui = GUIName, status = disconnected, channels = [] }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    From = self(),
    Nick = St#client_st.nick,
    Request = {connect, {From, Nick, Server}},
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Request),

    case Response of
        "Connection established" ->
            {reply, ok, St#client_st{status = connected}};
        "User already connected" ->
            {reply, {error, connection_exist, "User already connected"} ,St}
    end ;

%% Disconnect from server
handle(St, disconnect) ->
    From = self(),
    Nick = St#client_st.nick,
    Request = {disconnect, {From, Nick, St}},
    ServerAtom = list_to_atom("shire"),
    Response = genserver:request(ServerAtom, Request),

    case Response of
        "User disconnected" ->
            {reply, ok, St#client_st{status = disconnected}};
        "User not connected" ->
            {reply, {error, user_not_connected, "User not connected"}, St};
        "Leave channels first" ->
            {reply, {error, leave_channels_first, "Leave channels first"}, St}
    end;

% Join channel
handle(St, {join, Channel}) ->
    From = self(),
    Request = {join, {From, St, Channel}},
    Response = genserver:request(list_to_atom("shire"), Request),

    case Response of
      "Channel joined" ->
        {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
      "User already joined" ->
        {reply, {error, user_already_joined, "User already joined"}, St}
    end;
%% Leave channel
handle(St, {leave, Channel}) ->
    Request = {leave, St},
    ChannelAtom = list_to_atom(Channel),
    Response = genserver:request(ChannelAtom, Request),

    case Response of
        "User left" ->
            {reply, ok, St#client_st{channels = [Channels || Channels <- St#client_st.channels, Channels =/= Channel]}};
        "User not joined" ->
            {reply, {error, user_not_joined, "User not joined"}, St}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    ChannelAtom = list_to_atom(Channel),
    Request = {msg_from_gui, {St, Channel, Msg}},
    Response = genserver:request(ChannelAtom, Request),
    case Response of
        "User in channel" ->
            {reply, ok ,St};
        "User not joined" ->
            {reply, {error, user_not_joined, "User not joined"}, St}
    end;
    %{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    case St#client_st.status of
        disconnected ->
            {reply, ok, St#client_st{nick = Nick}};
        connected ->
            {reply, {error, user_already_connected, "User already connected", St}}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.