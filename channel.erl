-module(channel).
-export([initial_state/1, handle/2]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
  #channel_st{channelName = ChannelName, users = []}.

handle(St, Request) ->
  case Request of
    {join, Info} ->
      joinHandle(St, Info);
    {leave, Info} ->
      leaveHandle(St, Info);
    {msg_from_gui, Info} ->
      msgHandle(St, Info)
  end.

joinHandle(St, {From, User, Channel}) ->
  case lists:keymember(User#client_st.gui, 2, St#channel_st.users) of
    true ->
      {reply, "User already joined", St};
    false ->
      NewSt = St#channel_st{users = [User | St#channel_st.users]},
      {reply, "Channel joined", NewSt}
  end.

leaveHandle(St, User) ->
  case lists:keymember(User#client_st.gui, 2, St#channel_st.users) of
    true ->
      NewSt = St#channel_st{users = [Pid || Pid <- St#channel_st.users, Pid =/= User]},
      {reply, "User left", NewSt};
    false ->
      {reply, "User not joined", St}
  end.

msgHandle(St, {User, Channel, Msg}) ->
  case lists:keymember(User#client_st.gui, 2, St#channel_st.users) of
    true ->
      Request = {incoming_msg, Channel, User#client_st.nick, Msg},
      [client:handle(ChannelUserID, Request) || ChannelUserID <- St#channel_st.users, ChannelUserID =/= User],
      {reply, "User in channel", St};
    false ->
      {reply, "User not joined", St}
  end.