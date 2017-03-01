-module(channel).
-export([initial_state/1, handle/2]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
  #channel_st{channelName = ChannelName, users = []}.

handle(St, {join, {From}}) ->
  case lists:member(From, St#channel_st.users) of
    true ->
      {reply, "User already joined", St};
    false ->
      NewSt = St#channel_st{users = [From | St#channel_st.users]},
      {reply, "Channel joined", NewSt}
  end;

handle(St, {leave, From}) ->
  case lists:member(From, St#channel_st.users) of
    true ->
      NewSt = St#channel_st{users = [Pid || Pid <- St#channel_st.users, Pid =/= From]},
      {reply, "User left", NewSt};
    false ->
      {reply, "User not joined", St}
  end;

handle(St, {msg_from_gui, {From, Nick, Channel, Msg}}) ->
  case lists:member(From, St#channel_st.users) of
    true ->
      Request = {incoming_msg, Channel, Nick, Msg},
      [spawn (fun() -> genserver:request(ChannelUserID, Request) end) || ChannelUserID <- St#channel_st.users, ChannelUserID =/= From],
      {reply, "User in channel", St};
    false ->
      {reply, "User not joined", St}
  end.
