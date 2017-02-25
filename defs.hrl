% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(client_st, {gui,server,status=disconnected,nick}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {servername, users = [{}], channels = []}).

% This record is for handling the channel parameters
-record(channel_st,{channelname,users=[]}).
