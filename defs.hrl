% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(client_st, {gui,server,status=disconnected,nick,channels=[]}).

% This record defines the structure of the server process.
-record(server_st, {servername, users = [], channels = [],inputs=[],output=[]}).

% This record is for handling the channel process
-record(channel_st,{channelname,users=[]}).