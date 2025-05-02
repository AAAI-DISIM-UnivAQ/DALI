% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Load the package required from Linda-server
:-use_module(library('linda/server')).

% Starts a Linda-server listener in this SICStus Prolog instance. Example: linda((Host:Port)-Goal)
% When connection is made, Host and Port are bound to  
% the server  host and port respectively and Goal is called.
go:-go(3010,'server.txt').
go(Port,Path):- linda((Host:Port)-(user:on_open(Host,Port,Path))). 

% Is called for store the connection information in a file at the path Path
% so that the clients can find the server to connect to.
on_open(Host,Port,Path):-open(Path,write,Stream,[]),
                         format(Stream,'\'~s\':~d.\n', [Host, Port]),
                         close(Stream).
