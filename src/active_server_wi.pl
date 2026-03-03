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

% Try to bind on Port; if the port is busy (ADDRINUSE), automatically retry
% on the next port up to Port+9. The actual host:port is written to Path.
go(Port,Path):-
    MaxPort is Port + 9,
    go_range(Port, MaxPort, Path).

go_range(Port, _MaxPort, Path) :-
    catch(
        linda((Host:Port)-(user:on_open(Host,Port,Path))),
        _Error,
        fail
    ), !.
go_range(Port, MaxPort, Path) :-
    Port < MaxPort,
    NextPort is Port + 1,
    go_range(NextPort, MaxPort, Path).

% Is called for store the connection information in a file at the path Path
% so that the clients can find the server to connect to.
% Always writes 'localhost' so clients on the same host never depend on DNS.
on_open(_Host,Port,Path):-open(Path,write,Stream,[]),
                          format(Stream,'\'localhost\':~d.\n', [Port]),
                          close(Stream).
