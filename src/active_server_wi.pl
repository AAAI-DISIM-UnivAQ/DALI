:-use_module(library('linda/server')),use_module(library('linda/client')),use_module(library('lists')).


%When the linda server is started, Host and Port are bound to the server host and port respectively and the %%%goal Goal is called. A typical use of this would be to store the connection information in a file so that    %the clients can find the server to connect to. 
%

% linda(Address-Goal)

go(Port,Path):-linda((Host:Port)-(user:my_pred(Host,Port,Path))). 


% Goal è usato per scrivere su un file informazioni per permettere ai clients di trovare il server

my_pred(Host,Port,Path):-open(Path,write,Stream,[]),
              write(Stream,'\''),write(Stream,Host),write(Stream,'\''),
              write(Stream,':'),write(Stream,Port),
              write(Stream,'.'),nl(Stream),
              close(Stream).

%CARICA UNA ONTOLOGIA 
carica_ontologia(F):-see(F), 
	     repeat,
		read(T),
                            if(T=end_of_file,true,
                            out(T)),
                		T == end_of_file,
	     !,
	     seen.


