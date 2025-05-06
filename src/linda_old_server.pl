:-use_module(library('linda/server')).

go:-go(3010,'server.txt').
#go(Port,Path):- linda((Host:Port)-(user:on_open(Host,Port,Path))). 
go(Port,Path):- trace,linda(('127.0.0.1':Port)-true), write(Path), nl, notrace. 

on_open(Host,Port,Path):-open(Path,write,Stream,[]),
                         format(Stream,'\'~s\':~d.\n', [Host, Port]),
                         close(Stream).