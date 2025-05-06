:- module(linda_server, [start_linda_server/0, server_loop/0]).

:- use_module(library('linda/server')).
:- use_module(library(system)).

% Predicati di trace
trace_linda_server(Message) :-
    write('DEBUG [Linda Server]: '), write(Message), nl.

% Inizializzazione del server
start_linda_server :-
    trace_linda_server('Avvio del server Linda'),
    catch(
        (trace_linda_server('Tentativo di avvio server sulla porta 3010'),
         linda(('localhost':3010)-true),
         trace_linda_server('Server Linda avviato sulla porta 3010')),
        Error,
        (trace_linda_server('Errore nell\'avvio del server'), 
         write('Errore dettagliato: '), write(Error), nl, fail)
    ),
    trace_linda_server('In attesa di connessioni...'),
    trace_linda_server('Server pronto per il loop').

