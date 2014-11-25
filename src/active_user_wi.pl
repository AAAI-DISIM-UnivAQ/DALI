% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Load the package required
:-use_module(library('linda/client')),
  use_module(library(fdbg)).


%%%%%%%%%%%
%% Procedure loop that keeps this agent continuously active and waiting for user interaction. 
%% When user types a message an event is triggered, the agent will be associate a timestamp for the unicity. 
%% Once the message is written on the blackboard will be discarded.
%%%%%%%%%%%
procedure_message(I):-nl, print('New message'),nl,
                      print('Insert name of addressee'), nl, read(D),
                      print('Insert From'), nl, read(From),
                      print('Insert message'), nl, read(M),
                      variables(M), clause(result_format(F),_),
                      out(message(I,D,I,From,italian,[],F)), nl,
                      procedure_message(I).

%%message('tir-486':3010,nessuno,'tir-486':3010,davide,italian,[],cavallo(fdvar_1,fdvar_2,fdvar_3,caspio)) 

% Trigger for variables/3 rules that examines the message 
% and puts fd_var front of the variables that appear in the message
variables(T):-variables(T,_,[]).

% if it is a variable, puts fd_var front of the variables that appear in the message
variables(X,[X|L0],L):-var(X), fdbg_assign_name(X,N), X=N, !, L=L0.

% if it is not a variable, analyzes the functor
variables(T,L0,L):-nonvar(T), functor(T,_,A), variables(0,A,T,L0,L).

variables(A,A,T,L0,L):-retractall(result_format(_)),assert(result_format(T)),!,L=L0.
variables(A0,A,T,L0,L):-A0<A, A1 is A0+1, arg(A1,T,X), variables(X,L0,L1), variables(A1,A,T,L1,L).

% Performs the same operations as the next one, but can be called by the goal 'utente.'.
utente:-open('server.txt',read,Stream,[]), read(Stream,I), close(Stream),
        linda_client(I),
        out(agente_attivo(user,I)),
        procedure_message(I).

% Opens the file with the linda server information, connects to this,
% writes on the backboard that the agent 'user' is active and calls the
% loop procedure for processing the messages.
:-open('server.txt',read,Stream,[]), read(Stream,I), close(Stream),
  linda_client(I),
  out(agente_attivo(user,I)),
  assert(ind(I)),
  procedure_message(I).
