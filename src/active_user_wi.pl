
:-use_module(library(random)),
  use_module(library(lists)),
  use_module(library(system)),
  use_module(library('linda/client')),
  use_module(library(fdbg)).

user.

% apri in lettura il file, Stream è la variabile dove metto lo stream e le opzioni sono niente
utente:-open('../interpreter/server.txt',read,Stream,[]),
              read(Stream,I),
              close(Stream),linda_client(I),
			  out(agente_attivo(user,I)),
			  procedure_message(I).

procedure_message(I):-print('Insert name of addressee'),nl,read(D),
                  
                   print('Insert From'),nl,read(From),
                   print('Insert message'),nl,read(M),
                   go(M),clause(result_format(F),_),
                   out(message(I,D,I,From,italian,[],F)),nl,  
                   assert(ind(I)),
                   print('New message'),nl,procedure_message(I).



go(T):-variables(T,_,[]). %%innesco sul predicato variables. T è il MESSAGGIO %%


%% mette alla variabile X il fdvar davanti ma a che serve?
variables(X,[X|L0],L):-var(X),fdbg_assign_name(X,N),
                       X=N,!,L=L0.

%functor gives arity of functor T

variables(T,L0,L):-nonvar(T),
                   functor(T,_,A),
                   variables(0,A,T,L0,L).

variables(A,A,T,L0,L):-retractall(result_format(_)),assert(result_format(T)),!,L=L0.
variables(A0,A,T,L0,L):-
          A0<A,
          A1 is A0+1,
          arg(A1,T,X),
          variables(X,L0,L1),
          variables(A1,A,T,L1,L).
