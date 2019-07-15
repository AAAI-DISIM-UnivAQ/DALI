% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% AGREE
meta_agree(T,Ag):-once(chiama_agree(T,Ag)).

chiama_agree(T,_):-call(isa(T,_,_));call(evp(T)).

chiama_agree(T,Ag):-meta(T,G,Ag),
                (call(isa(G,_,_));call(evp(G))).

%QUERY_REF
meta_ref(T,N,R,Ag):-once(chiama_ref(T,N,R,Ag)).

chiama_ref(T,N,R,_):-go_rvar(T),
                prendi_value(H),unifica_valori(H,N,R),R\=[].

chiama_ref(T,N,R,Ag):-meta(T,G,Ag),go_rvar(G),
                prendi_value(H),unifica_valori(H,N,R),R\=[].


%SEND_MESSAGE

call_meta_send_message(F,E,Ag,Ind,AgM,IndM,Time):-once(chiama_send_message(F,E,Ag,Ind,AgM,IndM,Time)).

chiama_send_message(F,E,_,_,AgM,_,Time):-go_rvar(E),
                prendi_value(H),present(F,H,Time),pre_event(F,H,Time),
                extern(F,H),extern_args(F,E),ext_event(_,_,AgM,_,_,H,Time).

chiama_send_message(F,E,_,_,AgM,_,Time):-go_rvar(E),
                prendi_value(H),extern(F,H),extern_args(F,E),ext_event(_,_,AgM,_,_,H,Time).

chiama_send_message(F,E,_,_,_,_,Time):-go_rvar(E),
                prendi_value(H),present(F,H,Time),pre_event(F,H,Time).

chiama_send_message(F,E,_,_,AgM,_,Time):-meta(E,G,AgM),go_rvar(G),
                prendi_value(H),extern(F,H),present(F,H,Time),
                pre_event(F,H,Time),extern_args(F,H),
                ext_event(_,_,AgM,_,_,H,Time).                

chiama_send_message(F,E,_,_,AgM,_,Time):-meta(E,G,AgM),go_rvar(G),
                prendi_value(H),present(F,H,Time),pre_event(F,H,Time).

chiama_send_message(F,E,_,_,AgM,_,Time):-meta(E,G,AgM),go_rvar(G),
                prendi_value(H),extern(F,H),extern_args(F,H),
                ext_event(_,_,AgM,_,_,H,Time).


chiama_send_message(_,E,_,_,AgM,_,_):-
                                     write('This event is not in desired event list:'), write(E),write(','), write(learning(E,AgM)),nl.

%CHIAMATE A PROCEDURE

call_meta_execute_proc(E):-once(chiama_execute_proc(E)).

chiama_execute_proc(E):-go_rvar(E),
                prendi_value(H),functor(H,F,N),current_predicate(F/N),call(H).

chiama_execute_proc(E):-meta(E,G,_),go_rvar(G),
                prendi_value(H),functor(H,F,N),current_predicate(F/N),call(H).

chiama_execute_proc(E):-write(learning_proc(E)),nl.


%PROPOSE

call_meta_execute_propose(A,C,Ag):-
             clause(agente(_,_,F,_),_),
             once(chiama_execute_propose(F,A,C,Ag)).

chiama_execute_propose(F,A,C,Ag):-go_rvar(A),prendi_value(H),
                             exists_action(F,H),esegui_propose(H,C,Ag).

chiama_execute_propose(F,A,C,Ag):-meta(A,G,_),go_rvar(G),
                prendi_value(H),exists_action(F,H),esegui_propose(H,C,Ag).



%UNIFICA  FATTI       
unifica_valori(H,N,R):-findall(H,(clause(isa(H,_,_),_);clause(past(H,_,_),_)),L),
                     length(L,Lu),if(Lu<N,R=L,taglia_lista(L,N,R)).
 
taglia_lista(L,N,R):-assert(cont(1)),
                     repeat,
                       clause(cont(X),_),
                       nth1(X,L,El),
                       assert(list_prev(El)),
                       K is X+1,
                       assert(cont(K)),
                       retractall(cont(X)),
                    X==N,!,retractall(cont(_)),
                    findall(W,clause(list_prev(W),_),Lw),
                    R=Lw,retractall(list_prev(_)).


%UNIFICA REGOLE       
unifica(H):-call(H).
 
%TRASFORMA DA VAR A NAMING VAR

go_var(T):-variables(T,_,[]).

variables(X,[X|L0],L):-var(X),fdbg_assign_name(X,N),
                       X=N,!,L=L0.
variables(T,L0,L):-nonvar(T),
                   functor(T,_,A),
                   variables(0,A,T,L0,L).

%% EDITED Aggiunto if(T\=[]).
variables(A,A,T,L0,L):-retractall(result_format(_)),assert(result_format(T)),!,L=L0.
variables(A0,A,T,L0,L):-
          A0<A,
          A1 is A0+1,if(T\=[],
          arg(A1,T,X),false),
          variables(X,L0,L1),
          variables(A1,A,T,L1,L).

%RIPORTA DA fdvar_ ALLA VARIABILE
go_rvar(X):-X=..L,length(L,Lu),
          agg_liste_rvar(L,Lu,1).

agg_liste_rvar(L,Lu,C):-Luf is Lu+1,if(C=Luf,costruisci_term_rvar(L),agg_liste1_rvar(L,Lu,C)).
agg_liste1_rvar(L,Lu,C):-nth1(C,L,E),K is C+1, 
                         if(fdvar(E),sostituisci_fd_rvar(L,E,Lu,K),check_new_term_rvar(L,E,Lu,K)).

%EDITED
sostituisci_fd_rvar(L,E,Lu,K):-sostituisci_name_rvar(E,N),substitute(E,L,N,T),agg_liste_rvar(T,Lu,K).

costruisci_term_rvar(L):-if(clause(lost_rvar(X,E,Y,Z),_),costr_term_rvar(X,E,Y,Z,L),wrt_term_rvar(L)).
costr_term_rvar(X,E,Y,Z,L):-T=..L,substitute(E,X,T,Lf),retractall(lost_rvar(X,E,Y,Z)),
                               Y1 is Y+1, if(Z>Y1,wrt_term_rvar(Lf),cont_wrt_term_rvar(Z,Lf,Y)).
cont_wrt_term_rvar(Z,Lf,Y):-agg_liste_rvar(Lf,Y,Z).

%%Proviamo EDITED
wrt_term_rvar(L):-clause(agente(_,_,S,_),_),name(S,Li),append(A,[46,112,108,101],Li),
                  append(A,[121,121,114,101,101,46,116,120,116],Lf),
                  name(Fi,Lf),
                  examine_if_apex(L,L1),T=..L1,!,open(Fi,append,Stream,[]),
                                             write(Stream,T),write(Stream,'.'),
                                             nl(Stream), close(Stream),
                                             assert(name_file_app(Fi)).
											 
											 
examine_if_apex([],L1):-findall(X,clause(token_apex(X),_),L1),retractall(token_apex(_)).
examine_if_apex([A|B],L1):-is_list(A),examine_if_apex_sublist(A,L),assert(token_apex(L)),examine_if_apex(B,L1).

examine_if_apex([A|B],L1):-not(is_list(A)),name(A,L),nth1(1,L,A1),
                        A1>=65,A1=<90,append([39],L,L11),append(L11,[39],L2),name(T2,L2),
                        assert(token_apex(T2)),examine_if_apex(B,L1).
examine_if_apex([A|B],L1):-assert(token_apex(A)),examine_if_apex(B,L1).


examine_if_apex_sublist([],L1):-findall(X,clause(token_apex_sublist(X),_),L1),retractall(token_apex_sublist(_)).
examine_if_apex_sublist([A|B],L1):-name(A,L),nth1(1,L,A1),
                        A1>=65,A1=<90,append([39],L,L11),append(L11,[39],L2),name(T2,L2),
                        assert(token_apex_sublist(T2)),examine_if_apex_sublist(B,L1).
examine_if_apex_sublist([A|B],L1):-assert(token_apex_sublist(A)),examine_if_apex_sublist(B,L1).
						
						
check_new_term_rvar(L,E,Lu,K):-if(compound(E),check_compound_term_rvar(L,E,Lu,K),agg_liste_rvar(L,Lu,K)).

check_compound_term_rvar(L,E,Lu,K):-asserta(lost_rvar(L,E,Lu,K)),go_rvar(E).           

fdvar(E):-functor(E,_,N),N=0,name(E,L),nth0(0,L,A1),nth0(1,L,A2),nth0(2,L,A3),nth0(3,L,A4),nth0(4,L,A5),nth0(5,L,A6),
            A1=102,A2=100,A3=118,A4=97,A5=114,A6=95.

sostituisci_name_rvar(E,N):-atom_codes(E,Rl),append([102,100,118,97,114,95],N0,Rl),append([95,88],N0,LN),atom_codes(N,LN).

%% Fi è il nome del file "yyree.txt" blablabla 
prendi_value(T):-clause(name_file_app(Fi),_),
       retractall(name_file_app(_)),
       open(Fi,read,Stream,[]),
       read(Stream,T),
       close(Stream),
       if(file_exists(Fi),
       delete_file(Fi),true).


%PRENDE IN INGRESSO IL FILE .pl%        
extern(F,E):-
              leggiriga(F,1),functor(E,Fu,_),
              clause(eventE(Le),_),
              if(member(Fu,Le),true,false).
    
extern_args(F,E):-
              leggiriga(F,9),
              clause(even_args(Le),_),
              if(member(E,Le),true,false).


%VERIFICA LA FATTIBILITA' DI UNA AZIONE
exists_action(F,A):-leggiriga(F,3),
              clause(az(Az),_),
              if(member(A,Az),true,false).

%PRENDE IN INGRESSO IL FILE .pl%        
present(F,E,T):-
              leggiriga(F,5),
              clause(evN(Le),_),
              if(member(E,Le),(retractall(en(E,_)),assert(en(E,T))),false).

              


% PER GLI EVENTI DEL PRESENTE
pre_event(F,E,T):-leggiriga(F,5),
                 clause(evN(Y),_),if(nonvar(Y),canc_en_e(E,T),true).
 
canc_en_e(E,T):-if(clause(en(E,T),false),retract((en(E,T):-false)),true),
              assert(en(E,T)).

ass_se(E,T):-if(clause(ev(E,T),_),true,assert(ev(E,T))).

%PER GLI EVENTI ESTERNI
ext_event(_,_,AgM,_,_,ME,T):-functor(ME,E,_),clause(external_event(E,P),_),
                     if(P=normal,assert(ev_normal(AgM,ME,T)),
                     if(P=high,assert(ev_high(AgM,ME,T)),true)). 


%CHECK PER PROPOSE
esegui_propose(H,C,Ag):-vere_cond(H,C,Ag).
vere_cond(A,Lc,Ag):-Lc=[H|T],functor(H,F,N),current_predicate(F/N),call(H),!,vere_cond(A,T,Ag).
vere_cond(A,Lc,Ag):-Lc=[H|T],evp(H),!,vere_cond(A,T,Ag).
vere_cond(A,[],Ag):-assert((do_propose(A,Ag):-cd(A),asse_cosa(do_action(A,Ag)))),assert(do_action_propose(A,Ag)).


