% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it


%%DEBUG
%%:-leash([exception]).
%%

:- multifile user:term_expansion/6.
:- set_prolog_flag(discontiguous_warnings,off),
   set_prolog_flag(single_var_warnings,off).

%%
:-['communication_onto.pl'].
:-['substitute.pl'].
:-dynamic prefixes/1.
:-dynamic repositories/1.
:-dynamic ontology/3.
%%

:-['tokefun.pl'].

:-['meta1.pl'].
:-['togli_var.pl'].
:-['memory.pl'].




:-['examine_past_constraints.pl'].

:-dynamic tesg/1.
:-dynamic ontology/2.
:-dynamic en/1.


:-dynamic told/6.
:-dynamic export_past/1.
:-dynamic export_past_do/1.
:-dynamic export_past_not_do/1.

:-dynamic deltat/1, deltatime/1, simultaneity_interval/1, wishlist/1, tstart/1, mem_current/1, mem_no_dup/1, mem_past/1, verifica_lista/1.
:-dynamic user_profile_location/1,dali_onto_location/1,server_obj/1, specialization/1,own_language/1, agente/4, agent/1, time_charge/1, da_agg/1.
:-dynamic rule_base/1, mul/1,no_check/0,past/3,tep/2,fatto_mul/2,azi/1,even/1,evin/1,continue_mul_f/1,evN/1.



:-op(500,xfy,:>).
:-op(500,xfy,:<).
:-op(10,xfy,~/).
:-op(1200,xfy,</).
:-op(200,xfy,?/).

:-op(1200,xfx,[:-,:>]).
:-op(1200,xfx,[:-,:<]).
:-op(1200,xfx,[:-,~/]).
:-op(1200,xfx,[:-,</]).
:-op(1200,xfx,[:-,?/]).

:-['leggi_mul.pl'].

user:term_expansion((X,Y),[],[], ([X,Y]),[],[]).
user:term_expansion((X;Y),[],[], ([X,Y]),[],[]).
user:term_expansion((H:>B),[],[],(H:-B),[],[]).
user:term_expansion((X->Y),[],[],([X,Y]),[],[]).
user:term_expansion((H:<B),[],[],(cd(H):-B),[],[]).
user:term_expansion((H~/B),[],[],(export_past(H):-decompose(H,B)),[],[]).
user:term_expansion((H</B),[],[],(export_past_not_do(H):-decompose_not_do(H,B)),[],[]).
user:term_expansion((H?/B),[],[],(export_past_do(H):-decompose_if_it_is(H,B)),[],[]).
user:term_expansion((H:B),[],[],(ct(H,B)),[],[]).
user:term_expansion((H:at(B)),[],[],(ct(H,B)),[],[]).

:-use_module(library(random)),
  use_module(library(lists)),
  use_module(library(system)),
  use_module(library('linda/client')),
  use_module(library(clpq)),
  use_module(library(fdbg)),
  use_module(library(file_systems)).

:-dynamic eve/1.
:-dynamic eve_cond/1.
:-['utils.pl'].

start0(FI):-set_prolog_flag(redefine_warnings,off),
            set_prolog_flag(discontiguous_warnings,off),
            open(FI,read,Stream,[]), read(Stream,Me), close(Stream),
            Me \= end_of_file,
            agent(File, AgentName, Ontolog, Lang, Fil, Lib, UP, DO, Specialization) = Me,
            open('server.txt',read,Stream2,[]),read(Stream2,T),close(Stream2),
            if(UP=no, true, assert(user_profile_location(UP))),
            if(DO=no,true,assert(dali_onto_location(DO))),
            assert(server_obj('localhost':3010)), %% questo si puo togliere se si passa ad una sola funzione
            filtra_fil(Fil),
            assert(specialization(Specialization)),
            if(Ontolog=no,true,load_ontology_file(Ontolog,AgentName)),
            assert(own_language(Lang)),

            linda_client('localhost':3010),
            out(activating_agent(AgentName)),

            delete_agent_files(File),
            token(File),
            start1(File, AgentName, Lib, Fil).

load_ontology_file(Ontolog,Agent):-
        open(Ontolog,read,Stream,[]),
        read(Stream,PrefixesC),
        read(Stream,RepositoryC),
        read(Stream, HostC),
        close(Stream),
        name(Repository,RepositoryC),
        name(Prefixes,PrefixesC),
        name(Host,HostC),
        assert(ontology(Prefixes,[Repository,Host],Agent)).

filtra_fil(FI):-arg(1,FI,File),token_fil(File),retractall(parentesi(_)),togli_var_fil(File).

start1(Fe,AgentName,Libr,Fil):-
  set_prolog_flag(discontiguous_warnings,off),
  if(Libr=no,true,libreria(Fe,Libr,Fil)),

  pl_from_name(Fe, FilePl),
  ple_from_name(Fe, FilePle),
  plv_from_name(Fe, FilePlv),
  plf_from_name(Fe, FilePlf),
  txt_from_name(Fe, FileTxt),

  aprifile(FilePl),

  aprifile_res(FilePl),

  carica_file(FilePl),

  togli_var(Fe),

  togli_var_ple(Fe),

  if(file_exists(FilePlf),controlla_ev_all(FilePle), (inizializza_plf(FilePle), check_messaggio(FilePle, FilePlf))),

  load_directives(FilePlf),
  server_obj(Tee), %% questo si puo togliere se si passa ad una sola funzione
  linda_client(Tee),
  assert(agente(AgentName,Tee,FilePle,FilePl)),
  clause(specialization(Sp),_),
  out(specialized_to(AgentName,Tee,Sp)),
  assert(agent(AgentName)),
  in_noblock(activating_agent(AgentName)),
  out(agente_attivo(AgentName,Tee)),
  assert(time_charge(5)),

  azioni(FilePlv),

  cond_esterni(FilePlv),!,

  asser_evN(FilePle),

  obg_goal(FilePlv),

  aprifile_en(FilePl),

  ass_internal_repeat,

  ass_stringhe_mul(FilePlv),

  compile(FilePlv),

  apri_learn(FileTxt),
  start_learn,
  manage_export_past,
  manage_export_past_not_do,
  manage_export_past_do,
  check_constr_all,
  delete_agent_log_file(AgentName),
  print('..................   Actived Agent '),print(AgentName),print(' ...................'),nl,go.

%L0 it should be communicationf/fipa
libreria(F,L0,Fil):-name(F,Lf),append(Lf,[46,112,108],Ltf),
           append(L0,Fil,L),
           name(F1,Ltf),if(L=[],true,libreria1(F1,L)).

libreria1(F,L):-last(L,U),
                   repeat,
                          member(Me,L),
                                  appendi_regole0(Me),
                   Me==U,!,versa(F).

appendi_regole0(Me):-name(Me,L),append(L,[46,116,120,116],Ltf),
                           name(T,Ltf),if(file_exists(T),appendi_files(T),true),
                           append(L,[46,112,108],Ltf1),name(T1,Ltf1),
                           if(file_exists(T1),appendi_regole(T1),true).

appendi_files(T):-set_prolog_flag(redefine_warnings,off),compile(T).

appendi_regole(Mef):-open(Mef,read,Stream,[]),
                           repeat,
                                         read(Stream,T),if(T=end_of_file,true,
                                         assert(da_agg(T))),
                                T==end_of_file,!,
                close(Stream).

%%Write communication file
versa(F):-findall(X,clause(da_agg(X),_),L),
          last(L,U),
              open(F,append,Stream,[]),nl(Stream),
              repeat,
              member(T,L),
              write(Stream,T),write(Stream,'.'),nl(Stream),
              T==U,!,
              close(Stream),retractall(da_agg(_)).


aprifile(F):-see(F),
             repeat,
                read(T),expand_term(T,Te),
                            if(T=end_of_file,true,
                            assert(rule_base(Te))),
                                T == end_of_file,
             !,
             seen,take,costruisci0(F).

take:-findall(T,clause(rule_base(T),_),L),
          last(L,U),
          repeat,
          member(M,L),
               spezza(M),

           M==U,!,retractall(rule_base(_)), if(clause(mul(_),_),ass_mul_first,true).

examine_mul:-if(clause(mul(_),_),examine1_mul,true).
examine1_mul:-findall(L,clause(mul(L),_),S),
              last(S,E),
              repeat,member(Me,S),keep_past(Me),
             if(clause(no_check,_),retractall(no_check),chiama_cong(Me)),
             Me==E,!.

keep_past(Me):- append([eve],L,Me),examine_head(L).

examine_head(L):-last(L,U),repeat,member(Me,L),check_past(Me),Me==U,!.

check_past(Me):-if(clause(past(Me,_,_),_),true,assert(no_check)).

chiama_cong(Me):-T=..Me,append([eve],L,Me),controlla_time_ep(L,T),take_time_ep(L).

controlla_time_ep(L,T):-last(L,U),repeat,member(M,L),
                                clause(past(M,Tp,_),_),asse_cosa(tep(M,Tp)),
                 M==U,!,cont_tep1(L,T).

cont_tep1(L,T):-findall(X,clause(tep(_,X),_),Le),retractall(tep(_,_)),
                if(clause(fatto_mul(L,Le),_),true,call(T) ).

take_time_ep(L):-last(L,U),repeat,member(M,L),clause(past(M,Tp,_),_),asse_cosa(tep(M,Tp)),
                 M==U,!,cont_tep(L).
cont_tep(L):-findall(X,clause(tep(_,X),_),Le),retractall(tep(_,_)),
         if(clause(fatto_mul(_,_),_),(retractall(fatto_mul(_,_)),assert(fatto_mul(L,Le))),assert(fatto_mul(L,Le))).

spezza(C):-arg(1,C,Head),C=..L,eve_mul_first(Head),if(member(':-',L),is_clausola(L,Head),true).


is_clausola(L,Head):-append([':-'],L1,L),ejec(L1,Head).
ejec([],_).
ejec([S1|Resto],Head):-ejec(S1,Head),!,ejec(Resto,Head).
ejec((X,Y),Head):-expand_term((X,Y),Z),ejec(Z,Head).
ejec((X;Y),Head):-expand_term((X;Y),Z),ejec(Z,Head).
ejec((X->Y),Head):-expand_term((X->Y),Z),ejec(Z,Head).
ejec(X,Head):-if(X=[],true,prova_spezza(X,Head)).
prova_spezza(M,Head):-functor(M,F,_),if(F=a,(arg(1,M,Az),discriminate_learn(Az)),if(F=eve,(arg(1,M,Es),asse_cosa(even(Es))),
          (if(F=evi,(arg(1,M,Iv),asse_cosa(evin(Iv))),
          (if(F=cd,(arg(1,M,Co),asse_cosa(cond(Co))),
          (if(F=en,(arg(1,M,En),asse_cosa(evN(En))),if(F=rem,(arg(1,M,Re),asse_cosa(fact_rem(Re))),caso_if(M,F,Head)))))))))).


caso_if(M,F,Head):-if(F=if,go_to_if(M,Head),if(F=obg,(arg(1,M,G),asse_cosa(obt_goal(G))),
           if(F=tesg,(arg(1,M,Go),asse_cosa(test_goal(Go))),true))).
go_to_if(M,Head):-arg(2,M,A1),arg(3,M,A2),ejec(A1,Head),ejec(A2,Head).

asse_cosa(C):-if(clause(C,_),true,assert(C)).

discriminate_learn(Az):-functor(Az,F,_),
                if(F=message,discriminate_learn1(Az),asse_cosa(azi(Az))).

discriminate_learn1(Az):-arg(2,Az,Pe),functor(Pe,F,_),if(F=confirm,discriminate_learn2(Pe,Az),asse_cosa(azi(Az))).

discriminate_learn2(Pe,Az):-arg(1,Pe,Le),functor(Le,F,_),if(F=learn,discriminate_learn3,asse_cosa(azi(Az))).

discriminate_learn3:-assert(azi(message(_254802,confirm(learn(_254655),_254800)))).

costruisci0(F):-if((((clause(even(_),_);clause(evin(_),_));clause(azi(_),_));clause(evN(_),_)),
                     recupera_fun0(F),true).
recupera_fun0(F):-recupera_funE(F),recupera_funI(F),recupera_funA(F),recupera_tot_cd(F),recupera_funEn(F),recupera_gol_obt(F),recupera_gol_test(F),recupera_tot_rem(F),recupera_E(F).

%ASSERISCE GLI EVENTI ESTERNI RELATIVI AGLI EVENTI MULTIPLI
eve_mul_first(Head):-functor(Head,_,N),Head=..L_eve,if((arg(1,L_eve,_),N>1),continue_mul_f(L_eve),true).
continue_mul_f(L_eve):-arg(1,L_eve,X_eve),if((X_eve=eve,is_list(L_eve)),
                     asse_cosa(mul(L_eve)),true).
ass_mul_first:-findall(L,clause(mul(L),_),S),last(S,E),repeat,member(Me,S),
         keep_past_ass_first(Me),Me==E,!,retractall(mul(_)).

keep_past_ass_first(Me):- append([eve],L,Me),examine_head_ass_first(L).

examine_head_ass_first(L):-last(L,U),repeat,member(Me,L),asse_cosa(even(Me)),
                 Me==U,!.

%RECUPERO FUNTORI EVENTI ESTERNI%
recupera_funE(F):-if(clause(even(_),_),(transf_eventi_esterni, recupera_fun_even(F)),lista_assente(F)).
transf_eventi_esterni:-findall(X,clause(even(X),_),Ls),
                last(Ls,U),
                         repeat,
                                member(Me,Ls),
                                         functor(Me,Fu,_),
                                         assert(app_even(Fu)),
                Me==U,!.



recupera_fun_even(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(app_even(X),_),LA1),
                        remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                                  retractall(app_even(_)).


%RECUPERO FUNTORI EVENTI INTERNI%
recupera_funI(F):-if(clause(evin(_),_),recupera_fun_evin(F),lista_assente(F)).

recupera_fun_evin(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(evin(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(evin(_)).

%RECUPERO AZIONI%
recupera_funA(F):-if(clause(azi(_),_),recupera_fun_azioni(F),lista_assente(F)).

transf_message(Me):-arg(2,Me,Ar),functor(Ar,Far,_),assert(app_azi(message(Far))).
recupera_fun_azioni(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(azi(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(azi(_)).



%RECUPERO CONDIZIONI-AZIONI %
recupera_tot_cd(F):-if(clause(cond(_),_),recupera_cd(F),lista_assente(F)).

recupera_cd(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(cond(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(cond(_)).

%RECUPERO FUNTORI EVENTI PRESENTE%
recupera_funEn(F):-if(clause(evN(_),_),(transf_eventi_presente, recupera_fun_evN(F)),lista_assente(F)).
transf_eventi_presente:-findall(X,clause(evN(X),_),Ls),
                last(Ls,U),
                         repeat,
                                member(Me,Ls),
                                         functor(Me,Fu,_),
                                         assert(app_evN(Fu)),
                Me==U,!.

recupera_fun_evN(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(app_evN(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(app_evN(_)),retractall(evN(_)).


%RECUPERO FUNTORI GOALS DA OTTENERE%
recupera_gol_obt(F):-if(clause(obt_goal(_),_),recupera_fun_obt_goal(F),lista_assente(F)).

recupera_fun_obt_goal(F):- name(F,L),
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(obt_goal(X),_),LA1),
                        remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                        write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                        retractall(obt_goal(_)).

%RECUPERO FUNTORI GOALS DA TESTARE%
recupera_gol_test(F):-if(clause(test_goal(_),_),(transf_tes_goal, recupera_fun_tes_goal(F)),lista_assente(F)).
transf_tes_goal:-findall(X,clause(test_goal(X),_),Ls),last(Ls,U),
                 repeat,
                        member(Me,Ls),
                                 functor(Me,Fu,_),
                                 assert(app_tes_goal(Fu)),
                                Me==U,!.

recupera_fun_tes_goal(F):- name(F,L),
                   append(L,[101],T),
                   name(Y,T),
                                findall(X,clause(app_tes_goal(X),_),LA1),
                                 remove_dups(LA1,LA),
                                open(Y,append,Stream,[]),
                                 write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                          retractall(app_tes_goal(_)).


%RECUPERO FATTI DA RICORDARE E GESTIRE COME EVENTI DEL PASSATO%
recupera_tot_rem(F):-
        if(clause(fact_rem(_),_), recupera_rem(F),lista_assente(F)).

recupera_rem(F):-findall(X,clause(fact_rem(X),_),Ls1),
        remove_dups(Ls1,Ls),
    name(F,L),
        append(L,[101],T),
        name(Y,T),
                 open(Y,append,Stream,[]),
                 write(Stream,Ls),write(Stream,'.'),nl(Stream), close(Stream),
          retractall(fact_rem(_)).


lista_assente(F):- name(F,L),
        append(L,[101],T),
        name(Y,T),open(Y,append,Stream,[]),
                 write(Stream,[]),write(Stream,'.'),nl(Stream), close(Stream).


%RECUPERO EVENTI ESTERNI%
recupera_E(F):-if(clause(even(_),_),recupera_tot_even(F),lista_assente(F)).

recupera_tot_even(F):- name(F,L),
   append(L,[101],T),
   name(Y,T),
                findall(X,clause(even(X),_),LA1),
                 remove_dups(LA1,LA),
                open(Y,append,Stream,[]),
                 write(Stream,LA),write(Stream,'.'),nl(Stream),
                 close(Stream),
  retractall(even(_)).


%INIZIALIZZA IL FILE PLF DELLE DIRETTIVE%

inizializza_plf(F):-if(file_exists(F),leggirighe(F),true).
leggirighe(F):-leggiriga(F,1),leggiriga(F,2),leggiriga(F,3),leggiriga(F,8),
                 clause(eventE(Le),_),clause(evintI(Li),_),clause(az(La),_),clause(rem_fact(Lr),_),
                 append(Le,Li,Lp1),append(Lp1,La,Lp2),append(Lp2,Lr,Lp3),
                 remove_dups(Lp3,Lp4),smonta(Lp4,F),
                 smonta_interno(Li,F).

smonta(E,F):-last(E,Ls),
           name(F,Fe),
           append(L0,[101],Fe),
           append(L0,[102],Nf),
           name(Nof,Nf),
           priority_azi(F,Nof),
                priority_eve(F,Nof),
           repeat,
             member(Me,E),
             open(Nof,append,Stream,[]),
               write(Stream,'past_event('),
               write(Stream,Me),
               write(Stream,',60).'),
               nl(Stream),
               write(Stream,'remember_event_mod('),
               write(Stream,Me),
               write(Stream,',number(5),last).'),
               nl(Stream),
         close(Stream),

         Me==Ls,!.

smonta_interno(E,F):- if(E=[],true,smonta_interno1(E,F)).
smonta_interno1(E,F):- name(F,Fe),
           append(L0,[101],Fe),
           append(L0,[102],Nf),
           name(Nof,Nf),
                     last(E,Ls),
           repeat,
             member(Me,E),
                 open(Nof,append,Stream,[]),
                 write(Stream,'internal_event('),write(Stream,Me),write(Stream,','),
                 write(Stream,'3'),write(Stream,','),write(Stream,forever),
                 write(Stream,','),write(Stream,true),write(Stream,','),
                 write(Stream,'until_cond(past('),
                 write(Stream,Me), write(Stream,'))).'),nl(Stream), close(Stream),

           Me==Ls,!.

priority_azi(Fe,Ff):-leggiriga(Fe,3),
           clause(az(L),_),if(L=[],true,priority_azi1(L,Ff)).

priority_azi1(L,Ff):-last(L,U),
                repeat,
                        member(Me,L),
                                open(Ff,append,Stream,[]),
                                write(Stream,'action('),write(Stream,Me),write(Stream,','),write(Stream,'normal'),
                                write(Stream,').'),nl(Stream),

                                close(Stream),
                        Me==U,!.

priority_eve(Fe,Ff):-leggiriga(Fe,1),
           clause(eventE(L),_),if(L=[],true,priority_eve1(L,Ff)).

priority_eve1(L,Ff):-last(L,U),
                        repeat,
                                member(Me,L),
                                        open(Ff,append,Stream,[]),
                                        write(Stream,'external_event('),write(Stream,Me),write(Stream,','),
                                        write(Stream,'normal'),
                                        write(Stream,').'),nl(Stream),

                                        write(Stream,'mod('),
                                        write(Stream,Me),write(Stream,',check).'),
                                 nl(Stream),close(Stream),
                                                Me==U,!.

controlla_ev_all(F):-
           leggiriga(F,2),clause(evintI(Li),_),
           leggiriga(F,3),clause(az(La),_),
           leggiriga(F,6),clause(obtgoal(Lo),_),
           leggiriga(F,7),clause(testgoal(Lt),_),
           leggiriga(F,8),clause(rem_fact(Lr),_),
           leggiriga(F,9),clause(even_args(Lg),_),
           append(Li,La,L1),append(L1,Lo,L2),append(L2,Lt,L3),
           append(L3,Lr,L4),append(L4,Lg,L6),

                   name(F,Lista),append(Lista1,[101],Lista),
                   append(Lista1,[102],Lista3),name(Plf,Lista3),

                   see(Plf),
                        repeat,
                        read(T),
                           assert(valori_temporanei(T)),
                          T==end_of_file,
                         seen,!,

                          findall(X,clause(valori_temporanei(past_event(X,_)),_),Ls),
                          diff(Ls,L6,Ld1),

                          diff(L6,Ls,Ld2),

                          append(Ld1,Ld2,Ldf),

                          if(Ldf=[], true,
                          (diversi_ev_all(L6),riscrivi(F,Plf))),
                          retractall(valori_temporanei(_)).

diversi_ev_all(L):-assert(k110055(1)),length(L,N),
         repeat,
                 clause(k110055(K),_),
                 nth1(K,L,M),
                 scrivi_l124(M),
                 R is K+1,assert(k110055(R)),retractall(k110055(K)),
      K==N,!,retractall(k110055(_)).


scrivi_l124(M):-if(clause(valori_temporanei(past_event(M,_)),_),true,assert(da_scrivere(M))),!.


riscrivi(F,Plf):-
                findall(E1,clause(da_scrivere(E1),_),Ls1),
                if(Ls1=[],true,scrivi_da_scr(Ls1,Plf,F)).

scrivi_da_scr(Ls1,Plf,F):-assert(k22(1)),length(Ls1,N),
         repeat,
         clause(k22(K),_),
         nth1(K,Ls1,X),
                         clause(da_scrivere(X),_),
                         open(Plf,append,Stream,[]),
                         write(Stream,remember_event_mod(X,number(5),last)),write(Stream,'.'),nl(Stream),
                         write(Stream,past_event(X,60)),write(Stream,'.'),nl(Stream),
                         close(Stream),
                         appart_set_event(X,Plf,F),
                          R is K+1,assert(k22(R)),retractall(k22(K)),
         K==N,!,retractall(k11(_)),retractall(da_scrivere(_)).

appart_set_event(X,Plf,F):-leggiriga(F,1),clause(eventE(Le),_),
        leggiriga(F,2),clause(evintI(Li),_),
           leggiriga(F,3),clause(az(La),_),functor(X,Fun,_),
           if(member(Fun,Le),writ_est_event(Fun,Plf),true),
           if(member(X,Li),writ_int_event(X,Plf),true),
           if(member(X,La),writ_actn_event(X,Plf),true).

writ_est_event(X,Plf):-open(Plf,append,Stream,[]),
                  write(Stream,'external_event('),write(Stream,X),write(Stream,','),
                  write(Stream,'normal'),
                  write(Stream,').'),nl(Stream),
                 write(Stream,'mod('),write(Stream,X),write(Stream,',check).'),
                 nl(Stream),close(Stream).

writ_int_event(X,Plf):- open(Plf,append,Stream,[]),
           write(Stream,'internal_event('),write(Stream,X),write(Stream,','),
           write(Stream,'3'),write(Stream,','),write(Stream,forever),
           write(Stream,','),write(Stream,true),write(Stream,','),write(Stream,'until_cond(past('),
           write(Stream,X), write(Stream,'))).'),nl(Stream), close(Stream).

writ_actn_event(X,Plf):- functor(X,F,_),
         if(F=message,scr_msg_mod(X,Plf),scr_act_mod(X,Plf)).

scr_act_mod(X,Plf):-open(Plf,append,Stream,[]),
         write(Stream,'action('),write(Stream,X),write(Stream,','),
         write(Stream,'normal'),
         write(Stream,').'),nl(Stream),close(Stream).

scr_msg_mod(X,Plf):-open(Plf,append,Stream,[]),
         write(Stream,'action('),write(Stream,X),write(Stream,','),
         write(Stream,'normal'),
         write(Stream,').'),nl(Stream),
         write(Stream,'mod('),write(Stream,X),write(Stream,',check).'),
         nl(Stream),close(Stream).

%DETERMINA SE UNA AZIONE (messaggio) VA SOTTOPOSTA(check) O MENO A CONTROLLO
check_messaggio(Fe,Ff):-leggiriga(Fe,3),
           clause(az(L),_),if(L=[],true,check_mess(L,Ff)).

check_mess(L,Ff):-last(L,U),
          repeat,
                 member(Me,L),
                 functor(Me,Fu,_),
                 if(Fu=message,ass_plf_string(Me,Ff),true),
          Me==U,!.

ass_plf_string(Me,Ff):-open(Ff,append,Stream,[]),
           write(Stream,'mod('),write(Stream,Me),write(Stream,','),
           write(Stream,'check'),write(Stream,').'),nl(Stream),
           close(Stream).

%AGGIORNA LE CONDIZIONI DEGLI EVENTI ESTERNI
writ_cond_extevent(X,Plf):-name(Plf,L),append(Li,[102],L),append(Li,[118],Lj),name(F,Lj),clause(agente(_,_,S,_),_),leggiriga(S,4),
        clause(condt(Y),true),
        retractall(condt(_)),rip_esterni(X,Y,F).




%CARICA LE DIRETTIVE DEL FILE .plf%
load_directives(F):-open(F,read,Stream,[]),
                   repeat,
                                 read(Stream,T),
                                 if(clause(T,_),true,assert((T))),
                        T==end_of_file,!,
        close(Stream).



aprifile_en(F):-see(F),
         repeat,
        read(T),
                        assert(clause_man(T)),
        T == end_of_file,
         !,
         seen,man_go,
                 retractall(clause_man(_)).

man_go:-setof(X,clause_man(X),L),
         last(L,Lu),
                member(A,L),if(A\=end_of_file, arg(1,A,A1),false),
                          spezza1man(A,A1),
                A==Lu,!.

        %T è tutto, mentre A è la TESTA
        spezza1man(T,A):-T=..L,member(X,L),if(member(X,[':-',',']),true,spezza2man(X,A)).

        spezza2man(X,A):-functor(X,_,N),if(N>1,spezza1man(X,A),spezza3man(X,A)).

        % Cioè se cia esattamente un argomento
        spezza3man(X,A):-functor(X,H,N),if(N>0,continman(X,H,A),true).

        continman(X,H,A):-arg(1,X,I),if(H=en,( asse_cosa(legato_en(A,I)), write(A),nl,write(I),nl ),true).

%RICEZIONE EVENTI ESTERNI%
ricmess:-clause(agente(Ag,Ind,_,_),_),
         if(rd_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
         ricmess0(Ag,Ind,AgM,IndM,Language,Ontology,Con),true).


ricmess0(Ag,Ind,AgM,IndM,Language,Ontology,Con):-
          asse_cosa(ext_agent(AgM,IndM,Ontology,Language)),
          in_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
          %print('RECEIVED: '),print(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),nl,
          if(clause(receive(Con),_),chiama_con(AgM,IndM,Language,Ontology,Con),not_receivable_meta(AgM,IndM,Language,Ontology,Con)).

chiama_con(AgM,IndM,Language,Ontology,Con):-if(receive(Con),true,not_receivable_message(AgM,IndM,Language,Ontology,Con)).

%%EDITONTO asserisci_ontologia/2
send_message(E,AgM):-clause(ext_agent(AgM,IndM,Ontology,_),_),
             retractall(ext_agent(AgM,IndM,Ontology,_)),
             clause(agente(Ag,Ind,F,_),_),
             statistics(walltime,[Time,_]),
             asserisci_ontologia(AgM,Ontology),!,
             once(call_meta_send_message(F,E,Ag,Ind,AgM,IndM,Time)).

%%EDITONTO asserisci_ontologia/2
execute_proc(E,AgM):-clause(ext_agent(AgM,IndM,Ontology,_),_),
             retractall(ext_agent(AgM,IndM,Ontology,_)),
             asserisci_ontologia(AgM,Ontology),!,
             once(call_meta_execute_proc(E)).

execute_do_action_propose:-if(clause(do_action_propose(_,_),_),keep_action_propose1,true).

keep_action_propose1:-findall(act_propose(X,Ag),
        clause(do_action_propose(X,Ag),_),L),
        last(L,U),
          repeat,
                   member(Me,L),
                         arg(1,Me,Az),
                         arg(2,Me,Ag),
                         if(do_propose(Az,Ag),true,
                         asse_cosa(not_executable_action_propose(Az,Ag))),
                         retractall(do_action_propose(Az,Ag)),
                         retractall(act_propose(Az,Ag)),
                  Me==U,!.


%%EDITONTO Nuovo sistema gestione ontologie
% ASSORBIMENTO DELLA ONTOLOGIA DELL'AGENTE COMUNICANTE
asserisci_ontologia(AgM,O):-
        if(clause(ontology(_,_,AgM),_), true,
                ( if(O=[],true,(O=ontology(Pref,[Rep,Host],AgM), assert(ontology(Pref,[Rep,Host],AgM))) ) %%Ramo else
                )
        ).

% CONDIZIONE RICEZIONE MESSAGGI

not_receivable_message(AgM,IndM,Language,Ontology,Con):-write('Eliminated message:conditions not verified for  '),write(Con),nl,write('From:'),write(AgM),write(':'),write(IndM),nl,write('Language:'),write(Language),nl,write('Ontology:'),write(Ontology),nl,assert(eliminated_message(AgM,Language,Ontology,Con,motivation(conditions_not_verified))).


not_receivable_meta(AgM,IndM,Language,Ontology,Con):-write('Eliminated message:not exists meta_rule for  '),write(Con),nl,write('From:'),write(AgM),write(':'),write(IndM),nl,assert(eliminated_message(AgM,Language,Ontology,Con,motivation(not_exists_meta_rule))).




%PROCESSA GLI EVENTI ESTERNI
processa_eve:-clause(agente(_,_,S,_),_),leggiriga(S,1),clause(eventE(Es),_),
              if(Es=[],true,(processa_eve_high,processa_eve_normal)).

processa_eve_high:-if(clause(ev_high(_,_,_),_),processa_eve_high1,true).
processa_eve_high1:-findall(ev_high(AgM,E,T),clause(ev_high(AgM,E,T),_),L),
          last(L,U),
          repeat,
                 member(M,L),arg(1,M,Ag),
                 arg(2,M,E),arg(3,M,T),
          if(once(eve_cond(E)),processa_eve_high(Ag,E,T),no_proc_eve_high(Ag,E,T)),
              M==U,!.
no_proc_eve_high(AgM,E,T):-write('External event preconditions not verified'),nl,retractall(ev_high(_,E,T)),asse_cosa(refused_external(AgM,E,T)).
processa_eve_high(AgM,E,T):-retractall(ev_high(AgM,E,T)),asse_cosa(executed_external(AgM,E,T)),
divP(E,T,program),clause(agente(_,_,S,_),_),leggiriga(S,5),
clause(evN(Es),_),if(Es=[],true,if(member(E,Es),processa_eve3(E,T),true)).

check_presence:- clause(eventi_esterni(X),true), if(X>1,clause(deltaT(1),true),true). %controlla che se gli eventi esteni sono >1 deve essere presente il deltat
processa_eve_normal:-if(clause(ev_normal(_,_,_),_),processa_eve_normal1,true).
processa_eve_normal1:-clause(ev_normal(AgM,E,T),_),
                     if(check_presence,true,no_proc_eve_normal_no_time(AgM,E,T)),
                     if(once(eve_cond(E)),processa_eve_normal(AgM,E,T),no_proc_eve_normal(AgM,E,T)).

no_proc_eve_normal(AgM,E,T):-write('External event preconditions not verified'),nl,retractall(ev_normal(_,E,T)),
                          asse_cosa(refused_external(AgM,E,T)).

no_proc_eve_normal_no_time(AgM,E,T):-write('External event preconditions not verified: no DeltaTime'),nl,retractall(ev_normal(_,E,T)), % se gli eventi esterni sono maggiori di 1 e il deltaTime non � presente 
                          asse_cosa(refused_external(AgM,E,T)).


processa_eve_normal(AgM,E,T):-retractall(ev_normal(AgM,E,T)),simultaneity_interval(E), processa_eve.

become_past(E,T):-divP(E,T,program),clause(agente(_,_,S,_),_),leggiriga(S,5),
clause(evN(Es),_),if(Es=[],true,if(member(E,Es),processa_eve3(E,T),true)).


processa_eve3(E,T):-findall(I,clause(legato_en(I,E),_),Ls),
                    member(X,Ls),
                    if(clause(ev_pre_time(X,Tc,Lm),_),processa_eve4(Lm,X,T,Tc),true).
processa_eve4(Lm,X,T,Tc):-if(member(T,Lm),uccidi_iv(X,Tc),true).

uccidi_iv(X,Tc):-if(clause(iv(X,Tc),_),retractall(iv(X,Tc)),true).



%LEGA LE AZIONI ALLE REGOLE AZIONI CORRISPONDENTI%
azioni(Nf):-clause(agente(_,_,S,_),_),retractall(az(_)),
        leggiriga(S,3),
        clause(az(X),true),
        retractall(az(_)),if(X=[],true,azioni1(X,S,Nf)).
azioni1(X,S,Nf):-leggiriga(S,4),
        clause(condt(Y),true),
        retractall(condt(_)),
        last(X,La),
        repeat,
        member(Me,X),
        rip(Me,Y,Nf),
        Me==La.

rip(Me,Y,Nf):-open(Nf,append,Stream,[]),
              if(nonvar(Y),proc_rip(Me,Y,Stream),clause2(Me,Stream)),close(Stream).

proc_rip(Me,Y,Stream):-
                 if(member(Me,Y),clause1(Me,Stream),clause2(Me,Stream)).

        clause1(X,Stream):-nl(Stream),write(Stream,'a('),write(Stream,X),
                    write(Stream,'):-cd('),write(Stream,X),
                    write(Stream,'),assert(do_action('),
                    write(Stream,X),write(Stream,',program'),write(Stream,')).').

        clause2(X,Stream):-nl(Stream),write(Stream,'a('),write(Stream,X),
                    write(Stream,'):-cd('),write(Stream,X),
                    write(Stream,'),assert(do_action('),
                    write(Stream,X),write(Stream,',program'),write(Stream,')).'),nl(Stream),
                    write(Stream,'cd('),write(Stream,X),write(Stream,'):-true.').



%LEGA GLI EVENTI ESTERNI ALLE REGOLE AZIONI CORRISPONDENTI%

cond_esterni(Nf):-clause(agente(_,_,S,_),_),retractall(even_args(_)),
        leggiriga(S,9),
        clause(even_args(X),true),
        retractall(even_args(_)),if(X=[],true,cond_esterni1(X,S,Nf)).
cond_esterni1(X,S,Nf):-leggiriga(S,4),
        clause(condt(Y),true),
        retractall(condt(_)),
        last(X,La),
        repeat,
        member(Me,X),
        rip_esterni(Me,Y,Nf),
        Me==La.

rip_esterni(Me,Y,Nf):-open(Nf,append,Stream,[]),
              if(nonvar(Y),proc_rip_esterni(Me,Y,Stream),clause2_esterni(Me,Stream)),close(Stream).



proc_rip_esterni(Me,Y,Stream):-
                 if(member(Me,Y),clause1_esterni(Me,Stream),clause2_esterni(Me,Stream)).

        clause1_esterni(X,Stream):-nl(Stream),write(Stream,'eve_cond('),write(Stream,X),
                    write(Stream,'):-cd('),write(Stream,X),
                    write(Stream,'),eve('),
                    write(Stream,X),write(Stream,').').

        clause2_esterni(X,Stream):-nl(Stream),write(Stream,'eve_cond('),write(Stream,X),
                    write(Stream,'):-cd('),write(Stream,X),
                    write(Stream,'),eve('),
                    write(Stream,X),write(Stream,').'),nl(Stream),
                    write(Stream,'cd('),write(Stream,X),write(Stream,'):-true.').
     
                    
%EVENTI DEL PRESENTE

asser_evN(S):-leggiriga(S,5),clause(evN(C),_),if(C=[],true,asser_evN1(C)).

asser_evN1(C):- last(C,La),retractall(evN(_)),
                        repeat,
                          member(Me,C),
                          assert((en(Me):-en(Me,_))),
                          assert((en(Me,_):-false)),
                        Me==La,!.





%LEGA I GOALS DA OTTENERE ALLE REGOLE GOALS CORRISPONDENTI%
obg_goal(Nf):-clause(agente(_,_,S,_),_),
        leggiriga(S,6),
        clause(obtgoal(X),true),if(X=[],true,obg_goal1(X,Nf)).

obg_goal1(X,Nf):-retractall(obtgoal(_)),
        open(Nf,append,Stream,[]),
        last(X,La),
        repeat,
        member(Me,X),
        rip_goal(Me,Stream),
        Me==La,!,close(Stream).


rip_goal(Me,Stream):-nl(Stream),write(Stream,'obg('),
              write(Stream,Me),
              write(Stream,'):-asse_goal(attiva_goal('),
              write(Stream,Me),
              write(Stream,')),asse_cosa(tenta_residuo('),
              write(Stream,Me),
              write(Stream,')),clause(past('),
              write(Stream,Me),
              write(Stream,',_,_),_).').



asse_goal(Me):-if(clause(Me,_),true,assert(Me)).

%GOALS DA TESTARE%
tesg(X):-clause(past(X),_);clause(isa(X,_,_),_);clause(past(X,_,_),_).


%% Il do_action è asserivo nel plv quando nelle azioni.
keep_action:-if(clause(do_action(_,_),_),keep_action1,true).

keep_action1:-findall(azione(X,Ag),clause(do_action(X,Ag),_),L),
         last(L,U),
                  repeat,
                           member(Me,L),!,
                           arg(1,Me,Az),
                           arg(2,Me,Ag),

                           retractall(do_action(Az,Ag)),
                           retractall(azione(Az,Ag)),

                           statistics(walltime,[Te,_]),
                           clause(action(Az,Mod),_),
                           if(Mod=high,assert(high_action(Az,Te,Ag)),assert(normal_action(Az,Te,Ag))),

                          Me==U,!.

%SVUOTA LA CODA DELLE AZIONI AD ALTA PRIORITA'%
svuota_coda_priority:-if(clause(high_action(_,_,_),_),svuota_coda_priority1,true).

svuota_coda_priority1:-findall(act(X,T,Ag),clause(high_action(X,T,Ag),_),L),
         last(L,U),
                  repeat,
                           member(Me,L),
                                                 arg(1,Me,Az),
                                                 arg(2,Me,Te),
                                                 arg(3,Me,Agt),
                                                  retractall(high_action(Az,Te,Agt)),
                                                  fai(Az,Te,Agt),
                          Me==U,!.


%PRENDE UNA AZIONE DALLA CODA A  PRIORITA' NORMALE%'
prendi_action_normal:-if(clause(normal_action(_,_,_),_),prendi_action_normal1,true).

prendi_action_normal1:-findall(act_n(X,T,Ag),clause(normal_action(X,T,Ag),_),L),
                         if(L\=[],arg(1,L,Me),false),
                         arg(1,Me,Az),
                         arg(2,Me,Te),
                         arg(3,Me,Agt),
                         retractall(normal_action(Az,Te,Agt)),fai(Az,Te,Agt).




fai(X,T,Ag):-functor(X,F,_),if(F=message,em_mess0(X,T,Ag),choose_action(X)),if(F\=message,(ver_az_int(X,T,Ag),!),true).

choose_action(X):-functor(X,F,_),if(member(F,[drop_past,look_up_past,add_past,set_past,callasp]),
take_past_actions(F,X),(print(make(X)),nl)),save_on_log_file(make(X)).

take_past_actions(F,X):-write('Sono in Take_past_actions'),arg(1,X,E),caso0_past(F,E,X).
caso0_past(F,X,Y):-if(F=drop_past,drop_evento(X),caso1_past(F,X,Y)).
caso1_past(F,X,Y):-if(F=add_past,add_evento(X),caso2_past(F,X,Y)).
caso2_past(F,X,Y):-if(F=look_up_past,look_up_evento(X),caso3_past(F,X,Y)).

%ASP INTERCONNECTION
caso3_past(F,X,Y):-if(F=callasp,call(Y),set_past_evento(X,Y)).


%PRIMITIVE DI GESTIONE DEGLI EVENTI DEL PASSATO
drop_evento(X):-if(clause(past(X,_,_),_),retractall(past(X,_,_)),true).
add_evento(X):-statistics(walltime,[Tc,_]),assert(past(X,Tc,program)).
look_up_evento(X):-clause(past(X,_,_),_).
set_past_evento(X,C):-retractall(past_event_life(X,_)),retractall(past_event_mod(X,_,_)),retractall(past_event_mod(X,_,_,_)),assert(C).


em_mess0(X,T,Ag):-if(clause(mod(X,check),_),check_msg(X,T,Ag),em_mess(X)).
check_msg(X,T,Ag):-arg(1,X,To),arg(2,X,M),

              if(call(send(To,M)),(ver_az_int(X,T,Ag),!),not_sendable_message(To,M)).
em_mess(X):-arg(1,X,To),arg(2,X,M),send_m0(To,M).

not_sendable_message(To,M):-write('Eliminated message:conditions not verified for  '),write(M),nl,write('To:'),write(To),nl,
assert(eliminated_message(M,To,motivation(conditions_not_verified))).

send_m0(To,M):-go_var(M),clause(result_format(R),_),send_m(To,R).

send_m(To,M):-clause(agente(S,IndS,_,_),_),
            if(To=all,manda_a_all(M,S,IndS),if(To=room,manda_a_room(M,S,IndS),manda_a(To,M,S,IndS))).

manda_a(To,M,S,IndS):-
                        %print('testing agent active...'),print(rd_noblock(agente_attivo(To,IndTo))),nl,
                        if(rd_noblock(agente_attivo(To,IndTo)),
                                emetti_ms(To,M,S,IndS,IndTo),
                                print('Message addressed to a not active agent')),nl.

%% Azione di invio del messaggio
emetti_ms(To,M,S,IndS,IndTo):-clause(own_language(Lang),_),
          invia_terms_ontology(O),
          out(message(IndTo,To,IndS,S,Lang,O,M)),
          %print(send_message_to(To,M,Lang,O)),nl,
          save_on_log_file(send_message(M)).

invia_terms_ontology(O):-
        clause(agent(Agent),_),
        if(clause(ontology(Prefixes,[Rep,Host],Agent),_), O=ontology(Prefixes, [Rep,Host], Agent), O=[]).

%%EDITONTO
manda_a_all(M,S,IndS):-bagof_rd_noblock(ag(A,Ind),
           agente_attivo(A,Ind),Lis),
           nth1(N,Lis,ag(S,IndS)),
           nth1(N,Lis,ag(S,IndS),R),
           invia_terms_ontology(L),
           clause(own_language(Lang),_),
           remove_dups(R,Rwd),
           last(Rwd,RL),
                 repeat,
                   member(X,Rwd),
                          arg(1,X,To),
                          arg(2,X,IndTo),
                          out(message(IndTo,To,IndS,S,Lang,L,M)),
                          print(send_message_to(To,M,Lang,L)),nl,nl,
           X==RL,!.

%%EDITONTO
manda_a_room(M,S,IndS):-arg(2,M,Cat),functor(M,F,_),
           datime(C),
           invia_terms_ontology(L),
           clause(own_language(Lang),_),
                          out(room(F,Cat,M,IndS,S,Lang,L,C)),
                          print(send_desire(F,Cat,M,IndS,S,Lang,L,C)),nl.


ver_az_int(X,Tc,Ag):-
           divP(X,Tc,Ag),
   clause(agente(_,_,S,_),_),
   leggiriga(S,2),
           clause(evintI(L),_),
           if(L\=[],ver_az_int1(L,Tc,X),true).

ver_az_int1(L,Tc,X):-retractall(evintI(_)),
if(member(X,L),assert(iv(X,Tc)),true).

%EVENTI INTERNI%'
ev_int:-clause(agente(_,_,S,_),_),
        leggiriga(S,2),clause(evintI(L),_),
        if(L\=[],con_ev_int1(L,S),true).

con_ev_int1(L,S):-leggiriga(S,3),
  clause(az(As),_),
  retractall(evintI(_)),
  retractall(az(_)),
  if(As=[],freq_tutto1(L),freq_sing1(L,As)).


freq_tutto1(L):-last(L,Ul),
        repeat,
        member(Me,L),
        asser_prov0(Me),
    Me==Ul,!.

freq_sing1(L,As):-last(L,Ul),
        repeat,
        member(Me,L),
        if(member(Me,As),true,asser_prov0(Me)),
    Me==Ul,!.

asser_prov0(Me):-if(clause(prov_int0(Me),_),true,assert(prov_int0(Me))).

ev_goal:-findall(M,clause(prov_int0(M),_),L),
         if(L=[],true,ev_goal1(L)).
ev_goal1(L):-clause(agente(_,_,S,_),_),
        leggiriga(S,6),
        clause(obtgoal(X),true),if(X=[],passa_a_prov_int(L),ev_goal21(X,L)).

ev_goal21(X,L):-last(X,U),
              repeat,
                member(Me,X),
                  functor(Me,F,_),
                  asse_cosa(see_goal(F)),
              Me==U,!,ev_goal2(L).

ev_goal2(L):-findall(V,clause(see_goal(V),_),Lv),
              last(L,U),
              repeat,
                member(Me,L),
                functor(Me,F,_),
                  if(member(F,Lv),ex_ev_goal(Me),asser_prov(Me)),
              Me==U,!,retractall(see_goal(_)).

ex_ev_goal(Me):-if(clause(attiva_goal(Me),_),asser_prov(Me),scarica_prov(Me)).

passa_a_prov_int(L):-last(L,U),
              repeat,
                member(Me,L),
                  asser_prov(Me),retractall(prov_int0(Me)),
              Me==U,!.


asser_prov(Me):-if(clause(prov_int(Me),_),true,assert(prov_int(Me))).
scarica_prov(Me):-if(clause(prov_int(Me),_),retractall(prov_int(Me)),true).

ev_int0:-if(clause(prov_int(_),_),ev_int01,true).
ev_int01:-findall(M,prov_int(M),L),

         last(L,U),
         repeat,
           member(Me,L),
             clause(internal_event(Me,_,_,_,C),_),
             functor(C,F,_),
             if(F=forever,carica(Me),ev_int011(Me,F,C)),
        Me==U,!.

ev_int011(Me,C0,C):-if(C0=until_cond,esamina_cd(Me,C),ev_int012(Me,C0,C)).
ev_int012(Me,C0,C):-if(C0=in_date,esamina_dt(Me,C),print('Writing_error1')).
esamina_cd(Me,C):-arg(1,C,C1),if(clause(C1,true),scarica(Me),carica(Me)).
esamina_dt(Me,C):-arg(1,C,D),arg(2,C,A),verif_time(Me,D,A).

 verif_time(Iv,D,A):-datime(C),arg(1,C,Ac),arg(1,D,A1),arg(1,A,A2),
              if((Ac>A1,Ac<A2),carica(Iv),if((Ac=A1,Ac=\=A2),
              conf_date_max(Iv,D,A,C),
              if((Ac=A2,A1=\=Ac),conf_date_min(Iv,D,A,C),
              if((Ac=A1,Ac=A2),
              conf_date_3(Iv,D,A,C),scarica(Iv))))).

        conf_date_max(Iv,D,A,C):-arg(2,D,Me1),arg(3,D,G1),arg(2,C,Me),arg(3,C,G),
                       R1 is (Me1*43790+G1*1440),
                       R is (Me*43790+G*1440),

                       if(R>R1,controlla_ore(Iv,D,A,C),scarica(Iv)).

        conf_date_min(Iv,D,A,C):-arg(2,A,Me1),arg(3,A,G1),arg(2,C,Me),arg(3,C,G),
                       R2 is (Me1*43790+G1*1440),
                       R is (Me*43790+G*1440),

                       if(R=<R2,controlla_ore(Iv,D,A,C),scarica(Iv)).

        conf_date_3(Iv,D,A,C):-
                arg(2,D,Me1),arg(3,D,G1),arg(2,C,Me),arg(3,C,G),
                                   arg(2,A,Me2),arg(3,A,G2),
                                   R1 is (Me1*43790+G1*1440),
                                   R2 is (Me2*43790+G2*1440),
                                   R is (Me*43790+G*1440),

                                   if((R>=R1,R=<R2),controlla_ore(Iv,D,A,C),scarica(Iv)).

        controlla_ore(Iv,D,A,C):-arg(4,D,H1),arg(5,D,M1),arg(6,D,S1),arg(4,A,H2),arg(5,A,M2),arg(6,A,S2),arg(4,C,H),arg(5,C,M),arg(6,C,S),
                        O1 is (H1*3600+M1*60+S1),
                        O2 is (H2*3600+M2*60+S2),
                        O is (H*3600+M*60+S),
                        if(O2>O1,ora_normale(Iv,O,O1,O2),print('To insert a correct temporal interval')).

ora_normale(Iv,O,O1,O2):-if((O>O1,O<O2),carica(Iv),scarica(Iv)).



carica(Me):-if(clause(prov_int1(Me),_),true,assert(prov_int1(Me))).

scarica(Me):-
if(clause(prov_int1(Me),_),retractall(prov_int1(Me)),true).


blocco_numero_ev_int:-if(clause(prov_int1(_),_),blocco_numero1,true).
blocco_numero1:-findall(M,clause(prov_int1(M),_),L),
                if(L=[],true,blocco_numero2(L)).

blocco_numero2(L):-last(L,U),
        repeat,
        member(Me,L),
        clause(internal_event(Me,_,N,_,_),_),
        if(N=forever,assert(prov_int2(Me)),check_time_rep(Me,N)),
        Me==U,!,retractall(prov_int1(_)).

check_time_rep(Me,N):-functor(Me,F,_),
                      if(cond_int_repeat(F,N),check_cond_repeat(Me,N),
                      asserisco_prov_int2(Me)).

cond_int_repeat(F,N):-clause(internal_repeat(F,P),_),P>=N.
asserisco_prov_int2(Me):-asse_cosa(prov_int2(Me)).

check_cond_repeat(Me,N):-clause(internal_event(Me,_,N,Cn,_),_),
                         if(Cn=true,true,check_cond_repeat1(Me,Cn)).

check_cond_repeat1(Me,Cn):-functor(Cn,F,_),
                             if(F=change,check_cond_change(Me,Cn),true).

%%Edited: Vedi gli arg(1 cosa ho fatto .
check_cond_change(Me,Cn):-if(Cn\=[],arg(1,Cn,L),false),if(L=[],true,check_cond_change1(Me,L)).
check_cond_change1(Me,L):-last(L,U),
          repeat,
                 member(M,L),
                 if(change_fact_or_evp(M,Me),
                 fact_evp_changed(Me),
                true),
           M==U,!.

fact_evp_changed(Me):-functor(Me,F,_),retractall(internal_repeat(F,_)),
          assert(internal_repeat(F,0)).

change_fact_or_evp(M,Me):-functor(Me,F,_),clause(internal_reaction(F,Ti),_),
          if(clause(isa(M,_,_),_),
          change_fact(M,Ti),(if(clause(past(M,_,_),_),
          change_past(M,Ti),false))).


change_fact(M,Ti):-clause(isa(M,_,Tf),_),
          if(Tf>Ti,true,false).

change_past(M,Ti):-clause(past(M,Tp,_),_),
          if(Tp>Ti,true,false).




blocco_frequenza:-if(clause(prov_int2(_),_),blocco_frequenza1,true).
blocco_frequenza1:-findall(M,clause(prov_int2(M),_),L),
        if(L=[],true,blocco_frequenza2(L)).

blocco_frequenza2(L):-last(L,U),
        repeat,
        member(Me,L),
        if(clause(reagito(Me,_),_),true,asserisci_tempI(Me)),
         Me==U,!,retractall(prov_int2(_)).


asserisci_tempI(Me):-
          if(clause(tempI(Me),_),true,assert(tempI(Me))).

ev_int2:-if(clause(tempI(_),_),ev_intp,true).

ev_intp:-findall(Y,tempI(Y),Ls),
         statistics(walltime,[Tc,_]),
         last(Ls,La),
         repeat,
           member(Me,Ls),
       if(clause(tentato_evento(Me,_),_),true,ev_int_p_no_tent(Me,Tc)),
         Me==La,!.

ev_int_p_no_tent(Me,Tc):-if(once(Me),asserisci_iv(Me,Tc),retractall(tempI(Me))).

asserisci_iv(Me,Tc):-assert(tentato_evento(Me,Tc)),
         if(clause(iv(Me,Tc),_),true,assert(iv(Me,Tc))),
         retractall(tempI(Me)),
         if(clause(attiva_goal(Me),_),
         canc_goal(Me),true),
         findall(X,clause(legato_en(Me,X),_),L),
         if(L=[],true,(interno_pres(Me,L),ass_ev_pre_i(Me,Tc))).

interno_pres(Me,L):-last(L,Lu),
   member(X,L),
          clause(en(X,T1),true),
           assert(lega_int_time(Me,T1)),
          retractall(en(X,T1)),
   X==Lu,!.

ass_ev_pre_i(Me,Tc):-setof(T,clause(lega_int_time(Me,T),_),L1),
   retractall(lega_int_time(Me,_)),
   if(clause(ev_pre_time(Me,_,_),_),
         (retractall(ev_pre_time(Me,_,_)),assert(ev_pre_time(Me,Tc,L1))),
        assert(ev_pre_time(Me,Tc,L1))).

%GESTISCE CODA GOALS DA OTTENERE%
obtaining_goals:-findall(a(G,T),clause(ob_goal(G,T),_),Lg),if(Lg=[],true,process_ob_goal(Lg)).
process_ob_goal(Lg):-nth0(0,Lg,Gl),arg(1,Gl,El),arg(2,Gl,T),if(once(El),fai_div_evp(El,T),true).
fai_div_evp(El,T):-divP(El,T,program),retractall(ob_goal(El,T)).

%TRATTA I GOALS DA OTTENERE COME EVENTI INTERNI



scatena:-findall(k(Me,Tc),clause(iv(Me,Tc),_),L),
manage_list_int(L).

manage_list_int([]):-true.

%%EDITED!! Aggiunto un trace
manage_list_int([A|B]):-arg(1,A,Me),arg(2,A,Tc),scat0(Me,Tc),manage_list_int(B).

scat0(Me,Tc):-if(clause(legato_en(Me,_),_),scat0_en(Me,Tc),scat1(Me,Tc)).

scat0_en(Me,Tc):-findall(E,clause(legato_en(Me,E),_),L),
         last(L,U),
                  repeat,
                                member(X,L),
                                if(clause(past(X,T1,_),_),check_app(Me,Tc,T1),scat1(Me,Tc)),
          X==U,!.

check_app(Me,Tc,T1):-if(clause(ev_pre_time(Me,Tc,L1),_),check_member(Me,T1,Tc,L1),true).

check_member(Me,T1,Tc,L1):-if(member(T1,L1),ritrai_ev_pres(Me,Tc),scat1(Me,Tc)).

ritrai_ev_pres(Me,Tc):-retractall(iv(Me,Tc)).

scat1(Me,Tc):-if(evi(Me),scatena_agisci(Me,Tc),no_scatena(Me,Tc)),!.
no_scatena(Me,Tc):-retractall(iv(Me,Tc)),if(clause(reagito(Me,_),_),true,assert(reagito(Me,Tc))),

                   retractall(tempI(Me)), rec_internal_reaction(Me,Tc),
                           rec_internal_repeat(Me).

scatena_agisci(Me,Tc):-retractall(iv(Me,Tc)),divP(Me,Tc,program),
                   assert(reagito(Me,Tc)),
                   rec_internal_reaction(Me,Tc),
                   rec_internal_repeat(Me).

rec_internal_reaction(Me,Tc):-functor(Me,F,_),if(clause(internal_reaction(F,_),_),
                   agg_internal_reaction(Me,Tc),crea_internal_reaction(Me,Tc)).

agg_internal_reaction(Me,Tc):-functor(Me,F,_),retractall(internal_reaction(F,_)),assert(internal_reaction(F,Tc)).
crea_internal_reaction(Me,Tc):-functor(Me,F,_),assert(internal_reaction(F,Tc)).

rec_internal_repeat(Me):- functor(Me,F,_),clause(internal_repeat(F,N),_),
                  retractall(internal_repeat(F,N)),R is N+1,
                  assert(internal_repeat(F,R)).


controlla_freq_tent:-clause(agente(_,_,S,_),_),statistics(walltime,[T,_]),
                  leggiriga(S,2),clause(evintI(L),_),
        if(L\=[],contr_freq_tent0(L,T),true).


contr_freq_tent0(Ls,T):-last(Ls,U),
           repeat,
           member(X,Ls),
           if(clause(internal_event(X,Tp,_,_,_),_),
           controlla_freq_tent1(X,T,Tp),true),
                X==U,!.

controlla_freq_tent1(X,T,Tp):-if(clause(tentato_evento(X,Tc),_),controlla_freq_tent2(X,T,Tp,Tc),true).

controlla_freq_tent2(X,T,Tp,Tc):-Tp1 is Tp*1000,
                       ST is Tp1+Tc,
                       if(T>ST,canc_tentato(X,Tc),true).
canc_tentato(X,Tc):-retractall(tentato_evento(X,Tc)).

controlla_freq_iv:-clause(agente(_,_,S,_),_),statistics(walltime,[T,_]),
                  leggiriga(S,2),clause(evintI(L),_),
        if(L\=[],contr_freq_iv0(L,T),true).


contr_freq_iv0(Ls,T):-last(Ls,U),
           repeat,
           member(X,Ls),
           if(clause(internal_event(X,Tp,_,_,_),_),
           controlla_freq_iv1(X,T,Tp),true),
                X==U,!.





controlla_freq_iv1(X,T,Tp):-if(clause(reagito(X,Tc),_),controlla_freq_int2(X,T,Tp,Tc),true).

controlla_freq_int2(X,T,Tp,Tc):-Tp1 is Tp*1000,
                       ST is Tp1+Tc,
                       if(T>ST,canc_e_reagito(X,Tc),true).
canc_e_reagito(X,Tc):-retractall(reagito(X,Tc)).





divP(Me,Tc,Ag):-if(clause(past(Me,Tr,A),_),sposta_in_rem(Me,Tr,A,Tc,Ag),asserta(past(Me,Tc,Ag))).

sposta_in_rem(Me,Tr,A,Tc,Ag):-retractall(past(Me,Tr,A)),retractall(past(Me)),
                  assert(remember(Me,Tr,A)),asserta(past(Me,Tc,Ag)).

canc_goal(Me):-retractall(attiva_goal(Me)).

%CONTROLLA LA VITA DEI PAST EVENTS
controlla_vita_past_base:-findall(it_past(I,T,Pr),clause(past(I,T,Pr),_),Is),
        if(Is=[],true,controlla_vita_past(Is)).
controlla_vita_past(Is):-last(Is,IU),
        repeat,
          member(Me,Is),arg(1,Me,Mee),
                functor(Mee,F,_),
                 if(clause(past_event(Mee,T),_),contrr_past_event1(Me,T),
                  if(clause(past_event(F,T),_),contrr_past_event1(Me,T),true)),
          Me==IU,!.

contrr_past_event1(Me,T):-if(number(T),processa_tempo_past(Me,T),controlla_vita_past1(Me,T)).
controlla_vita_past1(Me,T):-functor(T,F,_),if(F=forever,true,controlla_vita_past2(Me,F,T)).
controlla_vita_past2(Me,F,T):-if(F=until,processa_cd_past(Me,T),print('Writing_error2')).

processa_tempo_past(Me,T):-arg(1,Me,P),arg(2,Me,Tc),arg(3,Me,Pr),T1 is T*1000, Tt is Tc+T1,
          statistics(walltime,[Tcorr,_]),

          if(Tt<Tcorr,processa_tempo_canc_item(P,Tc,Pr),true).

processa_tempo_canc_item(Me,Tc,Pr):-retractall(past(Me)),
                        retractall(past(Me,Tc,Pr)),
                        assert(remember(Me,Tc,Pr)).


processa_cd_past(Me,T):-arg(1,Me,P),arg(1,T,C),
                        last(C,U),
                          repeat,
                            member(K,C),
                             if(clause(K,true),(retractall(past(P)),retractall(past(P,_,_))),
                              true),
                          K==U,!.


%CONTROLLA LA VITA DEI REMEMBER EVENTS
controlla_vita_remember:-controlla_vita_remember_base,controlla_past_remember.

controlla_vita_remember_base:-findall(it(I,T),clause(remember(I,T,_),_),Is),
                                if(Is=[],true,controlla_vita_remember0(Is)).



controlla_vita_remember0(Is):-last(Is,IU),
                repeat,
                  member(Me,Is),arg(1,Me,Mee),
                        functor(Mee,F,_),
                         if(clause(remember_event(Me,T),_),contrr_remember_event1(Me,T),
                          if(clause(remember_event(F,T),_),contrr_remember_event1(Me,T),true)),
                  Me==IU,!.

contrr_remember_event1(Me,T):-if(number(T),processa_tempo_rem(Me,T),controlla_vita_remember1(Me,T)).
controlla_vita_remember1(Me,T):-functor(T,F,_),if(F=forever,true,controlla_vita_remember2(Me,F,T)).
controlla_vita_remember2(Me,F,T):-if(F=until,processa_cd_remember(Me,T),print('Writing_error2')).

processa_tempo_rem(Me,T):-arg(1,Me,P),arg(2,Me,Tc),T1 is T*1000, Tt is Tc+T1,
          statistics(walltime,[Tcorr,_]),

          if(Tt<Tcorr,processa_tempo_canc_item_rem(P,Tc),true).

processa_tempo_canc_item_rem(Me,Tc):-retractall(remember(Me)),retractall(remember(Me,Tc,_)).


processa_cd_remember(Me,T):-arg(1,Me,P),arg(1,T,C),
                last(C,U),
                  repeat,
                        member(K,C),
                         if(clause(K,true),(retractall(remember(P)),retractall(remember(P,_,_))),
                          true),
                  K==U,!.

controlla_past_remember:-findall(I,clause(remember_event_mod(I,_,_),_),Is0),
                remove_dups(Is0,Is),
                if(Is=[],true,controlla_past_mod_rem0(Is)).




controlla_past_mod_rem0(Is):-last(Is,IU),
                repeat,
                  member(Me,Is),
                         if(clause(remember_event_mod(Me,number(N),last),_),contrr_remember_event_event_mod1(Me,N),
                          if(clause(remember_event_mod(Me,number(N),first),_),contrr_remember_event_event_mod2(Me,N),
                                if(clause(remember_event_mod(Me,number(N),T1,T2),_),contrr_remember_event_event_mod3(Me,T1,T2),
                                 true))),
                  Me==IU,!.

contrr_remember_event_event_mod1(P,N):-findall(past_app(P,T,S),clause(remember(P,T,S),_),L0),
                 length(L0,Lu),
                  if(Lu>N,eliminate_not_last_items_remember(L0,N),true).


eliminate_not_last_items_remember(L,N):-
   length(L,Lu),
          Ul is Lu-N,
          assert(cont_last_it1(Ul)),
          repeat,
          clause(cont_last_it1(K),_),
          nth1(K,L,E),arg(1,E,P),arg(2,E,T),arg(3,E,Ty),retractall(remember(P,T,Ty)),
         Y is K-1,
          retractall(cont_last_it1(_)),
          assert(cont_last_it1(Y)),
          Y==0,!,retractall(cont_last_it1(_)).


contrr_remember_event_mod2(P,N):-findall(past_app(P,T,S),clause(past(P,T,S),_),L0),
         length(L0,Lu),
          if(Lu>N,eliminate_not_first_items_remember(L0,N),true).


eliminate_not_first_items_remember(L,N):-
   length(L,Lu),
          Ul is N+1,
          assert(cont_last_it(Ul)),
          repeat,
          clause(cont_last_it(K),_),
          nth1(K,L,E),arg(1,E,P),arg(2,E,T),arg(3,E,Ty),retractall(remember(P,T,Ty)),
          Y is K+1,
          retractall(cont_last_it(_)),
          assert(cont_last_it(Y)),
          S is Lu+1,
          Y==S,!,retractall(cont_last_it(_)).

contrr_remember_event_mod3(P,T1,T2):-findall(past_app(P,T,S),clause(remember(P,T,S),_),L0),
        last(L0,U),
           member(M,L0),
                 arg(1,M,Pm),arg(2,M,Tm),arg(3,M,Ty),
                        if(cond_true_remember_event_mod3(T,T1,T2),true,elim_remember_event_mod3(Pm,Tm,Ty)),
           M==U,!.

elim_remember_event_mod3(Pm,Tm,Ty):-retractall(remember(Pm,Tm,Ty)).


cond_true_remember_event_mod3(T,T1,T2):-T>=T1,T=<T2.

pari:-sleep(1),random(1,10,R), R1 is R mod 2,
      if(R1=0,(internal,external),(external,internal)).

internal:-blocco_constr,ricmess,ev_int, ev_goal,ev_int0,blocco_numero_ev_int,blocco_frequenza,
ev_int2,controlla_freq_tent,svuota_coda_priority,controlla_freq_iv,scatena,blocco_constr,keep_action,execute_do_action_propose,prendi_action_normal,blocco_constr,controlla_vita.

external:-blocco_constr,ricmess,processa_eve,examine_mul,keep_action,svuota_coda_priority,prendi_action_normal,blocco_constr,controlla_vita.

side_goal:-obtaining_goals,residue_goal.


go:-repeat,(pari,side_goal),fail,!.


blocco_constr:-evaluate_evp_constr,evaluate_evp_do,evaluate_evp_not_do.

controlla_vita:-controlla_vita_past_base, controlla_vita_remember.



en(X):-en(X,_).
not(X):-if(X,false,true).

% LIST PROCEDURE determina gli elementi di differenza tra due liste

diff([],_,[]).
diff([H|Tail],L2,[H|Tail1]):-
not(member(H,L2)),!,
diff(Tail,L2,Tail1).
diff([_|Tail],L2,Tail1):-
diff(Tail,L2,Tail1).


% PRENDE LA LISTA DEI RESIDUI DEI GOALS E LA SODDISFA
residue_goal:-if(clause(tenta_residuo(_),_),residue_goal1,true).
residue_goal1:-findall(X,clause(tenta_residuo(X),_),L),
          last(L,U),
                        repeat,
                           member(Me,L),
                           if(call_residue_goal(Me),resid_goal(Me),true),
          Me==U,!.
resid_goal(Me):-retractall(tenta_residuo(Me)),if(clause(attiva_goal(Me),_),
                   canc_goal(Me),true).

% PREFISSO PAST DEi PAST%
evp(E):-clause(past(E),_).
evp(E):-clause(past(E,_,_),_).
rem(E):-clause(remember(E,_,_),_).


% PREFISSO PAST DEi PAST%
isa(E):-clause(isa(E,_,_),_).

%INIZIALIZZA INTERNAL REPEAT%

ass_internal_repeat:-clause(agente(_,_,S,_),_),
         leggiriga(S,2),
         clause(evintI(L),_),
         if(L\=[],ass_internal_repeat1(L),true).

ass_internal_repeat1(L):-last(L,U),
         repeat,
                member(M,L),functor(M,F,_),
                 assert(internal_repeat(F,0)),
         M==U,!.

%SCRIVE NEL FILE PLV GLI EVENTI ESTERNI SINGOLI DI UN EVENTO ESTERNO MULTIPLO
ass_stringhe_mul(Nf):-findall(X,clause(da_asserire_stringhe_mul(X),_),L),
          if(L=[],true,ass_stringhe_mul1(Nf,L)).
ass_stringhe_mul1(Nf,L):-open(Nf,append,Stream,[]),
                 last(L,U),
                  repeat,
                         member(Me,L),
                         nl(Stream),
                         write(Stream,'eve('),
                         write(Stream,Me),write(Stream,'):-true.'),
                   Me==U,!,
                  close(Stream).

%APRE IL FILE PLV ED ASSERISCE LE REGOLE PER GLI EVENTI SINGOLI DEGLI EVENTI ESTERNI MULTIPLI
%% e' chiamato dalla "take_meta_var" contenuta in "togli_var.pl"
%% Il file PLV non è vuoto. Contiene cioè che è scritto nel file PL.
aprifile_head_mul(F):-see(F),
         repeat,
        read(T),expand_term(T,Te),
                                                if(T=end_of_file,true,
                                                assert(rule_mul_head(Te))),
                                        T == end_of_file,
         !,
         seen,take_head_mul.


take_head_mul:-findall(T,clause(rule_mul_head(T),_),L),
          last(L,U),
         repeat,
          member(M,L),

                   arg(1,M,Head),
                   eve_mul(Head),
         M==U,!,retractall(rule_mul_head(_)), if(clause(mul(_),_),ass_mul_head,true).

eve_mul(Head):-functor(Head,_,N),Head=..L_eve,if((L_eve\=[],arg(1,L_eve,_),N>1),continue_mul(L_eve),true).
        continue_mul(L_eve):-arg(1,L_eve,X_eve),if((X_eve=eve,is_list(L_eve)),
                     asse_cosa(mul(L_eve)),true).
ass_mul_head:-findall(L,clause(mul(L),_),S),last(S,E),repeat,member(Me,S),
         keep_past_ass_head(Me),Me==E,!.

keep_past_ass_head(Me):- append([eve],L,Me),examine_head_ass(L).

examine_head_ass(L):-last(L,U),repeat,member(Me,L),assert(da_asserire_stringhe_mul(Me)),
                     Me==U,!.

%FUNZIONI AUSILIARIE PER LA COMUNICAZIONE
exist_event(X):-clause(agente(_,_,F,_),_),extern(F,X).




%LEGGE LE LISTE DEGLI EVENTI SU FILE .PLE%
leggiriga(F,N):-see(F),read(T),if(N=1,chiudi1(T),leggiriga2(N)).

        leggiriga2(N):-read(T),if(N=2,chiudi2(T),leggiriga3(N)).

        leggiriga3(N):-read(T),if(N=3,chiudi3(T),leggiriga4(N)).

        leggiriga4(N):-read(T),if(N=4,chiudi4(T),leggiriga5(N)).

        leggiriga5(N):-read(T),if(N=5,chiudi5(T),leggiriga6(N)).

        leggiriga6(N):-read(T),if(N=6,chiudi6(T),leggiriga7(N)).

        leggiriga7(N):-read(T),if(N=7,chiudi7(T),leggiriga8(N)).

        leggiriga8(N):-read(T),if(N=8,chiudi8(T),leggiriga9).

        leggiriga9:-read(T),chiudi9(T).

        chiudi1(T):-if(clause(eventE(T),_),true,assert(eventE(T))),seen.

        chiudi2(T):-if(clause(evintI(T),_),true,assert(evintI(T))),seen.

        chiudi3(T):-if(clause(az(T),_),true,assert(az(T))),seen.

        chiudi4(T):-assert(condt(T)),seen.

        chiudi5(T):-if(clause(evN(T),_),true,assert(evN(T))),seen.

        chiudi6(T):-if(clause(obtgoal(T),_),true,assert(obtgoal(T))),seen.

        chiudi7(T):-if(clause(testgoal(T),_),true,assert(testgoal(T))),seen.

        chiudi8(T):-if(clause(rem_fact(T),_),true,assert(rem_fact(T))),seen.

        chiudi9(T):-if(clause(even_args(T),_),true,assert(even_args(T))),seen.




    look_past_events:-findall((X,Y,T),clause(past(X,Y,T),_),L),write(L),nl.


%LEARNING

manage_lg(C,F):-update_txt(C,F),token_clause(C,F,F1),name(F,L),append(L,[46,112,108],Lf),name(Tpl,Lf),
append(L,[46,112,108,101],Lf1),name(Tple,Lf1),
if(file_exists(Tple),delete_file(Tple),true),elimina_tag,aprifile(Tpl),togli_var_ple(F),append(L,[46,112,108,102],Lf2),name(Tplf,Lf2),if(file_exists(Tplf),controlla_ev_all(Tple),
  (inizializza_plf(Tple),check_messaggio(Tple,Tplf))),
  load_directives(Tplf),append(L,[46,112,108,118],Lv),name(Tplv,Lv),cond_esterni(Tplv),togli_var_clause(F,F1).

elimina_tag:-retractall(eventE(_)),retractall(evintI(_)),retractall(az(_)),
             retractall(condt(_)),retractall(evN(_)),retractall(obtgoal(_)),
             retractall(testgoal(_)),retractall(rem_fact(_)),
             retractall(even_args(_)).


update_txt(C,F):-name(F,L),append(L,[46,116,120,116],Lf),name(Nf,Lf),open(Nf,append,Stream,[]),
                             write(Stream,C),write(Stream,'.'),
                             nl(Stream),
                          close(Stream).


start_learn:-if(current_predicate(learn_if/3),true,ass_learn).
ass_learn:-assert(learn_if(_,_,_)).

%BLOCCO LEARNING CLAUSE



apri_learn(F):-see(F),
             repeat,
                read(T),expand_term(T,Te),
                            if(T=end_of_file,true,
                            assegna_nome(T,Te)),
                                T == end_of_file,
             !,
             seen.


assegna_nome(T,Te):-functor(Te,_,N),if(N>1,assegna_nome1(T,Te),true).
assegna_nome1(T,Te):-arg(1,Te,Head),fdbg_assign_name(Head,Name),assert(modified_clause(Head,Name)),
assert(txt_clause(Name,T)).


learn_clause(H,Ag):-clause(modified_clause(H,Name),_),
                  clause(txt_clause(Name,T),_),
                  open('prova1%13%.txt',write,Stream,[]),
            format(Stream,"'~p'.",[T]),nl(Stream),
            close(Stream),leggi_l(Ag).

leggi_l(Ag):-clause(agent(A),_),see('prova1%13%.txt'),
       read(T),send_msg_learn(T,A,Ag),
       seen, if(file_exists('prova1%13%.txt'),delete_file('prova1%13%.txt'),true).

learn_all(H,Ag):-findall(Name,clause(modified_clause(H,Name),_),LC),
         last(LC,U),
           open('prova1%13%.txt',write,Stream,[]),
          repeat,
           member(Me,LC),
                  clause(txt_clause(Me,T),_),
                  format(Stream,"'~p'.",[T]),nl(Stream),
           Me==U,!,
           close(Stream),leggi_all(Ag).

leggi_all(Ag):-clause(agent(A),_),
        open('prova1%13%.txt',read,Stream,[]),
        repeat,
          read(Stream,T),
          if((T= end_of_file),true,send_msg_learn(T,A,Ag)),
          T==end_of_file,!,
        close(Stream), if(file_exists('prova1%13%.txt'),delete_file('prova1%13%.txt'),true).

%GESTIONE EVP CONSTRANTS

ct(E,T):-if(var(T),simple_past(E,T),no_var_evp_n(E,T)).
no_var_evp_n(E,T):-functor(T,_,N),if(N=1,simple_past(E,T),composed_past(E,T)).
simple_past(E,T):-clause(past(E,T,_),_).
simple_past(E,T):-clause(remember(E,T,_),_).
composed_past(E,T):-functor(T,F,_),if(F=at,composed_past_ok(E,T),wrt_error_evp(T)).
wrt_error_evp(T):-write('Error in writing time in past constraints: '),write(T),nl.
composed_past_ok(_,_):-true.






% ASP INTERCONNECTION

callasp(Name,Model):-name(Name,L),append(L,[46,116,120,116],Lf),
                name('lparse ',L1),append(L1,Lf,Lp1),
                name('|smodels ',L2),append(Lp1,L2,Lp2),
                name(Model,L5),append(Lp2,L5,Lp5),append(Lp5,[32,62,32],Lp5f),
                append(L,[95,111,117,116,46,116,120,116],Lp3),
                append(L,[95,111,117,116],Lp4),
                name(Ff,Lp4),
                append(Lp5f,Lp3,Lpf),name(T,Lpf),system(T),assert(wait_asp_result(Ff,Name)).





start_planning_process(Ff,Name):-leggiFile(Ff,Fi),tokenize(Fi,L), open('asp_intermedio9993423.txt',write,Stream,[]),examine_asp_token(L,Stream),close(Stream),
leggi_asp_file_intermedio(Name),retractall(wait_asp_result(Ff,Name)).

examine_asp_token([],_).
examine_asp_token([Testa|Coda],Stream):-clause(write_asp_el,_),write(Stream,Testa),
                examine_asp_item2(Testa),
                examine_asp_token(Coda,Stream).

examine_asp_token([Testa|Coda],Stream) :-examine_asp_item1(Testa),examine_asp_token(Coda,Stream).

examine_asp_token([_|Coda],Stream):- examine_asp_token(Coda,Stream).

examine_asp_item1(Testa):-Testa='done',assert(write_asp_el).
examine_asp_item2(Testa):-Testa='False',retractall(write_asp_el).
examine_asp_item2(Testa):-Testa='True',retractall(write_asp_el).



leggi_asp_file_intermedio(Name):-leggiFile('asp_intermedio9993423',Fi),
tokenize(Fi,L),if(file_exists('asp_intermedio9993423.txt'),delete_file('asp_intermedio9993423.txt'),true),open('asp_intermedio9993423.txt',write,Stream,[]),append([start],L,L1),examine_asp_token1(L1,Stream),write(Stream,'.'),close(Stream),leggi_file_open_asp(Name),generate_plan_list(Name).

examine_asp_token1([],_).

examine_asp_token1([Testa|Coda],Stream):-Testa=' ',write(Stream,'.'),nl(Stream),
                           examine_asp_token1(Coda,Stream).
examine_asp_token1([Testa|Coda],Stream):-Testa=':',examine_asp_token1(Coda,Stream).

examine_asp_token1([Testa|Coda],Stream) :-Testa\=' ',write(Stream,Testa),
                                          examine_asp_token1(Coda,Stream).
examine_asp_token1([Testa|Coda],Stream) :-Testa\=':',write(Stream,Testa),
                                          examine_asp_token1(Coda,Stream).


leggi_file_open_asp(Name):-open('asp_intermedio9993423.txt',read,Stream,[]),
                 repeat,
                   read(Stream,T),if(var(T),true,examine_asp_result(T,Name)),
                   T=='end_of_file',!,
                          close(Stream).


examine_asp_result(T,_):-number(T),R is T-1, not(clause(join(R),_)),assert(join(T)).
examine_asp_result(T,_):-number(T),R is T-1,clause(join(R),_),retractall(join(R)),assert(join(T)).
examine_asp_result(T,Name):-not(number(T)),T\='end_of_file', clause(join(X),_),assert(plan(Name,X,T)).
examine_asp_result(T,_):-not(number(T)),T='end_of_file'.


generate_plan_list(Name):-if(not(clause(join(_),_)),no_models(Name),generate_plan_list0(Name)).


generate_plan_list0(Name):-retractall(join(_)),findall(N,clause(plan(Name,N,_),_),L),
                          if(L\=[],generate_plan_list1(Name,L),empty_plan_generated(Name)).

generate_plan_list1(Name,L):-remove_dups(L,L1),
                  last(L1,U),
                          repeat,
                                member(M,L1),
                                   findall(X,clause(plan(Name,M,X),_),Lp),
                                   statistics(walltime,[Tc,_]),
                                        assert(past(plan_list(Name,M,Lp),Tc,program)),
                                M==U,!,
                   retractall(plan(_,_,_)),
           if(file_exists('asp_intermedio9993423.txt'),
           delete_file('asp_intermedio9993423.txt'),true).


empty_plan_generated(Name):-statistics(walltime,[Tc,_]),
                assert(past(plan_list(Name,1,[]),Tc,program)).

no_models(Name):-write('No Stable Models for plan: '),write(Name),nl.





% GALILEO POSITION FUNCTIONS

calcola_time(D,Dn):-R is integer(D),name(R,L),length(L,N),if(N=6,calcola_time_6(L,Dn),calcola_time_tree1(L,N,Dn)).
calcola_time_tree1(L,N,Dn):-if(N=5,calcola_time_5(L,Dn),calcola_time_tree2(L,N,Dn)).
calcola_time_tree2(L,N,Dn):-if(N=4,calcola_time_4(L,Dn),calcola_time_tree3(L,N,Dn)).
calcola_time_tree3(L,N,Dn):-if(N=3,calcola_time_3(L,Dn),calcola_time_tree4(L,N,Dn)).
calcola_time_tree4(L,N,Dn):-if(N=2,calcola_time_2(L,Dn),calcola_time_tree5(L,N,Dn)).
calcola_time_tree5(L,N,Dn):-if(N=1,calcola_time_1(L,Dn),true).



calcola_time_6(L,Dn):-nth1(1,L,H1),nth1(2,L,H2),nth1(3,L,M1),nth1(4,L,M2),nth1(5,L,S1),nth1(6,L,S2),
          name('time(',W0),name(')',U0),append([H1],[H2],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).



calcola_time_5(L,Dn):-nth1(1,L,H2),nth1(2,L,M1),nth1(3,L,M2),nth1(4,L,S1),nth1(5,L,S2),
          name('time(',W0),name(')',U0),append([48],[H2],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).

calcola_time_4(L,Dn):-nth1(1,L,M1),nth1(2,L,M2),nth1(3,L,S1),nth1(4,L,S2),
          name('time(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_time_3(L,Dn):-nth1(1,L,M2),nth1(2,L,S1),nth1(3,L,S2),
          name('time(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_time_2(L,Dn):-nth1(1,L,S1),nth1(2,L,S2),
          name('time(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[48],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_time_1(L,Dn):-nth1(1,L,S2),
          name('time(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[48],HM2),append(HM2,[44],HMv),append(HMv,[48],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).

calcola_date(D,Dn):-name(D,L),length(L,N),if(N=6,
calcola_date_6(L,Dn),calcola_date_tree1(L,N,Dn)).

calcola_date_tree1(L,N,Dn):-if(N=5,calcola_date_5(L,Dn),
calcola_date_tree2(L,N,Dn)).

calcola_date_tree2(L,N,Dn):-if(N=4,calcola_date_4(L,Dn),
calcola_date_tree3(L,N,Dn)).

calcola_date_tree3(L,N,Dn):-if(N=3,calcola_date_3(L,Dn),calcola_date_tree4(L,N,Dn)).

calcola_date_tree4(L,N,Dn):-if(N=2,calcola_date_2(L,Dn),calcola_date_tree5(L,N,Dn)).

calcola_date_tree5(L,N,Dn):-if(N=1,calcola_date_1(L,Dn),true).



calcola_date_6(L,Dn):-nth1(1,L,H1),nth1(2,L,H2),nth1(3,L,M1),nth1(4,L,M2),nth1(5,L,S1),nth1(6,L,S2),
          name('date(',W0),name(')',U0),append([H1],[H2],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).



calcola_date_5(L,Dn):-nth1(1,L,H2),nth1(2,L,M1),nth1(3,L,M2),nth1(4,L,S1),nth1(5,L,S2),
          name('date(',W0),name(')',U0),append([48],[H2],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).

calcola_date_4(L,Dn):-nth1(1,L,M1),nth1(2,L,M2),nth1(3,L,S1),nth1(4,L,S2),
          name('date(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[M1],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_date_3(L,Dn):-nth1(1,L,M2),nth1(2,L,S1),nth1(3,L,S2),
          name('date(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[M2],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_date_2(L,Dn):-nth1(1,L,S1),nth1(2,L,S2),
          name('date(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[48],HM2),append(HM2,[44],HMv),append(HMv,[S1],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).


calcola_date_1(L,Dn):-nth1(1,L,S2),
          name('date(',W0),name(')',U0),append([48],[48],H),append(H,[44],Hv),append(Hv,[48],HM1),
          append(HM1,[48],HM2),append(HM2,[44],HMv),append(HMv,[48],HMS1),
          append(HMS1,[S2],HMS2),append(W0,HMS2,P0),append(P0,U0,HMSf),name(Dn,HMSf).

% TRASFORMA UNA LISTA IN ASCII E VICEVERSA
list_in_ascii(Ln,La):-trasform_list_in_ascii(Ln),create_new_ascii_list(La).

trasform_list_in_ascii(Ln):-last(Ln,U),
          repeat,
                member(Mn,Ln),
                  name(Mn,Ma),
                  assert(list_ascii(Ma)),
                Mn==U,!.

create_new_ascii_list(La):-findall(X,clause(list_ascii(X),_),La),
           retractall(list_ascii(_)).



ascii_in_list(La,Ln):-trasform_ascii_in_list(La),create_new_term_list(Ln).

trasform_ascii_in_list(La):-last(La,U),
          repeat,
                member(Ma,La),
                  name(Tn,Ma),
                  assert(new_this_term(Tn)),
                Ma==U,!.

create_new_term_list(Ln):-findall(X,clause(new_this_term(X),_),Ln),
           retractall(new_this_term(_)).

%TRASFORMA UNA LISTA IN UNA STRINGA

concatena_items_poi_in_string(L,S):-assert(conc_item003('')),
         retractall(k_item003(_)),
         assert(k_item003(1)),length(L,N),
         if(N>1,(parte_prima_conc_items(L,N),
         parte_seconda(L,N,S)),parte_seconda(L,N,S)).
parte_prima_conc_items(L,N):-repeat,
         clause(k_item003(K),_),
         nth1(K,L,M),
         clause(conc_item003(X),_),
         atom_concat(X,M,M1),atom_concat(M1,',',M2),
         retractall(conc_item003(X)),
         assert(conc_item003(M2)),

         R is K+1,assert(k_item003(R)),retractall(k_item003(K)),
         K==N,!.

parte_seconda(L,N,S):-nth1(N,L,M3),
         clause(conc_item003(D),_),
         atom_concat(D,M3,S),
         retractall(k_item003(_)),
         retractall(conc_item003(_)).



% DETERMINA L'INTERSEZIONE DI DUE LISTE

intersection([],_,[]).
intersection([H|T],S2,[H|T3]):- member(H,S2),
                                intersection(T,S2,T3).
intersection([_|T],S2,S3):-intersection(T,S2,S3).


save_on_log_file(P):-clause(agent(N),_),
        name('log/log_',L0),name(N,L1),name('.txt',L2),
        append(L0,L1,L01),append(L01,L2,L02),name(Q,L02),
        open(Q,append,Stream,[]),
        write(Stream,P),nl(Stream),
        close(Stream).

% GENERA UN nUOVO AGENTE
generate_new_agent(U):-generation_path(_,P,LO,Ont,K,Pr),create_initialization_file(U,P,LO,Ont,K),!,
create_comm_file(U,P),create_plf_file(U,P),
create_txt_file(U,P),create_profile_file(U,P,Pr).

create_initialization_file(U,P,LO,Ont,K):-
           atom_concat(P,'initialization_dali_files/',I),
           number_in_term(I,U,I1),
           atom_concat(I1,'.txt',I2),
           atom_concat(P,'program/',Y),
           number_in_term(Y,U,Y1),
           atom_concat(P,LO,R),
           atom_concat(P,'communication',C),
           number_in_term(C,U,C1),
           atom_concat(P,'communication_fipa',E),
           atom_concat(P,'learning',J),
           atom_concat(P,'planasp',D),
           atom_concat(P,'user_profiles/',S),
           atom_concat(S,'profile_',S1),
           number_in_term(S1,U,S2),
           atom_concat(S2,'.txt',S3),
           atom_concat(P,'onto/',B),
           atom_concat(B,Ont,B1),
           atom_concat(B1,'.txt',B2),
           open(I2,append,Stream,[eof_action(reset)]),
           write(Stream,'agent('),
           write(Stream,'\''),
           write(Stream,Y1),
           write(Stream,'\','),
           write(Stream,U),
           write(Stream,',\''),
           write(Stream,R),
           write(Stream,'\','),
           write(Stream,'italian,'),
           write(Stream,'[\''),
           write(Stream,C1),
           write(Stream,'\'],'),
           write(Stream,'[\''),
           write(Stream,E),
           write(Stream,'\',\''),
           write(Stream,J),
           write(Stream,'\',\''),
           write(Stream,D),
           write(Stream,'\'],\''),
           write(Stream,S3),
           write(Stream,'\',\''),
           write(Stream,B2),
           write(Stream,'\','),
           write(Stream,K),
           write(Stream,').'),
           close(Stream).


divide_host_port(L,A1,A2):-manage_host_port_list(L,A1,A2).
manage_host_port_list([],_,A2):-findall(X,code_asc(X),L1),
                           name(A2,L1),retractall(code_asc(_)).

manage_host_port_list([A|B],A1,A2):-A=58,findall(X,code_asc(X),L1),
                           name(A1,L1),retractall(code_asc(_)),manage_host_port_list(B,A1,A2).


manage_host_port_list([A|B],A1,A2):-A=\=58,assert(code_asc(A)),
                              manage_host_port_list(B,A1,A2).


create_comm_file(U,P):-duplicate_file_comm(U,P,'communication').

number_in_term(I,U,I1):-name(I,L),name(U,L1),append(L,L1,L2),name(I1,L2).

duplicate_file_comm(U,P,S):-
   atom_concat(P,S,K),
   atom_concat(K,'.con',K1),
   open(K1,read,Stream,[]),
   repeat,
   get_code(Stream,T),
   assert(base_cod_term(T)),
   T==(-1),!,
   close(Stream),
   atom_concat(P,S,S0),
   number_in_term(S0,U,S1),

   atom_concat(S1,'.con',S2),
   open(S2,append,Stream,[]),
   findall(X,base_cod_term(X),L),append(L1,[-1],L),append(L1,[38],L2),
   repeat,
   member(M,L2),
   name(B,[M]),
   if(M=38,true,write(Stream,B)),
   M==38,!,
   close(Stream),
   retractall(base_cod_term(_)).

create_plf_file(U,P):-duplicate_file_plf(U,P,'user_assistant_plf').

duplicate_file_plf(U,P,S):-
   atom_concat(P,S,K),
   atom_concat(K,'.plf',K1),
   open(K1,read,Stream,[]),
   repeat,
   get_code(Stream,T),
   assert(base_cod_term(T)),
   T==(-1),!,
   close(Stream),
   atom_concat(P,'program/',S1),
   number_in_term(S1,U,S2),
   atom_concat(S2,'.plf',S3),
   open(S3,append,Stream,[]),
   findall(X,base_cod_term(X),L),append(L1,[-1],L),append(L1,[38],L2),
   repeat,
   member(M,L2),
   name(B,[M]),
   if(M=38,true,write(Stream,B)),
   M==38,!,
   close(Stream),
   retractall(base_cod_term(_)).

create_txt_file(U,P):-generation_path(NF,_,_,_,_,_),duplicate_file_txt(U,P,NF).

duplicate_file_txt(U,P,S):-
   atom_concat(P,S,K),
   atom_concat(K,'.txt',K1),
   open(K1,read,Stream,[]),
   repeat,
   get_code(Stream,T),
   assert(base_cod_term(T)),
   T==(-1),!,
   close(Stream),
   atom_concat(P,'program/',S1),
   number_in_term(S1,U,S2),
   atom_concat(S2,'.txt',S3),
   open(S3,append,Stream,[]),
   findall(X,base_cod_term(X),L),append(L1,[-1],L),append(L1,[38],L2),
   repeat,
   member(M,L2),
   name(B,[M]),
   if(M=38,true,write(Stream,B)),
   M==38,!,
   close(Stream),
   retractall(base_cod_term(_)).


create_profile_file(U,P,Pr):-atom_concat(P,'user_profiles/profile_',S1),
           number_in_term(S1,U,S2),
           atom_concat(S2,'.txt',S3),if(file_exists(S3),true,duplicate_file_profile(U,P,Pr)).

duplicate_file_profile(U,P,S):-
           atom_concat(P,S,K),
           atom_concat(K,'.txt',K1),
           open(K1,read,Stream,[]),
           repeat,
           get_code(Stream,T),
           assert(base_cod_term(T)),
           T==(-1),!,
           close(Stream),
           atom_concat(P,'user_profiles/profile_',S1),
           number_in_term(S1,U,S2),
           atom_concat(S2,'.txt',S3),
           open(S3,append,Stream,[]),
           findall(X,base_cod_term(X),L),append(L1,[-1],L),append(L1,[38],L2),
           repeat,
           member(M,L2),
           name(B,[M]),
           if(M=38,true,write(Stream,B)),
           M==38,!,
           close(Stream),
           retractall(base_cod_term(_)).

% Controllo sull'intervallo di simultaneit� durante l'arrivo degli eventi prima che diventino passati
simultaneity_interval(E):- once(deltat(X)), assert(deltatime(X)),clause(agente(_,_,S,_),_),leggiriga(S,1),clause(eventE(Es),_),assert(wishlist(Es)), 
             controllo_eventi(E).

controllo_eventi(E):- if(verifica_tstart,existFirstTime(E),startFirstTime(E)).
verifica_tstart:- clause(tstart(_),true).
startFirstTime(E):- now(Time),assert(tstart(Time)), L=[], append([E],L,L1), write('This event is first events:'), write(L1), nl, assert(mem_current(L1)),total_member.
existFirstTime(E):- if(check_while, controllo_intervallo(E), divento_pass_and_first(E)).
check_while:- now(Time), deltatime(T), tstart(T0), Time-T0=<T. 
controllo_intervallo(E):- if(check_while, operation_list(E) , divento_pass ).
operation_list(E):-aggiorno_lista(E), if(verifica_lista,divento_pass,(write('Do not arrive all events'), nl)).
total_member:- if(verifica_lista,divento_pass,(write('Do not arrive all events'), nl)).
aggiorno_lista(E):- mem_current(L), append([E],L,L1),write('This is updated list:'),write(L1),nl, retract(mem_current(L)), assert(mem_current(L1)).
verifica_lista:- remove_dup, verifica_contenuto. 
remove_dup:- mem_current(L), remove_dups(L, L1), write('This is list without duplicates:'),write(L1),nl, assert(mem_no_dup(L1)).
verifica_contenuto:- check_lenght, contain.
check_lenght:- wishlist(L), mem_no_dup(L1),same_length(L,L1). 
contain:- mem_no_dup(L1), if(L1=[],true , scorri_member).
scorri_member:- mem_no_dup(L1), nth0(0,L1,X,L_rest), retract(mem_no_dup(L1)), assert(mem_no_dup(L_rest)), contain. 
divento_pass:- retract(tstart(_)),svuota_lista, passato.
svuota_lista:- mem_current(L), assert(mem_past(L)), retract(mem_current(L)), write('This is list of past event:'), write(L), nl.
passato:- mem_past(L), if(L=[],true,scorri_past(L)).
scorri_past(L):- nth0(0,L,E,L1), now(T), become_past(E,T), retract(mem_past(L)), assert(mem_past(L1)), passato.
divento_pass_and_first(E):-retract(tstart(_)),svuota_lista, passato, startFirstTime(E).











