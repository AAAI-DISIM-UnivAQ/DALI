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
:-['remove_var.pl'].  % translated from 'togli_var.pl'
:-['memory.pl'].

:-['examine_past_constraints.pl'].

:-dynamic tesg/1.
:-dynamic ontology/2.
:-dynamic en/1.

:-dynamic told/6.
:-dynamic export_past/1.
:-dynamic export_past_do/1.
:-dynamic export_past_not_do/1.

:-dynamic deltat/1, deltatime/1, simultaneity_interval/1, wishlist/1, tstart/1, mem_current/1, mem_no_dup/1, mem_past/1, check_list/1.
:-dynamic user_profile_location/1,dali_onto_location/1,server_obj/1, specialization/1,own_language/1, agent_x/4, agent_x/1, time_charge/1, to_add/1.
:-dynamic rule_base/1, mul/1,no_check/0,past/3,tep/2,fact_mul/2,azi/1,even/1,evin/1,continue_mul_f/1,evN/1.

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

:-['read_mul.pl'].  % translated from 'leggi_mul.pl'

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
            agent_x(File, AgentName, Ontolog, Lang, Fil, Lib, UP, DO, Specialization) = Me,
            open('server.txt',read,Stream2,[]),read(Stream2,T),close(Stream2),
            if(UP=no, true, assert(user_profile_location(UP))),
            if(DO=no,true,assert(dali_onto_location(DO))),
            assert(server_obj('localhost':3010)), %% this can be removed if we move to a single function
            filter_fil(Fil),
            assert(specialization(Specialization)),
            if(Ontolog=no,true,load_ontology_file(Ontolog,AgentName)),
            assert(own_language(Lang)),

            linda_client('localhost':3010),
            out(activating_agent_x(AgentName)),

            delete_agent_x_files(File),
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

filter_fil(FI):-arg(1,FI,File),token_fil(File),retractall(parentheses(_)),remove_var_fil(File).

start1(Fe,AgentName,Libr,Fil):-
  set_prolog_flag(discontiguous_warnings,off),
  if(Libr=no,true,library(Fe,Libr,Fil)),

  pl_from_name(Fe, FilePl),
  ple_from_name(Fe, FilePle),
  plv_from_name(Fe, FilePlv),
  plf_from_name(Fe, FilePlf),
  txt_from_name(Fe, FileTxt),

  open_file(FilePl),

  open_file_res(FilePl),

  load_file(FilePl),

  remove_var(Fe),

  remove_var_ple(Fe),

  if(file_exists(FilePlf),check_ev_all(FilePle), (initialize_plf(FilePle), check_message(FilePle, FilePlf))),

  load_directives(FilePlf),
  server_obj(Tee), %% this can be removed if we move to a single function
  linda_client(Tee),
  assert(agent_x(AgentName,Tee,FilePle,FilePl)),
  clause(specialization(Sp),_),
  out(specialized_to(AgentName,Tee,Sp)),
  assert(agent_x(AgentName)),
  in_noblock(activating_agent_x(AgentName)),
  out(agent_x_active(AgentName,Tee)),
  assert(time_charge(5)),

  actions(FilePlv),

  external_conditions(FilePlv),!,

  assert_evN(FilePle),

  obg_goal(FilePlv),

  open_file_en(FilePl),

  assert_internal_repeat,

  assert_strings_mul(FilePlv),

  compile(FilePlv),

  open_learn(FileTxt),
  start_learn,
  manage_export_past,
  manage_export_past_not_do,
  manage_export_past_do,
  check_constr_all,
  delete_agent_x_log_file(AgentName),
  print('..................   Activated Agent '),print(AgentName),print(' ...................'),nl,go.

%L0 should be communicationf/fipa
library(F,L0,Fil):-name(F,Lf),append(Lf,[46,112,108],Ltf),
           append(L0,Fil,L),
           name(F1,Ltf),if(L=[],true,library1(F1,L)).

library1(F,L):-last(L,U),
                   repeat,
                          member(Me,L),
                                  append_rules0(Me),
                   Me==U,!,pour(F).

append_rules0(Me):-name(Me,L),append(L,[46,116,120,116],Ltf),
                           name(T,Ltf),if(file_exists(T),append_files(T),true),
                           append(L,[46,112,108],Ltf1),name(T1,Ltf1),
                           if(file_exists(T1),append_rules(T1),true).

append_files(T):-set_prolog_flag(redefine_warnings,off),compile(T).

append_rules(Mef):-open(Mef,read,Stream,[]),
                           repeat,
                                         read(Stream,T),if(T=end_of_file,true,
                                         assert(to_add(T))),
                                T==end_of_file,!, 
                close(Stream).

%%Write communication file
pour(F):-findall(X,clause(to_add(X),_),L),
          last(L,U),
              open(F,append,Stream,[]),nl(Stream),
              repeat,
              member(T,L),
              write(Stream,T),write(Stream,'.'),nl(Stream),
              T==U,!, 
              close(Stream),retractall(to_add(_)).

open_file(F):-see(F),
             repeat,
                read(T),expand_term(T,Te),
                            if(T=end_of_file,true,
                            assert(rule_base(Te))),
                                T == end_of_file,
             !,
             seen,take,build0(F).

take:-findall(T,clause(rule_base(T),_),L),
          last(L,U),
          repeat,
          member(M,L),
               split(M),

           M==U,!,retractall(rule_base(_)), if(clause(mul(_),_),assert_mul_first,true).

examine_mul:-if(clause(mul(_),_),examine1_mul,true).
examine1_mul:-findall(L,clause(mul(L),_),S),
              last(S,E),
              repeat,member(Me,S),keep_past(Me),
             if(clause(no_check,_),retractall(no_check),call_cong(Me)),
             Me==E,!.

keep_past(Me):- append([eve],L,Me),examine_head(L).

examine_head(L):-last(L,U),repeat,member(Me,L),check_past(Me),Me==U,!.

check_past(Me):-if(clause(past(Me,_,_),_),true,assert(no_check)).

call_cong(Me):-T=..Me,append([eve],L,Me),check_time_ep(L,T),take_time_ep(L).

check_time_ep(L,T):-last(L,U),repeat,member(M,L),
                                clause(past(M,Tp,_),_),assign_what(tep(M,Tp)),
                 M==U,!,cont_tep1(L,T).

cont_tep1(L,T):-findall(X,clause(tep(_,X),_),Le),retractall(tep(_,_)),
                if(clause(fact_mul(L,Le),_),true,call(T) ).

take_time_ep(L):-last(L,U),repeat,member(M,L),clause(past(M,Tp,_),_),assign_what(tep(M,Tp)),
                   M==U,!,cont_tep(L).
cont_tep(L):-findall(X,clause(tep(_,X),_),Le),retractall(tep(_,_)),
          if(clause(fact_mul(_,_),_),(retractall(fact_mul(_,_)),assert(fact_mul(L,Le))),assert(fact_mul(L,Le))).

split(C):-arg(1,C,Head),C=..L,eve_mul_first(Head),if(member(':-',L),is_clause(L,Head),true).

is_clause(L,Head):-append([':-'],L1,L),execute(L1,Head).
execute([],_).
execute([S1|Rest],Head):-execute(S1,Head),!,execute(Rest,Head).
execute((X,Y),Head):-expand_term((X,Y),Z),execute(Z,Head).
execute((X;Y),Head):-expand_term((X;Y),Z),execute(Z,Head).
execute((X->Y),Head):-expand_term((X->Y),Z),execute(Z,Head).
execute(X,Head):-if(X=[],true,try_split(X,Head)).
try_split(M,Head):-functor(M,F,_),if(F=a,(arg(1,M,Az),discriminate_learn(Az)),if(F=eve,(arg(1,M,Es),assign_what(even(Es))),
          (if(F=evi,(arg(1,M,Iv),assign_what(evin(Iv))),
          (if(F=cd,(arg(1,M,Co),assign_what(cond(Co))),
          (if(F=en,(arg(1,M,En),assign_what(evN(En))),if(F=rem,(arg(1,M,Re),assign_what(fact_rem(Re))),case_if(M,F,Head))))))))).

case_if(M,F,Head):-if(F=if,go_to_if(M,Head),if(F=obg,(arg(1,M,G),assign_what(obt_goal(G))),
           if(F=tesg,(arg(1,M,Go),assign_what(test_goal(Go))),true))).
go_to_if(M,Head):-arg(2,M,A1),arg(3,M,A2),execute(A1,Head),execute(A2,Head).

assign_what(C):-if(clause(C,_),true,assert(C)).

discriminate_learn(Az):-functor(Az,F,_),
                if(F=message,discriminate_learn1(Az),assign_what(azi(Az))).

discriminate_learn1(Az):-arg(2,Az,Pe),functor(Pe,F,_),if(F=confirm,discriminate_learn2(Pe,Az),assign_what(azi(Az))).

discriminate_learn2(Pe,Az):-arg(1,Pe,Le),functor(Le,F,_),if(F=learn,discriminate_learn3,assign_what(azi(Az))).

discriminate_learn3:-assert(azi(message(_254802,confirm(learn(_254655),_254800)))).

build0(F):-if((((clause(even(_),_);clause(evin(_),_));clause(azi(_),_));clause(evN(_),_)),
                     recover_fun0(F),true).
recover_fun0(F):-recover_funE(F),recover_funI(F),recover_funA(F),recover_tot_cd(F),recover_funEn(F),recover_gol_obt(F),recover_gol_test(F),recover_tot_rem(F),recover_E(F).

%ASSERTS EXTERNAL EVENTS RELATED TO MULTIPLE EVENTS
eve_mul_first(Head):-functor(Head,_,N),Head=..L_eve,if((arg(1,L_eve,_),N>1),continue_mul_f(L_eve),true).
continue_mul_f(L_eve):-arg(1,L_eve,X_eve),if((X_eve=eve,is_list(L_eve)),
                     assert_this(mul(L_eve)),true).
ass_mul_first:-findall(L,clause(mul(L),_),S),last(S,E),repeat,member(Me,S),
         keep_past_ass_first(Me),Me==E,!,retractall(mul(_)).

keep_past_ass_first(Me):- append([eve],L,Me),examine_head_ass_first(L).

examine_head_ass_first(L):-last(L,U),repeat,member(Me,L),assert_this(even(Me)),
                 Me==U,!.

%RECOVERY OF EXTERNAL EVENT FUNCTORS%
recover_funE(F):-if(clause(even(_),_),(transf_external_events, recover_fun_even(F)),empty_list(F)).  % translated from 'recupera_funE' and 'lista_assente'
transf_external_events:-findall(X,clause(even(X),_),Ls),  % translated from 'transf_eventi_esterni'
                last(Ls,U),
                         repeat,
                                member(Me,Ls),
                                         functor(Me,Fu,_),
                                         assert(app_even(Fu)),
                Me==U,!.

recover_fun_even(F):- name(F,L),  % translated from 'recupera_fun_even'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(app_even(X),_),LA1),
                        remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                                  retractall(app_even(_)).

%RECOVERY OF INTERNAL EVENT FUNCTORS%
recover_funI(F):-if(clause(evin(_),_),recover_fun_evin(F),empty_list(F)).  % translated from 'recupera_funI' and 'lista_assente'

recover_fun_evin(F):- name(F,L),  % translated from 'recupera_fun_evin'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(evin(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(evin(_)).

%RECOVERY OF ACTIONS%
recover_funA(F):-if(clause(azi(_),_),recover_fun_actions(F),empty_list(F)).  % translated from 'recupera_funA' and 'lista_assente'

transf_message(Me):-arg(2,Me,Ar),functor(Ar,Far,_),assert(app_azi(message(Far))).  % translated from 'transf_message'
recover_fun_actions(F):- name(F,L),  % translated from 'recupera_fun_azioni'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(azi(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(azi(_)).

%RECOVERY OF CONDITIONS-ACTIONS%
recover_tot_cd(F):-if(clause(cond(_),_),recover_cd(F),empty_list(F)).  % translated from 'recupera_tot_cd' and 'lista_assente'

recover_cd(F):- name(F,L),  % translated from 'recupera_cd'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(cond(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(cond(_)).

%RECOVERY OF PRESENT EVENT FUNCTORS%
recover_funEn(F):-if(clause(evN(_),_),(transf_present_events, recover_fun_evN(F)),empty_list(F)).  % translated from 'recupera_funEn' and 'lista_assente'
transf_present_events:-findall(X,clause(evN(X),_),Ls),  % translated from 'transf_eventi_presente'
                last(Ls,U),
                         repeat,
                                member(Me,Ls),
                                         functor(Me,Fu,_),
                                         assert(app_evN(Fu)),
                Me==U,!.

recover_fun_evN(F):- name(F,L),  % translated from 'recupera_fun_evN'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(app_evN(X),_),LA1),
                         remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                         write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                  retractall(app_evN(_)),retractall(evN(_)).

%RECOVERY OF GOALS TO OBTAIN%
recover_gol_obt(F):-if(clause(obt_goal(_),_),recover_fun_obt_goal(F),empty_list(F)).  % translated from 'recupera_gol_obt' and 'lista_assente'

recover_fun_obt_goal(F):- name(F,L),  % translated from 'recupera_fun_obt_goal'
           append(L,[101],T),
           name(Y,T),
                        findall(X,clause(obt_goal(X),_),LA1),
                        remove_dups(LA1,LA),
                        open(Y,append,Stream,[]),
                        write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                        retractall(obt_goal(_)).

%RECOVERY OF GOALS TO TEST%
recover_gol_test(F):-if(clause(test_goal(_),_),(transf_test_goal, recover_fun_test_goal(F)),empty_list(F)).  % translated from 'recupera_gol_test' and 'lista_assente'
transf_test_goal:-findall(X,clause(test_goal(X),_),Ls),last(Ls,U),  % translated from 'transf_tes_goal'
                 repeat,
                        member(Me,Ls),
                                 functor(Me,Fu,_),
                                 assert(app_test_goal(Fu)),
                                Me==U,!.

recover_fun_test_goal(F):- name(F,L),  % translated from 'recupera_fun_tes_goal'
                   append(L,[101],T),
                   name(Y,T),
                                findall(X,clause(app_test_goal(X),_),LA1),
                                 remove_dups(LA1,LA),
                                open(Y,append,Stream,[]),
                                 write(Stream,LA),write(Stream,'.'),nl(Stream), close(Stream),
                          retractall(app_test_goal(_)).

%RECOVERY OF FACTS TO REMEMBER AND MANAGE AS PAST EVENTS%
recover_tot_rem(F):-  % translated from 'recupera_tot_rem'
        if(clause(fact_rem(_),_), recover_rem(F),empty_list(F)).

recover_rem(F):-findall(X,clause(fact_rem(X),_),Ls1),  % translated from 'recupera_rem'
        remove_dups(Ls1,Ls),
    name(F,L),
        append(L,[101],T),
        name(Y,T),
                 open(Y,append,Stream,[]),
                 write(Stream,Ls),write(Stream,'.'),nl(Stream), close(Stream),
          retractall(fact_rem(_)).

empty_list(F):- name(F,L),  % translated from 'lista_assente'
        append(L,[101],T),
        name(Y,T),open(Y,append,Stream,[]),
                 write(Stream,[]),write(Stream,'.'),nl(Stream), close(Stream).

%RECOVERY OF EXTERNAL EVENTS%
recover_E(F):-if(clause(even(_),_),recover_tot_even(F),empty_list(F)).  % translated from 'recupera_E'

recover_tot_even(F):- name(F,L),  % translated from 'recupera_tot_even'
   append(L,[101],T),
   name(Y,T),
                findall(X,clause(even(X),_),LA1),
                 remove_dups(LA1,LA),
                open(Y,append,Stream,[]),
                 write(Stream,LA),write(Stream,'.'),nl(Stream),
                 close(Stream),
  retractall(even(_)).

%INITIALIZES THE PLF FILE OF DIRECTIVES%
initialize_plf(F):-if(file_exists(F),read_lines(F),true).  % translated from 'inizializza_plf'
read_lines(F):-read_line(F,1),read_line(F,2),read_line(F,3),read_line(F,8).  % translated from 'leggirighe'

disassemble(E,F):-last(E,Ls),  % translated from 'smonta'
           name(F,Fe),
           append(L0,[101],Fe),
           append(L0,[102],Nf),
           name(Nof,Nf),
           priority_actions(F,Nof),  % translated from 'priority_azi'
           priority_events(F,Nof),  % translated from 'priority_eve'
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

disassemble_internal(E,F):- if(E=[],true,disassemble_internal1(E,F)).  % translated from 'smonta_interno'
disassemble_internal1(E,F):- name(F,Fe),  % translated from 'smonta_interno1'
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

priority_actions(Fe,Ff):-read_line(Fe,3),
           clause(az(L),_),if(L=[],true,priority_actions1(L,Ff)).

priority_actions1(L,Ff):-last(L,U),
                repeat,
                        member(Me,L),
                                open(Ff,append,Stream,[]),
                                write(Stream,'action('),write(Stream,Me),write(Stream,','),write(Stream,'normal'),
                                write(Stream,').'),nl(Stream),
                                close(Stream),
                        Me==U,!.

priority_events(Fe,Ff):-read_line(Fe,1),
           clause(eventE(L),_),if(L=[],true,priority_events1(L,Ff)).

priority_events1(L,Ff):-last(L,U),
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

check_all_events(F):-  % translated from 'controlla_ev_all'
           read_line(F,2),clause(evintI(Li),_),
           read_line(F,3),clause(az(La),_),
           read_line(F,6),clause(obtgoal(Lo),_),
           read_line(F,7),clause(testgoal(Lt),_),
           read_line(F,8),clause(rem_fact(Lr),_),
           read_line(F,9),clause(even_args(Lg),_),
           append(Li,La,L1),append(L1,Lo,L2),append(L2,Lt,L3),
           append(L3,Lr,L4),append(L4,Lg,L6),
                   name(F,List),append(List1,[101],List),
                   append(List1,[102],List3),name(Plf,List3),
                   see(Plf),
                        repeat,
                        read(T),
                           assert(temporary_values(T)),  % translated from 'valori_temporanei'
                          T==end_of_file,
                         seen,!,
                          findall(X,clause(temporary_values(past_event(X,_)),_),Ls),
                          diff(Ls,L6,Ld1),
                          diff(L6,Ls,Ld2),
                          append(Ld1,Ld2,Ldf),
                          if(Ldf=[], true,
                          (different_events_all(L6),rewrite(F,Plf))),  % translated from 'diversi_ev_all' and 'riscrivi'
                          retractall(temporary_values(_)).

different_events_all(L):-assert(k110055(1)),length(L,N),  % translated from 'diversi_ev_all'
         repeat,
                 clause(k110055(K),_),
                 nth1(K,L,M),
                 write_lines124(M),  % translated from 'scrivi_l124'
                 R is K+1,assert(k110055(R)),retractall(k110055(K)),
      K==N,!,retractall(k110055(_)).

write_lines124(M):-if(clause(temporary_values(past_event(M,_)),_),true,assert(to_write(M))),!.  % translated from 'scrivi_l124'

rewrite(F,Plf):-  % translated from 'riscrivi'
                findall(E1,clause(to_write(E1),_),Ls1),
                if(Ls1=[],true,write_to_write(Ls1,Plf,F)).  % translated from 'scrivi_da_scr'

write_to_write(Ls1,Plf,F):-assert(k22(1)),length(Ls1,N),  % translated from 'scrivi_da_scr'
         repeat,
         clause(k22(K),_),
         nth1(K,Ls1,X),
                         clause(to_write(X),_),
                         open(Plf,append,Stream,[]),
                         write(Stream,remember_event_mod(X,number(5),last)),write(Stream,'.'),nl(Stream),
                         write(Stream,past_event(X,60)),write(Stream,'.'),nl(Stream),
                         close(Stream),
                         belongs_to_event_set(X,Plf,F),  % translated from 'appart_set_event'
                          R is K+1,assert(k22(R)),retractall(k22(K)),
         K==N,!,retractall(k11(_)),retractall(to_write(_)).

belongs_to_event_set(X,Plf,F):-read_line(F,1),clause(eventE(Le),_),  % translated from 'appart_set_event'
        read_line(F,2),clause(evintI(Li),_),
           read_line(F,3),clause(az(La),_),functor(X,Fun,_),
           if(member(Fun,Le),write_ext_event(Fun,Plf),true),  % translated from 'writ_est_event'
           if(member(X,Li),write_int_event(X,Plf),true),  % translated from 'writ_int_event'
           if(member(X,La),write_action_event(X,Plf),true).  % translated from 'writ_actn_event'

write_ext_event(X,Plf):-open(Plf,append,Stream,[]),  % translated from 'writ_est_event'
                  write(Stream,'external_event('),write(Stream,X),write(Stream,','),
                  write(Stream,'normal'),
                  write(Stream,').'),nl(Stream),
                 write(Stream,'mod('),write(Stream,X),write(Stream,',check).'),
                 nl(Stream),close(Stream).

write_int_event(X,Plf):- open(Plf,append,Stream,[]),  % translated from 'writ_int_event'
           write(Stream,'internal_event('),write(Stream,X),write(Stream,','),
           write(Stream,'3'),write(Stream,','),write(Stream,forever),
           write(Stream,','),write(Stream,true),write(Stream,','),write(Stream,'until_cond(past('),
           write(Stream,X), write(Stream,'))).'),nl(Stream), close(Stream).

write_action_event(X,Plf):- functor(X,F,_),  % translated from 'writ_actn_event'
         if(F=message,write_msg_mod(X,Plf),write_action_mod(X,Plf)).  % translated from 'scr_msg_mod' and 'scr_act_mod'

write_action_mod(X,Plf):-open(Plf,append,Stream,[]),  % translated from 'scr_act_mod'
         write(Stream,'action('),write(Stream,X),write(Stream,','),
         write(Stream,'normal'),
         write(Stream,').'),nl(Stream),close(Stream).

write_msg_mod(X,Plf):-open(Plf,append,Stream,[]),  % translated from 'scr_msg_mod'
         write(Stream,'action('),write(Stream,X),write(Stream,','),
         write(Stream,'normal'),
         write(Stream,').'),nl(Stream),
         write(Stream,'mod('),write(Stream,X),write(Stream,',check).'),
         nl(Stream),close(Stream).

%DETERMINES IF AN ACTION (message) SHOULD BE SUBJECTED TO CHECK OR NOT
check_message(Fe,Ff):-read_line(Fe,3),  % translated from 'check_messaggio'
           clause(az(L),_),if(L=[],true,check_messages(L,Ff)).  % translated from 'check_mess'

check_messages(L,Ff):-last(L,U),  % translated from 'check_mess'
          repeat,
                 member(Me,L),
                 functor(Me,Fu,_),
                 if(Fu=message,write_plf_string(Me,Ff),true),  % translated from 'ass_plf_string'
          Me==U,!.

write_plf_string(Me,Ff):-open(Ff,append,Stream,[]),  % translated from 'ass_plf_string'
           write(Stream,'mod('),write(Stream,Me),write(Stream,','),
           write(Stream,'check'),write(Stream,').'),nl(Stream),
           close(Stream).

%UPDATES THE CONDITIONS OF EXTERNAL EVENTS
write_cond_extevent(X,Plf):-name(Plf,L),append(Li,[102],L),append(Li,[118],Lj),name(F,Lj),clause(agent_x(_,_,S,_),_),read_line(S,4),  % translated from 'writ_cond_extevent'
        clause(condt(Y),true),
        retractall(condt(_)),process_external_events(X,Y,F).  % translated from 'rip_esterni'

%LOADS THE DIRECTIVES FROM THE .plf FILE%
load_directives(F):-open(F,read,Stream,[]).  % translated from 'carica_le_direttive'
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
                          split1man(A,A1),
                A==Lu,!.

        %T è tutto, mentre A è la TESTA
        split1man(T,A):-T=..L,member(X,L),if(member(X,[':-',',']),true,split2man(X,A)).

        split2man(X,A):-functor(X,_,N),if(N>1,split1man(X,A),split3man(X,A)).

        % Cioè se cia esattamente un argomento
        split3man(X,A):-functor(X,H,N),if(N>0,continman(X,H,A),true).

        continman(X,H,A):-arg(1,X,I),if(H=en,( assert_this(linked_en(A,I)), write(A),nl,write(I),nl ),true).

%RICEZIONE EVENTI ESTERNI%
receive_message:-clause(agent_x(Ag,Ind,_,_),_),
         if(rd_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
         receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con),true).

receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con):-
          assert_this(ext_agent_x(AgM,IndM,Ontology,Language)),
          in_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
          if(clause(receive(Con),_),call_con(AgM,IndM,Language,Ontology,Con),not_receivable_meta(AgM,IndM,Language,Ontology,Con)).

call_con(AgM,IndM,Language,Ontology,Con):-if(receive(Con),true,not_receivable_message(AgM,IndM,Language,Ontology,Con)).

not_receivable_meta(AgM,IndM,Language,Ontology,Con):-write('Message not receivable: '),write(Con),nl,
                                                    assert_this(not_receivable(AgM,IndM,Language,Ontology,Con)).

not_receivable_message(AgM,IndM,Language,Ontology,Con):-write('Message not receivable: '),write(Con),nl,
                                                       assert_this(not_receivable(AgM,IndM,Language,Ontology,Con)).

send_message(AgM,IndM,Language,Ontology,Con):-clause(agent_x(Ag,Ind,_,_),_),
                                             out(message(IndM,AgM,Ind,Ag,Language,Ontology,Con)).

send_message_learn(AgM,IndM,Language,Ontology,Con):-clause(agent_x(Ag,Ind,_,_),_),
                                                   out(message(IndM,AgM,Ind,Ag,Language,Ontology,Con)),
                                                   assert_this(sent_message(AgM,IndM,Language,Ontology,Con)).

internal_event:-clause(agent_x(_,_,S,_),_),
        read_line(S,2),clause(evintI(L),_),
        if(L\=[],check_internal_event1(L,S),true).

check_internal_event1(L,S):-read_line(S,3),
  clause(az(As),_),
  retractall(evintI(_)),
  retractall(az(_)),
  if(As=[],freq_all1(L),freq_single1(L,As)).

freq_all1(L):-last(L,Ul),
        repeat,
        member(Me,L),
        assert_prov0(Me),
    Me==Ul,!.

freq_single1(L,As):-last(L,Ul),
        repeat,
        member(Me,L),
        if(member(Me,As),true,assert_prov0(Me)),
    Me==Ul,!.

assert_prov0(Me):-if(clause(prov_int0(Me),_),true,assert(prov_int0(Me))).

internal_event_goal:-findall(M,clause(prov_int0(M),_),L),
         if(L=[],true,internal_event_goal1(L)).

internal_event_goal1(L):-clause(agent_x(_,_,S,_),_),
        read_line(S,6),
        clause(obtgoal(X),true),if(X=[],move_to_prov_int(L),internal_event_goal21(X,L)).

internal_event_goal21(X,L):-last(X,U),
              repeat,
                member(Me,X),
                  functor(Me,F,_),
                  assert_this(see_goal(F)),
              Me==U,!,internal_event_goal2(L).

internal_event_goal2(L):-findall(V,clause(see_goal(V),_),Lv),
              last(L,U),
              repeat,
                member(Me,L),
                functor(Me,F,_),
                  if(member(F,Lv),ex_internal_event_goal(Me),assert_prov(Me)),
              Me==U,!,retractall(see_goal(_)).

ex_internal_event_goal(Me):-if(clause(activate_goal(Me),_),assert_prov(Me),unload_prov(Me)).

move_to_prov_int(L):-last(L,U),
              repeat,
                member(Me,L),
                  assert_prov(Me),retractall(prov_int0(Me)),
              Me==U,!.

assert_prov(Me):-if(clause(prov_int(Me),_),true,assert(prov_int(Me))).
unload_prov(Me):-if(clause(prov_int(Me),_),retractall(prov_int(Me)),true).

internal_event0:-if(clause(prov_int(_),_),internal_event01,true).
internal_event01:-findall(M,prov_int(M),L),
         last(L,U),
         repeat,
           member(Me,L),
             clause(internal_event(Me,_,_,_,C),_),
             functor(C,F,_),
             if(F=forever,load(Me),internal_event011(Me,F,C)),
        Me==U,!.

internal_event011(Me,C0,C):-if(C0=until_cond,examine_cd(Me,C),internal_event012(Me,C0,C)).
internal_event012(Me,C0,C):-if(C0=in_date,examine_dt(Me,C),print('Writing_error1')).
examine_cd(Me,C):-arg(1,C,C1),if(clause(C1,true),unload(Me),load(Me)).
examine_dt(Me,C):-arg(1,C,D),arg(2,C,A),verify_time(Me,D,A).

verify_time(Iv,D,A):-datime(C),arg(1,C,Ac),arg(1,D,A1),arg(1,A,A2),
              if((Ac>A1,Ac<A2),load(Iv),if((Ac=A1,Ac=\=A2),
              conf_date_max(Iv,D,A,C),
              if((Ac=A2,A1=\=Ac),conf_date_min(Iv,D,A,C),
              if((Ac=A1,Ac=A2),
              conf_date_3(Iv,D,A,C),unload(Iv))))).

conf_date_max(Iv,D,A,C):-arg(2,D,Me1),arg(3,D,G1),arg(2,C,Me),arg(3,C,G),
                       R1 is (Me1*43790+G1*1440),
                       R is (Me*43790+G*1440),
                       if(R>R1,check_hours(Iv,D,A,C),unload(Iv)).

conf_date_min(Iv,D,A,C):-arg(2,A,Me1),arg(3,A,G1),arg(2,C,Me),arg(3,C,G),
                       R2 is (Me1*43790+G1*1440),
                       R is (Me*43790+G*1440),
                       if(R=<R2,check_hours(Iv,D,A,C),unload(Iv)).

conf_date_3(Iv,D,A,C):-
                arg(2,D,Me1),arg(3,D,G1),arg(2,C,Me),arg(3,C,G),
                                   arg(2,A,Me2),arg(3,A,G2),
                                   R1 is (Me1*43790+G1*1440),
                                   R2 is (Me2*43790+G2*1440),
                                   R is (Me*43790+G*1440),
                                   if((R>=R1,R=<R2),check_hours(Iv,D,A,C),unload(Iv)).

check_hours(Iv,D,A,C):-arg(4,D,H1),arg(5,D,M1),arg(6,D,S1),arg(4,A,H2),arg(5,A,M2),arg(6,A,S2),arg(4,C,H),arg(5,C,M),arg(6,C,S),
                        O1 is (H1*3600+M1*60+S1),
                        O2 is (H2*3600+M2*60+S2),
                        O is (H*3600+M*60+S),
                        if(O2>O1,normal_hour(Iv,O,O1,O2),print('To insert a correct temporal interval')).

normal_hour(Iv,O,O1,O2):-if((O>O1,O<O2),load(Iv),unload(Iv)).

load(Me):-if(clause(prov_int1(Me),_),true,assert(prov_int1(Me))).

unload(Me):-if(clause(prov_int1(Me),_),retractall(prov_int1(Me)),true).

block_number_internal_event:-if(clause(prov_int1(_),_),block_number1,true).
block_number1:-findall(M,clause(prov_int1(M),_),L),
                if(L=[],true,block_number2(L)).

block_number2(L):-last(L,U),
        repeat,
        member(Me,L),
        clause(internal_event(Me,_,N,_,_),_),
        if(N=forever,assert(prov_int2(Me)),check_time_rep(Me,N)),
        Me==U,!,retractall(prov_int1(_)).

check_time_rep(Me,N):-functor(Me,F,_),
                      if(cond_int_repeat(F,N),check_cond_repeat(Me,N),
                      assert_prov_int2(Me)).

cond_int_repeat(F,N):-clause(internal_repeat(F,P),_),P>=N.
assert_prov_int2(Me):-assert_this(prov_int2(Me)).

check_cond_repeat(Me,N):-clause(internal_event(Me,_,N,Cn,_),_),
                         if(Cn=true,true,check_cond_repeat1(Me,Cn)).

check_cond_repeat1(Me,Cn):-functor(Cn,F,_),
                             if(F=change,check_cond_change(Me,Cn),true).

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

block_frequency:-if(clause(prov_int2(_),_),block_frequency1,true).
block_frequency1:-findall(M,clause(prov_int2(M),_),L),
        if(L=[],true,block_frequency2(L)).

block_frequency2(L):-last(L,U),
        repeat,
        member(Me,L),
        if(clause(reacted(Me,_),_),true,assert_tempI(Me)),
         Me==U,!,retractall(prov_int2(_)).

assert_tempI(Me):-if(clause(tempI(Me),_),true,assert(tempI(Me))).

internal_event2:-if(clause(tempI(_),_),internal_eventp,true).

internal_eventp:-findall(Y,tempI(Y),Ls),
         statistics(walltime,[Tc,_]),
         last(Ls,La),
         repeat,
           member(Me,Ls),
       if(clause(tried_event(Me,_),_),true,internal_event_p_no_tent(Me,Tc)),
         Me==La,!.

internal_event_p_no_tent(Me,Tc):-if(once(Me),assert_iv(Me,Tc),retractall(tempI(Me))).

assert_iv(Me,Tc):-assert(tried_event(Me,Tc)),
         if(clause(iv(Me,Tc),_),true,assert(iv(Me,Tc))),
         retractall(tempI(Me)),
         if(clause(activate_goal(Me),_),
         cancel_goal(Me),true),
         findall(X,clause(linked_en(Me,X),_),L),
         if(L=[],true,(internal_pres(Me,L),ass_ev_pre_i(Me,Tc))).

internal_pres(Me,L):-last(L,Lu),
   member(X,L),
          clause(en(X,T1),true),
           assert(link_int_time(Me,T1)),
          retractall(en(X,T1)),
   X==Lu,!.

ass_ev_pre_i(Me,Tc):-setof(T,clause(link_int_time(Me,T),_),L1),
   retractall(link_int_time(Me,_)),
   if(clause(ev_pre_time(Me,_,_),_),
         (retractall(ev_pre_time(Me,_,_)),assert(ev_pre_time(Me,Tc,L1))),
        assert(ev_pre_time(Me,Tc,L1))).

check_freq_tent:-clause(agent_x(_,_,S,_),_),statistics(walltime,[T,_]),
                  read_line(S,2),clause(evintI(L),_),
        if(L\=[],check_freq_tent0(L,T),true).

check_freq_tent0(Ls,T):-last(Ls,U),
           repeat,
           member(X,Ls),
           if(clause(internal_event(X,Tp,_,_,_),_),
           check_freq_tent1(X,T,Tp),true),
                X==U,!.

check_freq_tent1(X,T,Tp):-if(clause(tried_event(X,Tc),_),check_freq_tent2(X,T,Tp,Tc),true).

check_freq_tent2(X,T,Tp,Tc):-Tp1 is Tp*1000,
                       ST is Tp1+Tc,
                       if(T>ST,cancel_tried(X,Tc),true).

cancel_tried(X,Tc):-retractall(tried_event(X,Tc)).

check_freq_iv:-clause(agent_x(_,_,S,_),_),statistics(walltime,[T,_]),
                  read_line(S,2),clause(evintI(L),_),
        if(L\=[],check_freq_iv0(L,T),true).

check_freq_iv0(Ls,T):-last(Ls,U),
           repeat,
           member(X,Ls),
           if(clause(internal_event(X,Tp,_,_,_),_),
           check_freq_iv1(X,T,Tp),true),
                X==U,!.

check_freq_iv1(X,T,Tp):-if(clause(reacted(X,Tc),_),check_freq_int2(X,T,Tp,Tc),true).

check_freq_int2(X,T,Tp,Tc):-Tp1 is Tp*1000,
                       ST is Tp1+Tc,
                       if(T>ST,cancel_reacted(X,Tc),true).

cancel_reacted(X,Tc):-retractall(reacted(X,Tc)).

process_external_events:-clause(agent_x(_,_,S,_),_),read_line(S,1),clause(eventE(Es),_),
if(Es=[],true,(process_high_events,process_normal_events)).

process_high_events:-if(clause(ev_high(_,_,_),_),process_high_events1,true).
process_high_events1:-findall(ev_high(AgM,E,T),clause(ev_high(AgM,E,T),_),L),
last(L,ev_high(Ag,E,T)),
if(once(eve_cond(E)),process_high_event(Ag,E,T),no_process_high_event(Ag,E,T)),
Ag==Ag,E==E,T==T,!.

no_process_high_event(AgM,E,T):-write('External event preconditions not verified'),nl,retractall(ev_high(_,E,T)),assert_this(refused_external(AgM,E,T)).
process_high_event(AgM,E,T):-retractall(ev_high(AgM,E,T)),assert_this(executed_external(AgM,E,T)),
clause(evN(Es),_),if(Es=[],true,if(member(E,Es),process_event3(E,T),true)).

process_normal_events:-if(clause(ev_normal(_,_,_),_),process_normal_events1,true).
process_normal_events1:-clause(ev_normal(AgM,E,T),_),
if(once(eve_cond(E)),process_normal_event(AgM,E,T),no_process_normal_event(AgM,E,T)).

no_process_normal_event(AgM,E,T):-write('External event preconditions not verified'),nl,retractall(ev_normal(_,E,T)),
assert_this(refused_external(AgM,E,T)).

no_process_normal_event_no_time(AgM,E,T):-write('External event preconditions not verified: no DeltaTime'),nl,retractall(ev_normal(_,E,T)),
assert_this(refused_external(AgM,E,T)).

process_normal_event(AgM,E,T):-retractall(ev_normal(AgM,E,T)),simultaneity_interval(E), process_events.
clause(evN(Es),_),if(Es=[],true,if(member(E,Es),process_event3(E,T),true)).

process_event3(E,T):-findall(I,clause(linked_en(I,E),_),Ls),
if(clause(ev_pre_time(X,T),_),process_event4(E,T,Ls),process_event5(E,T,Ls)).

process_event4(E,T,Ls):-if(Ls=[],true,process_event6(E,T,Ls)).
process_event5(E,T,Ls):-if(Ls=[],true,process_event7(E,T,Ls)).

process_event6(E,T,Ls):-last(Ls,U),
repeat,
member(I,Ls),
if(clause(ev_pre_time(I,T),_),true,assert(ev_pre_time(I,T,[]))),
I==U,!.

process_event7(E,T,Ls):-last(Ls,U),
repeat,
member(I,Ls),
assert(ev_pre_time(I,T,[])),
I==U,!.

manage_goals:-if(clause(goal(_),_),manage_goals1,true).

manage_goals1:-findall(goal(G),clause(goal(G),_),L),
         last(L,U),
         repeat,
           member(Me,L),
             if(clause(goal_completed(Me),_),true,manage_goals2(Me)),
         Me==U,!.

manage_goals2(Me):-if(clause(goal_precondition(Me,Pre),_),manage_goals3(Me,Pre),manage_goals4(Me)).

manage_goals3(Me,Pre):-if(Pre,manage_goals4(Me),true).

manage_goals4(Me):-if(clause(goal_do(Me,Do),_),manage_goals5(Me,Do),true).

manage_goals5(Me,Do):-if(Do,manage_goals6(Me),true).

manage_goals6(Me):-if(clause(goal_postcondition(Me,Post),_),manage_goals7(Me,Post),manage_goals8(Me)).

manage_goals7(Me,Post):-if(Post,manage_goals8(Me),true).

manage_goals8(Me):-assert_this(goal_completed(Me)).

cancel_goal(Me):-retractall(goal(Me)),retractall(goal_precondition(Me,_)),
                 retractall(goal_do(Me,_)),retractall(goal_postcondition(Me,_)),
                 retractall(goal_completed(Me)).

manage_time:-if(clause(time(_),_),manage_time1,true).

manage_time1:-findall(time(T),clause(time(T),_),L),
         last(L,U),
         repeat,
           member(Me,L),
             if(clause(time_completed(Me),_),true,manage_time2(Me)),
         Me==U,!.

manage_time2(Me):-if(clause(time_precondition(Me,Pre),_),manage_time3(Me,Pre),manage_time4(Me)).

manage_time3(Me,Pre):-if(Pre,manage_time4(Me),true).

manage_time4(Me):-if(clause(time_do(Me,Do),_),manage_time5(Me,Do),true).

manage_time5(Me,Do):-if(Do,manage_time6(Me),true).

manage_time6(Me):-if(clause(time_postcondition(Me,Post),_),manage_time7(Me,Post),manage_time8(Me)).

manage_time7(Me,Post):-if(Post,manage_time8(Me),true).

manage_time8(Me):-assert_this(time_completed(Me)).

cancel_time(Me):-retractall(time(Me)),retractall(time_precondition(Me,_)),
                 retractall(time_do(Me,_)),retractall(time_postcondition(Me,_)),
                 retractall(time_completed(Me)).

check_conditions:-if(clause(condition(_),_),check_conditions1,true).

check_conditions1:-findall(condition(C),clause(condition(C),_),L),
         last(L,U),
         repeat,
           member(Me,L),
             if(clause(condition_completed(Me),_),true,check_conditions2(Me)),
         Me==U,!.

check_conditions2(Me):-if(clause(condition_precondition(Me,Pre),_),check_conditions3(Me,Pre),check_conditions4(Me)).

check_conditions3(Me,Pre):-if(Pre,check_conditions4(Me),true).

check_conditions4(Me):-if(clause(condition_do(Me,Do),_),check_conditions5(Me,Do),true).

check_conditions5(Me,Do):-if(Do,check_conditions6(Me),true).

check_conditions6(Me):-if(clause(condition_postcondition(Me,Post),_),check_conditions7(Me,Post),check_conditions8(Me)).

check_conditions7(Me,Post):-if(Post,check_conditions8(Me),true).

check_conditions8(Me):-assert_this(condition_completed(Me)).

cancel_condition(Me):-retractall(condition(Me)),retractall(condition_precondition(Me,_)),
                     retractall(condition_do(Me,_)),retractall(condition_postcondition(Me,_)),
                     retractall(condition_completed(Me)).
