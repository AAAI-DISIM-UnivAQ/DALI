% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

decompose(H,B):-decompose1(H,B),!,write_del_items(H).

decompose1(H,B):-go_var(H),
               result_format(H1),write_head(H1),decompose_terms1(H1),
                go_var(B),
               result_format(B1),decompose_terms1(B1).


write_del_items(H):-decomp_head_del(H,S),!,write_end(S).

decompose_terms1([]).
decompose_terms1([S1|Resto]):-decompose_terms1(S1),!,decompose_terms1(Resto).
decompose_terms1((X,Y)):-expand_term((X,Y),Z),decompose_terms1(Z).
decompose_terms1((X;Y)):-expand_term((X;Y),Z),decompose_terms1(Z).
decompose_terms1(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms1(Z).
decompose_terms1(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms1(Z).
decompose_terms1(X):-if(X=[],true,discriminate_dec_terms1(X)).

discriminate_dec_terms1(X):-X=..L,arg(1,L,Lc),
if(Lc={},continue_keep(X),keep_dec(X)).

continue_keep(X):-arg(1,X,N),decompose_terms1(N).



write_head(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,evp_con(H)),write(Stream,':-'),close(Stream).
                  

keep_dec(X):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,X),write(Stream,','),close(Stream).

write_end(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,del_evp_items(H)),write(Stream,'.'),nl(Stream),close(Stream).



del_evp_items(L):-length(L,N),
                             repeat,
                             member(M,L),arg(1,M,Ep),arg(2,M,T),
                             if(clause(past(Ep,T,Ty1),_),del_evp_items1(Ep,T,Ty1,L,M),true),
                             nth1(K,L,M),
                              K==N,!.

del_evp_items1(Ep,T,Ty1):-retractall(past(Ep,T,_)),
                              write(happened_past_deletion(Ep)),nl,
                              retractall(past(Ep)),
                              assert(remember(Ep,T,Ty1)).





read_cevp_file:-if(file_exists('_tempfile0000.txt'),read_cevp_file_con,true).
read_cevp_file_con:-open('_tempfile0000.txt',read,Stream,[]),
               repeat,
               read(Stream,T),
               if(T=end_of_file,true,assert(change_evp_con(T))),
               
                                 
               T==end_of_file,!,
               take_all_evp_con,
               close(Stream),if(file_exists('_tempfile0000.txt'),
                delete_file('_tempfile0000.txt'),true).

 take_all_evp_con:-findall(C,clause(change_evp_con(C),_),L),
                   go_rvar(L),prendi_value(S),manage_evp_con(S).

manage_evp_con(S):-last(S,U),
                     repeat,
                       member(M,S),assert(M),
                      M==U,!.
                   
read_cevp_file1:-if(file_exists('_tempfile0000.txt'),read_cevp_file_con1,true).
read_cevp_file_con1:-open('_tempfile0000.txt',read,Stream,[]),
               
               read(Stream,T), 
               read_cevp_file1(T),
               close(Stream),if(file_exists('_tempfile0000.txt'),
               delete_file('_tempfile0000.txt'),true).


read_cevp_file1(T):-T=..Ld,go_rvar(Ld).

 decomp_head_del(H,Lfh):-decompose_terms2(H),findall(C,clause(eliminating_tok(C),_),Lfh),retractall(eliminating_tok(_)).
                             
decompose_terms2([]).
decompose_terms2([S1|Resto]):-decompose_terms2(S1),!,decompose_terms2(Resto).
decompose_terms2((X,Y)):-expand_term((X,Y),Z),decompose_terms2(Z).
decompose_terms2((X;Y)):-expand_term((X;Y),Z),decompose_terms2(Z).
decompose_terms2(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms2(Z).
decompose_terms2(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms2(Z).
decompose_terms2(X):-if(X=[],true,eliminate_evp_constr(X)).

eliminate_evp_constr(X):-assert(eliminating_tok(X)).

%CHIAMA LE CLAUSOLE EXPORT_PAST

manage_export_past:-if(manage_export_past0,true,true).
manage_export_past0:-setof(X,export_past(X),_).


%CHIAMA LE CLAUSOLE EXPORT_PAST_NOT_DO

manage_export_past_not_do:-if(manage_export_past_not_do0,true,true).
manage_export_past_not_do0:-setof(X,export_past_not_do(X),_).

%CHIAMA LE CLAUSOLE EXPORT_PAST_DO

manage_export_past_do:-if(manage_export_past_do0,true,true).
manage_export_past_do0:-setof(X,export_past_do(X),_).



%GESTIONE ASSERZIONE EVP CONSTRAINTS
pippo(Ld,Kl1):-go_rvar(Ld),prendi_value(Kl1).

%GESTIONE EVP_CON

check_constr_all:-read_cevp_file.

evaluate_evp_constr:-findall(evp_con(X),clause(evp_con(X),_),L),
                     if(L=[],true,evaluate_evp_constr1(L)).

evaluate_evp_constr1(L):-length(L,N),
                             repeat,
                             member(M,L),if(M,true,true),
                               nth1(K,L,M),
                             K==N,!.

%GESTIONE PRIMITIVE ALLR,FIRSTR,LASTR,INR

allr(rem(P,L,N)):-findall(rem(P,T,S),clause(remember(P,T,S),_),L),length(L,N).

lastr(rem(P,E)):-findall(rem(P,T,S),clause(remember(P,T,S),_),L),last(L,E).

firstr(rem(P,M)):-findall(rem(P,T,S),clause(remember(P,T,S),_),L),member(M,L).

inr(rem(P,T1,T2,L)):-findall(rem(P,T,S),clause(remember(P,T,S),_),L0),
                    last(L0,U),
                       repeat,
                          member(M,L0),
                              arg(2,M,Tm),
                              if(cond_rem_t(Tm,T1,T2),assert(ok_rem(M)),true),
                          M==U,!,
                             take_all_ok_rem(L).


cond_rem_t(T,T1,T2):-T>=T1,T=<T2.
take_all_ok_rem(L):-findall(X,clause(ok_rem(X),_),L),retractall(ok_rem(_)).


% GESTIONE CONSTRAINTS PER CONTROLLO BEHAVIOR

decompose_not_do(H,B):-decompose1_not_do(H,B),!,write_items_not_do(H).

decompose1_not_do(H,B):-go_var(H),
               result_format(H1),write_head_not_do(H1),decompose_terms1_not_do(H1),
                go_var(B),
               result_format(B1),decompose_terms1_not_do(B1).


write_items_not_do(H):-decomp_head_not_do(H,S),!,write_end_not_do(S).

decompose_terms1_not_do([]).
decompose_terms1_not_do([S1|Resto]):-decompose_terms1_not_do(S1),!,decompose_terms1_not_do(Resto).
decompose_terms1_not_do((X,Y)):-expand_term((X,Y),Z),decompose_terms1_not_do(Z).
decompose_terms1_not_do((X;Y)):-expand_term((X;Y),Z),decompose_terms1_not_do(Z).
decompose_terms1_not_do(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms1_not_do(Z).
decompose_terms1_not_do(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms1_not_do(Z).
decompose_terms1_not_do(X):-if(X=[],true,discriminate_dec_terms1_not_do(X)).

discriminate_dec_terms1_not_do(X):-X=..L,arg(1,L,Lc),
if(Lc={},continue_keep_not_do(X),keep_dec_not_do(X)).

continue_keep_not_do(X):-arg(1,X,N),decompose_terms1_not_do(N).



write_head_not_do(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,evp_not_do(H)),write(Stream,':-'),close(Stream).
                  

keep_dec_not_do(X):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,X),write(Stream,','),close(Stream).

write_end_not_do(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,check_evp_items(H)),write(Stream,'.'),nl(Stream),close(Stream).



check_evp_items(L):-length(L,N),
                             repeat,
                             member(M,L),arg(1,M,Ep),arg(2,M,T),
                             clause(past(Ep,T,Ty1),_),
                             if(clause(violated_not_do(Ep,T,Ty1),_),true,
                             happened_violation_not_do(Ep,T,Ty1)),
                               nth1(K,L,M),
                             K==N,!.

happened_violation_not_do(Ep,T,Ty1):-assert(violated_not_do(Ep,T,Ty1)),write(not_do_violation(Ep)),nl.
decomp_head_not_do(H,Lfh):-decompose_terms2_not_do(H),findall(C,clause(eliminating_tok(C),_),Lfh),retractall(eliminating_tok(_)).
                             
decompose_terms2_not_do([]).
decompose_terms2_not_do([S1|Resto]):-decompose_terms2_not_do(S1),!,decompose_terms2_not_do(Resto).
decompose_terms2_not_do((X,Y)):-expand_term((X,Y),Z),decompose_terms2_not_do(Z).
decompose_terms2_not_do((X;Y)):-expand_term((X;Y),Z),decompose_terms2_not_do(Z).
decompose_terms2_not_do(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms2_not_do(Z).
decompose_terms2_not_do(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms2_not_do(Z).

decompose_terms2_not_do(X):-if(X=[],true,eliminate_evp_constr_not_do(X)).

eliminate_evp_constr_not_do(X):-assert(eliminating_tok(X)).




%GESTIONE EVP_NOT_DO

evaluate_evp_not_do:-findall(evp_not_do(X),clause(evp_not_do(X),_),L),
                     if(L=[],true,evaluate_evp_constr1_not_do(L)).

evaluate_evp_constr1_not_do(L):-length(L,N),
                             repeat,
                             member(M,L),if(M,true,true),
                               nth1(K,L,M),
                             K==N,!.


% GESTIONE CONSTRAINTS PER VERIFICA BEHAVIOR

decompose_if_it_is(H,B):-decompose1_if_it_is(H,B),!,write_items_if_it_is(H).

decompose1_if_it_is(H,B):-go_var(H),
               result_format(H1),write_head_if_it_is(H1),
                go_var(B),
               result_format(B1),decompose_terms1_if_it_is(B1).


write_items_if_it_is(H):-decomp_head_if_it_is(H,S),!,write_end_if_it_is(S).

decompose_terms1_if_it_is([]).
decompose_terms1_if_it_is([S1|Resto]):-decompose_terms1_if_it_is(S1),!,decompose_terms1_if_it_is(Resto).
decompose_terms1_if_it_is((X,Y)):-expand_term((X,Y),Z),decompose_terms1_if_it_is(Z).
decompose_terms1_if_it_is((X;Y)):-expand_term((X;Y),Z),decompose_terms1_if_it_is(Z).
decompose_terms1_if_it_is(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms1_if_it_is(Z).
decompose_terms1_if_it_is(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms1_if_it_is(Z).
decompose_terms1_if_it_is(X):-if(X=[],true,discriminate_dec_terms1_if_it_is(X)).

discriminate_dec_terms1_if_it_is(X):-X=..L,arg(1,L,Lc),
if(Lc={},continue_keep_if_it_is(X),keep_dec_if_it_is(X)).

continue_keep_if_it_is(X):-arg(1,X,N),decompose_terms1_if_it_is(N).



write_head_if_it_is(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,evp_do(H)),write(Stream,':-'),close(Stream).
                  

keep_dec_if_it_is(X):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,X),write(Stream,','),close(Stream).

write_end_if_it_is(H):-open('_tempfile0000.txt',append,Stream,[]),
               write(Stream,check_evp_items_if_it_is(H)),write(Stream,'.'),nl(Stream),close(Stream).



check_evp_items_if_it_is(L):-nth1(1,L,Ct),arg(1,Ct,Ep),arg(2,Ct,At),arg(1,At,Atc),
                            check_time(Ep,Atc).


check_time(Ep,T):-datime(C),arg(1,C,Y),arg(2,C,M),arg(3,C,D),arg(4,C,H),arg(5,C,Mi),
arg(6,C,S),nth1(1,T,Yt),nth1(2,T,Mt),nth1(3,T,Dt),nth1(4,T,Ht),
nth1(5,T,Mit),nth1(6,T,St),
if(Y<Yt,true,check_other_par_time(Y,M,D,H,Mi,S,Yt,Mt,Dt,Ht,Mit,St,Ep)).

check_other_par_time(Y,M,D,H,Mi,S,Yt,Mt,Dt,Ht,Mit,St,Ep):- 
if(Y>Yt,check_evp_presence(Ep),check_other_par_time1(M,D,H,Mi,S,Mt,Dt,Ht,Mit,St,Ep)).

check_other_par_time1(M,D,H,Mi,S,Mt,Dt,Ht,Mit,St,Ep):-if(M<Mt,true,check_other_par_time2(M,D,H,Mi,S,Mt,Dt,Ht,Mit,St,Ep)).

check_other_par_time2(M,D,H,Mi,S,Mt,Dt,Ht,Mit,St,Ep):-if(M>Mt,check_evp_presence(Ep),check_other_par_time3(D,H,Mi,S,Dt,Ht,Mit,St,Ep)).

check_other_par_time3(D,H,Mi,S,Dt,Ht,Mit,St,Ep):-if(D<Dt,true,check_other_par_time4(D,H,Mi,S,Dt,Ht,Mit,St,Ep)).

check_other_par_time4(D,H,Mi,S,Dt,Ht,Mit,St,Ep):-if(D>Dt,check_evp_presence(Ep),check_other_par_time5(H,Mi,S,Ht,Mit,St,Ep)).

check_other_par_time5(H,Mi,S,Ht,Mit,St,Ep):-Sec_curr is H*3600+Mi*60+S,Sec_currt is Ht*3600+Mit*60+St,
if(Sec_curr<Sec_currt,true,check_other_par_time6(Sec_curr,Sec_currt,Ep)).

check_other_par_time6(Sec_curr,Sec_currt,Ep):-if(Sec_curr>=Sec_currt,check_evp_presence(Ep),true).

check_evp_presence(Ep):-if(clause(past(Ep,T,P),_),true,ass_violation_if_it_is(Ep,T,P)).


ass_violation_if_it_is(Ep,T,P):-if(clause(violated_do(Ep,T,P),_),true,
                             happened_violation_do(Ep,T,P)).


happened_violation_do(Ep,T,Ty1):-assert(violated_do(Ep,T,Ty1)),write(do_violation(Ep)),nl.
decomp_head_if_it_is(H,Lfh):-decompose_terms2_if_it_is(H),findall(C,clause(eliminating_tok(C),_),Lfh),retractall(eliminating_tok(_)).
                             
decompose_terms2_if_it_is([]).
decompose_terms2_if_it_is([S1|Resto]):-decompose_terms2_if_it_is(S1),!,decompose_terms2_if_it_is(Resto).
decompose_terms2_if_it_is((X,Y)):-expand_term((X,Y),Z),decompose_terms2_if_it_is(Z).
decompose_terms2_if_it_is((X;Y)):-expand_term((X;Y),Z),decompose_terms2_if_it_is(Z).
decompose_terms2_if_it_is(evp(X:Y)):-expand_term((X:Y),Z),decompose_terms2_if_it_is(Z).
decompose_terms2_if_it_is(rem(X:Y)):-expand_term((X:Y),Z),decompose_terms2_if_it_is(Z).

decompose_terms2_if_it_is(X):-if(X=[],true,eliminate_evp_constr_if_it_is(X)).

eliminate_evp_constr_if_it_is(X):-assert(eliminating_tok(X)).


%GESTIONE EVP_DO

evaluate_evp_do:-findall(evp_do(X),clause(evp_do(X),_),L),
                     if(L=[],true,evaluate_evp_constr1_do(L)).

evaluate_evp_constr1_do(L):-length(L,N),
                             repeat,
                             member(M,L),if(M,true,true),
                               nth1(K,L,M),
                             K==N,!.



