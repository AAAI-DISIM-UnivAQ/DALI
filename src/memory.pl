:- module(memory, []).

% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- multifile user:term_expansion/6.
user:term_expansion((X,Y),[],[], ([X,Y]),[],[]).
user:term_expansion([X,Y],[],[], ([(X,Y)]),[],[]).
:-use_module(library(lists)).

open_file_res(F):-see(F), 
	     repeat,
		read(T),expand_term(T,Te),
                            if(T=end_of_file,true,
                            assert(rule_base_res(Te))),
                		T == end_of_file,
	     !,
	     seen,take_res(F).


take_res(F):-findall(T,clause(rule_base_res(T),_),L),
          last(L,U),
         repeat,
          member(M,L),
              
               split_res(M,F),
              
           M==U,!,retractall(rule_base_res(_)).


split_res(C,F):-arg(1,C,Head),C=..L,if(clause(goal(_),_),retractall(goal(_)),true),
           if(member(':-',L),is_clause_res(L,Head),true),merge_head(Head,F).


is_clause_res(L,Head):-append([':-'],L1,L),execute_res(L1,Head).
execute_res([],_).
execute_res([S1|Resto],Head):-execute_res(S1,Head),!,execute_res(Resto,Head).
execute_res((X,Y),Head):-expand_term((X,Y),Z),execute_res(Z,Head).
execute_res(X,Head):-if(X=[],true,try_split_res(X,Head)).
try_split_res(M,Head):-functor(M,F,_),if(F=obg,write_obg_text(M),true),if(clause(goal(_),_),
                      write_residue(Head,M),true).

write_obg_text(M):-assert(goal(M)).
write_residue(Head,M):-assert(residue_g(Head,M)).
merge_head(Head,F):-findall(M,clause(residue_g(Head,M),_),L),if(L=[],true,merge_head1(Head,L,F)).

merge_head1(Head,L,F):-arg(1,L,A1),arg(1,A1,A2),nth1(1,L,A1,R),
                      open(F,append,Stream,[]),nl(Stream),
                      write(Stream,'call_residue_goal('),
                      write(Stream,A2),write(Stream,'):-clause(past('),
                      write(Stream,A2),write(Stream,',_),_),call_head(gl('),
                      write(Stream,Head),write(Stream,'))'),write(Stream,'.'),nl(Stream),
                      close(Stream),
                      retractall(residue_g(Head,_)),create_head_g(Head,R,F).

create_head_g(Head,Rg,F):-if(Rg=[],write_base_true(Head,F),create_head_g0(Head,Rg,F)).

create_head_g0(Head,Rg,F):-reverse(Rg,R),arg(1,R,A1),
                nth1(1,R,A1,R1),if(R1=[],(assert(base(R)),write_base(Head,F)),create_head_g1(Head,R1,F,A1)).
create_head_g1(Head,R1,F,A1):-arg(1,R1,A2),nth1(1,R1,A2,R2),
                nth1(1,L1,A1,[]),nth1(1,L2,A2,L1),
                expand_term(L2,S1),assert(base(S1)),
                length(R2,Lu),
                nth1(Lu,R2,Ul),
                repeat,
                   member(Me,R2),
                   clause(base(K),_),
                   nth1(1,Ri,Me,K),
                   expand_term(Ri,Sri),
                   retractall(base(K)),
                   assert(base(Sri)),
                Me==Ul,!,write_base(Head,F).
write_base_true(Head,F):- open(F,append,Stream,[]),write(Stream,'gl('),write(Stream,Head),
                        write(Stream,'):-true'),write(Stream,'.'),nl(Stream),close(Stream).

write_base(Head,F):-clause(base(B),_),retractall(base(B)),reverse(B,B1),
               append([':-',gl(Head)],B1,Lf),Cf=..Lf,
               open(F,append,Stream,[]),write(Stream,Cf),write(Stream,'.'),nl(Stream),close(Stream).

call_head(Head):-call(Head),arg(1,Head,Te),functor(Te,F,_),
                   if((F=evi;F=eve),call_head1(Te),true).

call_head1(Te):-arg(1,Te,E),statistics(walltime,[T,_]),divP(E,T).



 

