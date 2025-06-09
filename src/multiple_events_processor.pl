% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

carica_file(F):-see(F), 
	     repeat,
		read(T),
                            if(T=end_of_file,true,
                            assert(program_rule(T))),
                            
                		T == end_of_file,
	     !,
	     seen,if(file_exists(F),delete_file(F),true),take_rules(F),take_rule_mult(F).
take_rules(F):-if(clause(program_rule(_),_),take_rules1(F),true).
take_rules1(F):-findall(X,clause(program_rule(X),_),L),
             last(L,U),
               repeat,
                 member(Me,L),
                      examine_program_rule(Me,F),
                      
                      
             Me==U,!,
             retractall(program_rule(_)).

examine_program_rule(Me,F):-if(arg(1,Me,Head),examine_program_head(Head,Me,F),true).

examine_program_head(Head,Me,F):-functor(Head,Fu,N),
                               if((N>1,not_tell_told(Fu)),testa_multiplo_evento(Head,Me,F),scrivi_clause_no_mul(Me,F)).

testa_multiplo_evento(Head,Me,F):-arg(2,Head,A2),functor(A2,Fu,_),if(Fu=eve,testa_multiplo_evento2(Head,Me),
                                scrivi_clause_no_mul(Me,F)).
testa_multiplo_evento2(Head,Me):-ejec_mul(Head,Me).
not_tell_told(Fu):-Fu\=tell,Fu\=told.
ejec_mul([],_).
ejec_mul([S1|Resto],Me):-ejec_mul(S1,Me),!,ejec_mul(Resto,Me).
ejec_mul((X,Y),Me):-expand_term((X,Y),Z),ejec_mul(Z,Me).
ejec_mul(X,Me):-functor(X,F,N),if((X=[];N\=1),true,no_eve_mult(X,Me,F)).
no_eve_mult(X,Me,F):-if(F\=eve,true,assert(multip(X,Me))).
take_rule_mult(F):-if(clause(multip(_,_),_),take_rule_mult1(F),true).
take_rule_mult1(F):-findall(B,clause(multip(_,B),_),Lb),
                remove_dups(Lb,Lb1),
                last(Lb1,U),
                   repeat,
                   member(Mb,Lb1),
                   findall(X,clause(multip(eve(X),Mb),_),L),
                   append([eve],L,Lb2),
                   T=..Lb2,
                   arg(2,Mb,Body),
                   open(F,append,Stream,[]),nl(Stream),
                   write(Stream,T),write(Stream,':-'),write(Stream,Body),write(Stream,'.'),
                   nl(Stream), close(Stream),
                   retractall(multip(_,Mb)),
                 Mb==U,!.
scrivi_clause_no_mul(Me,F):-open(F,append,Stream,[]),nl(Stream),
                   write(Stream,Me),write(Stream,'.'),
                   nl(Stream), close(Stream).
