% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

gest_learn(var_H):-clause(past(learn(var_H),var_T,var_U),_),learn_if(var_H,var_T,var_U).
evi(gest_learn(var_H)):-retractall(past(learn(var_H),_,_)),
                        clause(agente(_,_,_,var_S),_),
                        name(var_S,var_N),
                        append(var_L,[46,112,108],var_N),
                        name(var_F,var_L),
                        manage_lg(var_H,var_F),a(learned(var_H)).



cllearn:- clause(agente(_,_,_,var_S),_),
                           name(var_S,var_N),append(var_L,[46,112,108],var_N),
                           append(var_L,[46,116,120,116],var_To),
                           name(var_FI,var_To),
                           open(var_FI,read,Stream,[]),
                           repeat,
                           read(Stream,var_T),
                           arg(1,var_T,var_H),write(var_H),nl,
                           var_T==end_of_file,!,
                           close(Stream).



send_msg_learn(var_T,var_A,var_Ag):-a(message(var_Ag,confirm(learn(var_T),var_A))).


      