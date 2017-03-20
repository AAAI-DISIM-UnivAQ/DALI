
:-dynamic ill/2.

:-dynamic trust/3.

trust(gen_doc,0.8,gen_doctor).

trust(friend_nurse,0.4,friend).

trust(friend_clerk,0.5,friend).

trust(lung_doc1,0.4,doctor).

trust(lung_doc2,0.4,doctor).

ill(cold,10).

ill(bone_pain,10).

ill(high_temperature,10).

:-dynamic maxtrust/2.

:-dynamic go_lung_doc/1.

:-dynamic ability/2.

:-dynamic controllo/1.

:-dynamic opinion/3.

:-dynamic guarito/1.

controllo(1).

modifiche_salute:-controllo(1).

evi(modifiche_salute):-random(var_C),random(var_Rc),random(var_Rb),random(var_Rh),if(var_C>0.6,(var_NRc is var_Rc/3,var_NRb is var_Rb/3,var_NRh is var_Rh/3),(var_NRc is-(var_Rc)/3,var_NRb is-(var_Rb)/3,var_NRh is-(var_Rh)/3)),clause(ill(cold,var_X1),var__),clause(ill(bone_pain,var_X2),var__),clause(ill(high_temperature,var_X3),var__),retractall(ill(var__,var__)),var_N1 is var_X1+var_NRc,var_N2 is var_X2+var_NRb,var_N3 is var_X3+var_NRh,assert(ill(cold,var_N1)),assert(ill(bone_pain,var_N2)),assert(ill(high_temperature,var_N3)),write('OLD COLD '),write(var_X1),nl,write('OLD BONE_PAIN '),write(var_X2),nl,write('OLD HIGH_TEMPERATURE '),write(var_X3),nl,write('NEW COLD '),write(var_N1),nl,write('NEW BONE_PAIN '),write(var_N2),nl,write('NEW HIGH_TEMPERATURE '),write(var_N3),nl,nl,assert(controllo(0)),go_reaction(modifiche_salute).

sto_male:-controllo(0),evtp(var_Dali_tt1,modifiche_salute),clause(ill(cold,var_X),var__),var_X>4.

sto_male:-controllo(0),evtp(var_Dali_tt2,modifiche_salute),clause(ill(bone_pain,var_X),var__),var_X>4.

sto_male:-controllo(0),evtp(var_Dali_tt3,modifiche_salute),clause(ill(high_temperature,var_X),var__),var_X>4.

i_am_ill:-sto_male.

evi(i_am_ill):-sleep(20),write('STO MALE, DEVO CONSULTARE UN DOTTORE PER FARMI DIRE COME AGIRE'),a(message(gen_doc,send_message(i_am_ill(patient),patient))),retractall(controllo(var__)),go_reaction(i_am_ill).

eve(gen_doc_propone):-a(prendere_antibiotico).

eve(consult_lung_doc):-write('CONSULTO I MIEI AMICI PER SCEGLIERE DOTTORE POLMONARE'),nl,retractall(opinion(var__,var__,var__)),findall(pp(var_X,var_Tr),trust(var_X,var_Tr,friend),var_L),last(var_L,var_U),arg(1,var_U,var_Ultimo),write('L ultimo è '),write(var_Ultimo),nl,repeat,member(var_Me,var_L),arg(1,var_Me,var_Amico),arg(2,var_Me,var_Fiducia),write('Inviamo richiesta di info a '),write(var_Amico),nl,a(message(var_Amico,send_message(what_about_lung_doc(patient),patient))),var_Amico==var_Ultimo,!,clause(trust(lung_doc1,var_FD1,doctor),var__),clause(trust(lung_doc2,var_FD2,doctor),var__),retractall(maxtrust(var__,var__)),if(var_FD2>var_FD1,(assert(maxtrust(default,lung_doc2)),assert(opinion(default,lung_doc2,-999))),(assert(maxtrust(default,lung_doc1)),assert(opinion(default,lung_doc1,-999)))).

eve(friend_opinion(var_Name,var_LD,var_X)):-assert(opinion(var_Name,var_LD,var_X)),write(var_Name),write(' mi dice di scegliere '),write(var_LD),write(' per il ricovero'),nl.

choose_doctor:-evtp(var_Dali_tt4,consult_lung_doc),after_evp_time(consult_lung_doc,0,0,0,10),control_times(choose_doctor,[var_Dali_tt4]).

evi(choose_doctor):-clause(trust(lung_doc1,var_FD1,doctor),var__),clause(trust(lung_doc2,var_FD2,doctor),var__),findall(yy(var_Name,var_LD,var_X),opinion(var_Name,var_LD,var_X),var_L),last(var_L,var_U),arg(1,var_U,var_Ultimo),repeat,member(var_Me,var_L),arg(1,var_Me,var_Amico),arg(2,var_Me,var_Dottore),arg(3,var_Me,var_Fiducia),if(var_Amico=default,(var_Fiducia is 1,var_Tr is 1),clause(trust(var_Amico,var_Tr,friend),var__)),if(var_Dottore=lung_doc1,var_FT is var_Fiducia*var_FD1*var_Tr,var_FT is var_Fiducia*var_FD2*var_Tr),retractall(opinion(var_Amico,var_Dottore,var__)),assert(opinion(var_Amico,var_Dottore,var_FT)),var_Amico==var_Ultimo,!,findall(yynew(var_Namenew,var_LDnew,var_Xnew),opinion(var_Namenew,var_LDnew,var_Xnew),var_Lnew),last(var_Lnew,var_Unew),arg(1,var_Unew,var_Ultimonew),repeat,member(var_Menew,var_Lnew),arg(1,var_Menew,var_Amiconew),arg(2,var_Menew,var_Dottorenew),arg(3,var_Menew,var_Fiducianew),clause(maxtrust(var_CurrentFriend,var_CurrentDoctor),var__),clause(opinion(var_CurrentFriend,var_CurrentDoctor,var_HisTrust),var__),if(var_Fiducianew>var_HisTrust,(retractall(maxtrust(var__,var__)),assert(maxtrust(var_Amiconew,var_Dottorenew))),true),var_Amiconew==var_Ultimonew,!,clause(maxtrust(var_ChosenFriend,var_ChosenDoctor),var__),a(scegli_dottore(var_ChosenFriend,var_ChosenDoctor)),go_reaction(choose_doctor).

go_to_lung_doctor(var_Lung_doc):-evtp(var_Dali_tt5,scegli_dottore(var_F,var_Lung_doc)),control_times(go_to_lung_doctor(var_Lung_doc),[var_Dali_tt5]).

evi(go_to_lung_doctor(var_Lung_doc)):-clause(ill(cold,var_X1),var__),clause(ill(bone_pain,var_X2),var__),clause(ill(high_temperature,var_X3),var__),random(var_R1),random(var_R2),random(var_R3),var_S1 is var_R1*var_X1,var_S2 is var_R2*var_X2,var_S3 is var_R3*var_X3,write('COLD AFTER LUNG DOCTOR VISIT '),write(var_S1),nl,write('BONE_PAIN AFTER LUNG DOCTOR VISIT '),write(var_S2),nl,write('HIGH_TEMPERATURE AFTER LUNG DOCTOR VISIT '),write(var_S3),nl,retractall(ill(var__,var__)),assert(ill(cold,var_S1)),assert(ill(bone_pain,var_S2)),assert(ill(high_temperature,var_S3)),a(go_doc(var_Lung_doc)),go_reaction(go_to_lung_doctor(var_Lung_doc)).

valutazione_ricovero(var_Lung_doc):-evtp(var_Dali_tt6,go_doc(var_Lung_doc)),after_evp_time(go_doc(var_Lung_doc),0,0,0,10),control_times(valutazione_ricovero(var_Lung_doc),[var_Dali_tt6]).

no_malato(var_HT,var_CC,var_BP):-if(var_HT>4,false,if(var_CC>4,false,if(var_BP>4,false,true))).

evi(valutazione_ricovero(var_Lung_doc)):-clause(ill(high_temperature,var_HT),var__),clause(ill(cold,var_CC),var__),clause(ill(bone_pain,var_BP),var__),clause(maxtrust(var_Friend,var_Lung_doctor),var__),write('IL TEMPO NECESSARIO PER LA VALUTAZIONE E TRASCORSO.'),nl,if(no_malato(var_HT,var_CC,var_BP),(write('SONO GUARITO!'),nl,if(var_Friend=default,true,increment_trust_to(var_Friend,0.2,friend)),increment_trust_to(var_Lung_doctor,0.2,lung_doctor)),(write('NON SONO GUARITO!'),nl,if(var_Friend=default,true,decrement_trust_to(var_Friend,0.2,friend)),decrement_trust_to(var_Lung_doctor,0.2,lung_doctor))),assert(controllo(1)),go_reaction(valutazione_ricovero(var_Lung_doc)).

increment_trust_to(var_Person,var_M,var_Type):-if(trust(var_Person,var_Tr,var_Type),retract(trust(var_Person,var_Tr,var_Type)),var_Tr is 0.4),var_Tn is var_Tr+var_M,assert(trust(var_Person,var_Tn,var_Type)),write('Congratulations to '),write(var_Type),write(' '),write(var_Person),write(', your trust of '),write(var_Tr),write(' will increase to '),write(var_Tn),nl.

decrement_trust_to(var_Person,var_M,var_Type):-if(trust(var_Person,var_Tr,var_Type),retract(trust(var_Person,var_Tr,var_Type)),var_Tr is 0.4),var_Tn is var_Tr-var_M,assert(trust(var_Person,var_Tn,var_Type)),write('Sorry for '),write(var_Type),write(' '),write(var_Person),write(', your trust of '),write(var_Tr),write(' will decrease to '),write(var_Tn),nl.

:-dynamic receive/1.

:-dynamic send/2.

:-dynamic isa/3.

receive(send_message(var_X,var_Ag)):-told(var_Ag,send_message(var_X)),call_send_message(var_X,var_Ag).

receive(propose(var_A,var_C,var_Ag)):-told(var_Ag,propose(var_A,var_C)),call_propose(var_A,var_C,var_Ag).

receive(cfp(var_A,var_C,var_Ag)):-told(var_Ag,cfp(var_A,var_C)),call_cfp(var_A,var_C,var_Ag).

receive(accept_proposal(var_A,var_Mp,var_Ag)):-told(var_Ag,accept_proposal(var_A,var_Mp),var_T),call_accept_proposal(var_A,var_Mp,var_Ag,var_T).

receive(reject_proposal(var_A,var_Mp,var_Ag)):-told(var_Ag,reject_proposal(var_A,var_Mp),var_T),call_reject_proposal(var_A,var_Mp,var_Ag,var_T).

receive(failure(var_A,var_M,var_Ag)):-told(var_Ag,failure(var_A,var_M),var_T),call_failure(var_A,var_M,var_Ag,var_T).

receive(cancel(var_A,var_Ag)):-told(var_Ag,cancel(var_A)),call_cancel(var_A,var_Ag).

receive(execute_proc(var_X,var_Ag)):-told(var_Ag,execute_proc(var_X)),call_execute_proc(var_X,var_Ag).

receive(query_ref(var_X,var_N,var_Ag)):-told(var_Ag,query_ref(var_X,var_N)),call_query_ref(var_X,var_N,var_Ag).

receive(inform(var_X,var_M,var_Ag)):-told(var_Ag,inform(var_X,var_M),var_T),call_inform(var_X,var_Ag,var_M,var_T).

receive(inform(var_X,var_Ag)):-told(var_Ag,inform(var_X),var_T),call_inform(var_X,var_Ag,var_T).

receive(refuse(var_X,var_Ag)):-told(var_Ag,refuse(var_X),var_T),call_refuse(var_X,var_Ag,var_T).

receive(agree(var_X,var_Ag)):-told(var_Ag,agree(var_X)),call_agree(var_X,var_Ag).

receive(confirm(var_X,var_Ag)):-told(var_Ag,confirm(var_X),var_T),call_confirm(var_X,var_Ag,var_T).

receive(disconfirm(var_X,var_Ag)):-told(var_Ag,disconfirm(var_X)),call_disconfirm(var_X,var_Ag).

receive(reply(var_X,var_Ag)):-told(var_Ag,reply(var_X)).

send(var_To,query_ref(var_X,var_N,var_Ag)):-tell(var_To,var_Ag,query_ref(var_X,var_N)),send_m(var_To,query_ref(var_X,var_N,var_Ag)).

send(var_To,send_message(var_X,var_Ag)):-tell(var_To,var_Ag,send_message(var_X)),send_m(var_To,send_message(var_X,var_Ag)).

send(var_To,reject_proposal(var_X,var_L,var_Ag)):-tell(var_To,var_Ag,reject_proposal(var_X,var_L)),send_m(var_To,reject_proposal(var_X,var_L,var_Ag)).

send(var_To,accept_proposal(var_X,var_L,var_Ag)):-tell(var_To,var_Ag,accept_proposal(var_X,var_L)),send_m(var_To,accept_proposal(var_X,var_L,var_Ag)).

send(var_To,confirm(var_X,var_Ag)):-tell(var_To,var_Ag,confirm(var_X)),send_m(var_To,confirm(var_X,var_Ag)).

send(var_To,propose(var_X,var_C,var_Ag)):-tell(var_To,var_Ag,propose(var_X,var_C)),send_m(var_To,propose(var_X,var_C,var_Ag)).

send(var_To,disconfirm(var_X,var_Ag)):-tell(var_To,var_Ag,disconfirm(var_X)),send_m(var_To,disconfirm(var_X,var_Ag)).

send(var_To,inform(var_X,var_M,var_Ag)):-tell(var_To,var_Ag,inform(var_X,var_M)),send_m(var_To,inform(var_X,var_M,var_Ag)).

send(var_To,inform(var_X,var_Ag)):-tell(var_To,var_Ag,inform(var_X)),send_m(var_To,inform(var_X,var_Ag)).

send(var_To,refuse(var_X,var_Ag)):-tell(var_To,var_Ag,refuse(var_X)),send_m(var_To,refuse(var_X,var_Ag)).

send(var_To,failure(var_X,var_M,var_Ag)):-tell(var_To,var_Ag,failure(var_X,var_M)),send_m(var_To,failure(var_X,var_M,var_Ag)).

send(var_To,execute_proc(var_X,var_Ag)):-tell(var_To,var_Ag,execute_proc(var_X)),send_m(var_To,execute_proc(var_X,var_Ag)).

send(var_To,agree(var_X,var_Ag)):-tell(var_To,var_Ag,agree(var_X)),send_m(var_To,agree(var_X,var_Ag)).

call_send_message(var_X,var_Ag):-send_message(var_X,var_Ag).

call_execute_proc(var_X,var_Ag):-execute_proc(var_X,var_Ag).

call_query_ref(var_X,var_N,var_Ag):-clause(agent(var_A),var__),not(var(var_X)),meta_ref(var_X,var_N,var_L,var_Ag),a(message(var_Ag,inform(query_ref(var_X,var_N),values(var_L),var_A))).

call_query_ref(var_X,var__,var_Ag):-clause(agent(var_A),var__),var(var_X),a(message(var_Ag,refuse(query_ref(variable),motivation(refused_variables),var_A))).

call_query_ref(var_X,var_N,var_Ag):-clause(agent(var_A),var__),not(var(var_X)),not(meta_ref(var_X,var_N,var__,var__)),a(message(var_Ag,inform(query_ref(var_X,var_N),motivation(no_values),var_A))).

call_agree(var_X,var_Ag):-clause(agent(var_A),var__),ground(var_X),meta_agree(var_X,var_Ag),a(message(var_Ag,inform(agree(var_X),values(yes),var_A))).

call_confirm(var_X,var_Ag,var_T):-ground(var_X),statistics(walltime,[var_Tp,var__]),datime(_274535),retractall(evtp_date(var_X,_274543)),assert(evtp_date(var_X,_274535)),asse_cosa(past_event(var_X,var_T)),retractall(past(var_X,var_Tp,var_Ag)),assert(past(var_X,var_Tp,var_Ag)).

call_disconfirm(var_X,var_Ag):-ground(var_X),retractall(past(var_X,var__,var_Ag)),retractall(past_event(var_X,var__)).

call_agree(var_X,var_Ag):-clause(agent(var_A),var__),ground(var_X),not(meta_agree(var_X,var__)),a(message(var_Ag,inform(agree(var_X),values(no),var_A))).

call_agree(var_X,var_Ag):-clause(agent(var_A),var__),not(ground(var_X)),a(message(var_Ag,refuse(agree(variable),motivation(refused_variables),var_A))).

call_inform(var_X,var_Ag,var_M,var_T):-asse_cosa(past_event(inform(var_X,var_M,var_Ag),var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(inform(var_X,var_M,var_Ag),var__,var_Ag)),assert(past(inform(var_X,var_M,var_Ag),var_Tp,var_Ag)),datime(_274380),retractall(evtp_date(inform(var_X,var_M,var_Ag),_274388)),assert(evtp_date(inform(var_X,var_M,var_Ag),_274380)).

call_inform(var_X,var_Ag,var_T):-asse_cosa(past_event(inform(var_X,var_Ag),var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(inform(var_X,var_Ag),var__,var_Ag)),assert(past(inform(var_X,var_Ag),var_Tp,var_Ag)),datime(_274298),retractall(evtp_date(inform(var_X,var_Ag),_274306)),assert(evtp_date(inform(var_X,var_Ag),_274298)).

call_refuse(var_X,var_Ag,var_T):-clause(agent(var_A),var__),asse_cosa(past_event(var_X,var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(var_X,var__,var_Ag)),assert(past(var_X,var_Tp,var_Ag)),a(message(var_Ag,reply(received(var_X),var_A))),datime(_274226),retractall(evtp_date(var_X,_274234)),assert(evtp_date(var_X,_274226)).

call_cfp(var_A,var_C,var_Ag):-clause(agent(var_AgI),var__),clause(ext_agent(var_Ag,_274115,var_Ontology,_274117),_274112),asserisci_ontologia(var_Ag,var_Ontology,var_A),once(call_meta_execute_cfp(var_A,var_C,var_Ag,_274134)),a(message(var_Ag,propose(var_A,[_274134],var_AgI))),retractall(ext_agent(var_Ag,_274153,var_Ontology,_274155)).

call_propose(var_A,var_C,var_Ag):-clause(agent(var_AgI),var__),clause(ext_agent(var_Ag,_274052,var_Ontology,_274054),_274049),asserisci_ontologia(var_Ag,var_Ontology,var_A),once(call_meta_execute_propose(var_A,var_C,var_Ag)),a(message(var_Ag,accept_proposal(var_A,[],var_AgI))),retractall(ext_agent(var_Ag,_274087,var_Ontology,_274089)).

call_propose(var_A,var_C,var_Ag):-clause(agent(var_AgI),var__),clause(ext_agent(var_Ag,_273996,var_Ontology,_273998),_273993),not(call_meta_execute_propose(var_A,var_C,var_Ag)),a(message(var_Ag,reject_proposal(var_A,[],var_AgI))),retractall(ext_agent(var_Ag,_274024,var_Ontology,_274026)).

call_accept_proposal(var_A,var_Mp,var_Ag,var_T):-asse_cosa(past_event(accepted_proposal(var_A,var_Mp,var_Ag),var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(accepted_proposal(var_A,var_Mp,var_Ag),var__,var_Ag)),assert(past(accepted_proposal(var_A,var_Mp,var_Ag),var_Tp,var_Ag)),datime(_273949),retractall(evtp_date(accepted_proposal(var_A,var_Mp,var_Ag),_273957)),assert(evtp_date(accepted_proposal(var_A,var_Mp,var_Ag),_273949)).

call_reject_proposal(var_A,var_Mp,var_Ag,var_T):-asse_cosa(past_event(rejected_proposal(var_A,var_Mp,var_Ag),var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(rejected_proposal(var_A,var_Mp,var_Ag),var__,var_Ag)),assert(past(rejected_proposal(var_A,var_Mp,var_Ag),var_Tp,var_Ag)),datime(_273865),retractall(evtp_date(rejected_proposal(var_A,var_Mp,var_Ag),_273873)),assert(evtp_date(rejected_proposal(var_A,var_Mp,var_Ag),_273865)).

call_failure(var_A,var_M,var_Ag,var_T):-asse_cosa(past_event(failed_action(var_A,var_M,var_Ag),var_T)),statistics(walltime,[var_Tp,var__]),retractall(past(failed_action(var_A,var_M,var_Ag),var__,var_Ag)),assert(past(failed_action(var_A,var_M,var_Ag),var_Tp,var_Ag)),datime(_273781),retractall(evtp_date(failed_action(var_A,var_M,var_Ag),_273789)),assert(evtp_date(failed_action(var_A,var_M,var_Ag),_273781)).

call_cancel(var_A,var_Ag):-if(clause(high_action(var_A,var_Te,var_Ag),_273691),retractall(high_action(var_A,var_Te,var_Ag)),true),if(clause(normal_action(var_A,var_Te,var_Ag),_273708),retractall(normal_action(var_A,var_Te,var_Ag)),true).

external_refused_action_propose(var_A,var_Ag):-clause(not_executable_action_propose(var_A,var_Ag),var__).

evi(external_refused_action_propose(var_A,var_Ag)):-clause(agent(var_Ai),var__),a(message(var_Ag,failure(var_A,motivation(false_conditions),var_Ai))),retractall(not_executable_action_propose(var_A,var_Ag)).

refused_message(var_AgM,var_Con):-clause(eliminated_message(var_AgM,var__,var__,var_Con,var__),var__).

refused_message(var_To,var_M):-clause(eliminated_message(var_M,var_To,motivation(conditions_not_verified)),_273599).

evi(refused_message(var_AgM,var_Con)):-clause(agent(var_Ai),var__),a(message(var_AgM,inform(var_Con,motivation(refused_message),var_Ai))),retractall(eliminated_message(var_AgM,var__,var__,var_Con,var__)),retractall(eliminated_message(var_Con,var_AgM,motivation(conditions_not_verified))).

send_jasper_return_message(var_X,var_S,var_T,var_S0):-clause(agent(var_Ag),_273523),a(message(var_S,send_message(sent_rmi(var_X,var_T,var_S0),var_Ag))).

gest_learn(var_H):-clause(past(learn(var_H),var_T,var_U),_273497),learn_if(var_H,var_T,var_U).

evi(gest_learn(var_H)):-retractall(past(learn(var_H),_273435,_273436)),clause(agente(_273446,_273447,_273448,var_S),_273444),name(var_S,var_N),append(var_L,[46,112,108],var_N),name(var_F,var_L),manage_lg(var_H,var_F),a(learned(var_H)).

cllearn:-clause(agente(_273332,_273333,_273334,var_S),_273330),name(var_S,var_N),append(var_L,[46,112,108],var_N),append(var_L,[46,116,120,116],var_To),name(var_FI,var_To),open(var_FI,read,_273382,[]),repeat,read(_273382,var_T),arg(1,var_T,var_H),write(var_H),nl,var_T==end_of_file,!,close(_273382).

send_msg_learn(var_T,var_A,var_Ag):-a(message(var_Ag,confirm(learn(var_T),var_A))).

told(var_From,send_message(var_M)):-trust(var_From,var_X,var__),var_X>0.2.

told(var_Ag,execute_proc(var__)):-true.

told(var_Ag,query_ref(var__,var__)):-true.

told(var_Ag,agree(var__)):-true.

told(var_Ag,confirm(var__),200):-true.

told(var_Ag,disconfirm(var__)):-true.

told(var_Ag,request(var__,var__)):-true.

told(var_Ag,propose(var__,var__)):-true.

told(var_Ag,accept_proposal(var__,var__),20):-true.

told(var_Ag,reject_proposal(var__,var__),20):-true.

told(var__,failure(var__,var__),200):-true.

told(var__,cancel(var__)):-true.

told(var_Ag,inform(var__,var__),70):-true.

told(var_Ag,inform(var__),70):-true.

told(var_Ag,reply(var__)):-true.

told(var__,refuse(var__,var_Xp)):-functor(var_Xp,var_Fp,var__),var_Fp=agree.

tell(var_To,var_From,send_message(var_M)):-trust(var_To,var_X,var__),var_X>0.2.

tell(var_To,var__,confirm(var__)):-true.

tell(var_To,var__,disconfirm(var__)):-true.

tell(var_To,var__,propose(var__,var__)):-true.

tell(var_To,var__,request(var__,var__)):-true.

tell(var_To,var__,execute_proc(var__)):-true.

tell(var_To,var__,agree(var__)):-true.

tell(var_To,var__,reject_proposal(var__,var__)):-true.

tell(var_To,var__,accept_proposal(var__,var__)):-true.

tell(var_To,var__,failure(var__,var__)):-true.

tell(var_To,var__,query_ref(var__,var__)):-true.

tell(var_To,var__,eve(var__)):-true.

tell(var__,var__,refuse(var_X,var__)):-functor(var_X,var_F,var__),(var_F=send_message;var_F=query_ref).

tell(var_To,var__,inform(var__,var_M)):-true;var_M=motivation(refused_message).

tell(var_To,var__,inform(var__)):-true,var_To\=user.

tell(var_To,var__,propose_desire(var__,var__)):-true.

meta(var_P,var_V,var_AgM):-functor(var_P,var_F,var_N),var_N=0,clause(agent(var_Ag),var__),clause(ontology(var_Pre,[var_Rep,var_Host],var_Ag),var__),if((eq_property(var_F,var_V,var_Pre,[var_Rep,var_Host]);same_as(var_F,var_V,var_Pre,[var_Rep,var_Host]);eq_class(var_F,var_V,var_Pre,[var_Rep,var_Host])),true,if(clause(ontology(var_PreM,[var_RepM,var_HostM],var_AgM),var__),if((eq_property(var_F,var_V,var_PreM,[var_RepM,var_HostM]);same_as(var_F,var_V,var_PreM,[var_RepM,var_HostM]);eq_class(var_F,var_V,var_PreM,[var_RepM,var_HostM])),true,false),false)).

meta(var_P,var_V,var_AgM):-functor(var_P,var_F,var_N),(var_N=1;var_N=2),clause(agent(var_Ag),var__),clause(ontology(var_Pre,[var_Rep,var_Host],var_Ag),var__),if((eq_property(var_F,var_H,var_Pre,[var_Rep,var_Host]);same_as(var_F,var_H,var_Pre,[var_Rep,var_Host]);eq_class(var_F,var_H,var_Pre,[var_Rep,var_Host])),true,if(clause(ontology(var_PreM,[var_RepM,var_HostM],var_AgM),var__),if((eq_property(var_F,var_H,var_PreM,[var_RepM,var_HostM]);same_as(var_F,var_H,var_PreM,[var_RepM,var_HostM]);eq_class(var_F,var_H,var_PreM,[var_RepM,var_HostM])),true,false),false)),var_P=..var_L,substitute(var_F,var_L,var_H,var_Lf),var_V=..var_Lf.

meta(var_P,var_V,var__):-functor(var_P,var_F,var_N),var_N=2,symmetric(var_F),var_P=..var_L,delete(var_L,var_F,var_R),reverse(var_R,var_R1),append([var_F],var_R1,var_R2),var_V=..var_R2.

meta(var_P,var_V,var_AgM):-clause(agent(var_Ag),var__),functor(var_P,var_F,var_N),var_N=2,(symmetric(var_F,var_AgM);symmetric(var_F)),var_P=..var_L,delete(var_L,var_F,var_R),reverse(var_R,var_R1),clause(ontology(var_Pre,[var_Rep,var_Host],var_Ag),var__),if((eq_property(var_F,var_Y,var_Pre,[var_Rep,var_Host]);same_as(var_F,var_Y,var_Pre,[var_Rep,var_Host]);eq_class(var_F,var_Y,var_Pre,[var_Rep,var_Host])),true,if(clause(ontology(var_PreM,[var_RepM,var_HostM],var_AgM),var__),if((eq_property(var_F,var_Y,var_PreM,[var_RepM,var_HostM]);same_as(var_F,var_Y,var_PreM,[var_RepM,var_HostM]);eq_class(var_F,var_Y,var_PreM,[var_RepM,var_HostM])),true,false),false)),append([var_Y],var_R1,var_R2),var_V=..var_R2.

meta(var_P,var_V,var_AgM):-clause(agent(var_Ag),var__),clause(ontology(var_Pre,[var_Rep,var_Host],var_Ag),var__),functor(var_P,var_F,var_N),var_N>2,if((eq_property(var_F,var_H,var_Pre,[var_Rep,var_Host]);same_as(var_F,var_H,var_Pre,[var_Rep,var_Host]);eq_class(var_F,var_H,var_Pre,[var_Rep,var_Host])),true,if(clause(ontology(var_PreM,[var_RepM,var_HostM],var_AgM),var__),if((eq_property(var_F,var_H,var_PreM,[var_RepM,var_HostM]);same_as(var_F,var_H,var_PreM,[var_RepM,var_HostM]);eq_class(var_F,var_H,var_PreM,[var_RepM,var_HostM])),true,false),false)),var_P=..var_L,substitute(var_F,var_L,var_H,var_Lf),var_V=..var_Lf.

meta(var_P,var_V,var_AgM):-clause(agent(var_Ag),var__),clause(ontology(var_Pre,[var_Rep,var_Host],var_Ag),var__),functor(var_P,var_F,var_N),var_N=2,var_P=..var_L,if((eq_property(var_F,var_H,var_Pre,[var_Rep,var_Host]);same_as(var_F,var_H,var_Pre,[var_Rep,var_Host]);eq_class(var_F,var_H,var_Pre,[var_Rep,var_Host])),true,if(clause(ontology(var_PreM,[var_RepM,var_HostM],var_AgM),var__),if((eq_property(var_F,var_H,var_PreM,[var_RepM,var_HostM]);same_as(var_F,var_H,var_PreM,[var_RepM,var_HostM]);eq_class(var_F,var_H,var_PreM,[var_RepM,var_HostM])),true,false),false)),substitute(var_F,var_L,var_H,var_Lf),var_V=..var_Lf.
