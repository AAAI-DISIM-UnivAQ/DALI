% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- multifile user:term_expansion/6.
:-op(500,xfy,:>).
:-op(500,xfy,:<).
:-op(1200,xfx,[:-,:>]).
:-op(1200,xfx,[:-,:<]).

:-use_module(library(lists)).
user:term_expansion((H:>B),[],[],(H:-B),[],[]).
user:term_expansion((H:<B),[],[],(cd(H):-B),[],[]).
:-use_module(library(random)),
  use_module(library(lists)),
  use_module(library(system)),
  use_module(library(file_systems)).


token(C):-name(C,Fi),tokenize(Fi,L),take_meta(L).


take_var(L):- name(ab11cf6h33,Lf),append(Lf,[95,116,111,107,46,116,120,116],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true),
                        last(L,U),repeat,member(Me,L),examine0(Me,Nf),Me==U,!,app_residue(Nf),
                        display_f(Nf).
examine0(Me,Nf):-if(Me='EOL',true,if(Me='.',(point_write(Nf),nl_write(Nf)),examine(Me,Nf))).
examine(Me,Nf):-name(Me,L),nth0(0,L,El),if(isa_variable(El),app_variable(L,Nf),re_write(L,Nf)).

isa_variable(El):-El>64,El<91.
isa_variabile(El):-El=95.

app_variable(L,Nf):-append([118,97,114,95],L,Lt),re_write(Lt,Nf).
re_write(L,Nf):-name(T,L),open(Nf,append,Stream,[]),
                    write(Stream,T), close(Stream).
nl_write(Nf):-open(Nf,append,Stream,[])
                 ,nl(Stream),close(Stream).
point_write(Nf):-open(Nf,append,Stream,[]),write(Stream,'.'), close(Stream).
app_residue(Nf):-if(clause(residue(R),_),(re_write(R,Nf),retractall(residue(R))),true).
res_write(R,Nf):-if(R=46,(point_write(Nf),nl_write(Nf)),(re_write(R,Nf),nl_write(Nf))).


display_f(Nf):-see(Nf),
                     repeat,
                     read(T),
                     assert((T)),
                     
                    T==end_of_file, !,
	     seen.
               
               



