% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- multifile user:term_expansion/6, scrittura/1.
:-dynamic rewrite_clause_le/1, parentesi_le/1,rewrite_clause/1,re_write/2, evento_aperto_le/0, examine_all/2, parentesi/1, buffer/1, cifre/1, cifre/2, cifre/4, residue/1, evento_aperto/0, eventi_esterni/1, deltaT/1.

:-op(500,xfy,:>).
:-op(500,xfy,:<).
:-op(1200,fy,:~).
:-op(1200,xfy,</).
:-op(500,xfy,?/).


:-op(1200,xfx,[:-,:>]).
:-op(1200,xfx,[:-,:<]).
:-op(1200,xfx,[:-,:~]).
:-op(1200,xfx,[:-,~/]).
:-op(1200,xfx,[:-,</]).
:-op(1200,xfx,[:-,?/]).
:-use_module(library(lists)).

user:term_expansion((H:>B),[],[],(H:-B),[],[]).
user:term_expansion((H:<B),[],[],(cd(H):-B),[],[]).
user:term_expansion((:~B),[],[],(vincolo:-B),[],[]).
user:term_expansion((H~/B),[],[],(export_past(H):-decompose(H,B)),[],[]).
user:term_expansion((H</B),[],[],(export_past_not_do(H):-decompose_not_do(H,B)),[],[]).
user:term_expansion((H?/B),[],[],(export_past_do(H):-decompose_if_it_is(H,B)),[],[]).


%%EDITED
token(F):-leggiFile(F,Fi),tokenize(Fi,L),take_meta(L,F).
token_fil(F):-leggiFile_fil(F,Fi),tokenize(Fi,L),take_meta_fil(L,F).
token_clause(C,F,F1):-crea_rand(F,F1),name(C,L),tokenize(L,S),append(Lb,['EOL'],S),append(Lb,['.'],Lb1),append(Lb1,['EOL'],Lb2),take_meta_update(Lb2,F,F1).

crea_rand(F,T):-random(97,122,A4),random(97,122,A5),random(97,122,A6),name(F,Lf),append(Lf,[A4],F1),append(F1,[A5],F2),append(F2,[A6],Fi),name(T,Fi).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Lettura del file e generazione corrispondente lista ascii    % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chiamata: leggiFile(Input,Output)                              %
%     Input  = nome file da leggere                              %
%     Output = lista dei codici ascii                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
charEof(-1). %codice "ascii" dell'end_of_file

charEol(10). %codice ascii dell'end_of_line


leggiFile(Infile,Txt) :-			%apertura file,lettura righe
	atom_concat(Infile,'.txt',File),
	see(File),   
	leggiChars(Txt), !,
	seen.

leggiFile_fil(Infile,Txt) :-			%apertura file,lettura righe
	atom_concat(Infile,'.con',File),
	see(File), 
	leggiChars(Txt), !,
	seen.

charBlank(32).

leggiChars(Final):-
	get_code(Kh), 
	if( charEof(Kh), Final=[], 
		(if( (charEol(Kh); charBlank(Kh)), leggiChars1(32,[], Final), leggiChars1(Kh, [Kh], Final)))).

leggiChars1(Prev, Temp, Final):-
	get_code(Kh),
	if( charEof(Kh), reverse(Temp,Final),
		(if( (charBlank(Kh);charEol(Kh)), leggiChars1(32,Temp,Final),
			(if( charBlank(Prev), 
				(Temp1 = [32|Temp], leggiChars1(Kh,[Kh|Temp1],Final)),
				(leggiChars1(Kh,[Kh|Temp],Final))
			))
		))
	).
		 		


%%EDIT Aggiunto un trace
skipBlanks(Ch,NextCh) :- 
	get_code(Kh),
	(   ( [Kh]=" "; charEol(Kh)   ) -> Ch=32, skipBlanks(_,NextCh);Ch=Kh, NextCh=Kh ).

%=================================================================================
% LEXICAL ANALYZER
%=================================================================================


%%EDITED MESSO UN TRACE'
tokenize(Input,Output) :- % trasforma una lista di codici ascii in una di lessemi
	tokenize(Output,Input,Residue), !, 
	(Residue=[] -> true; assert(residue(Residue))).

tokenize(Lexxs) --> chList( ChList, Sym ), tokenize(Lexs),
			  { name(Tok,ChList), putBefore(Tok, Sym, Lexs, Lexxs) }.
tokenize(['EOL']) -->  "".

putBefore('','',Lexs,Lexs).
putBefore(Tok,'',Lexs,[Tok|Lexs]).
putBefore('',Sym,Lexs,[Sym|Lexs]).
putBefore(Tok,Sym,Lexs,[Tok,Sym|Lexs]).

%chList([],'') --> [Ch], {[Ch]=" "}. 
chList([],Sym) -->simbolo(Sym).
chList([Ch|ChList],Sym) --> [Ch], chList(ChList,Sym).


simbolo(Sym) --> implicitaz(Sym)|
                 simb_univ(Sym)
			| identific(Sym)
                        | assegnamento(Sym)
			| separatore(Sym)
			| letterale(Sym).

letterale(Lett) --> ".", tillquote("$",T), {[Q]=".", name(Lett,[Q|T])}.
letterale(Lett) --> "'", tillquote("'",T), {[Q]="'", name(Lett,[Q|T])}.
letterale(Lett) --> "'s ", {[Q,S]="'s", name(Lett,[Q,Q,S])}. % genitivo sassone

tillquote([Q],[Q]) --> {[Q]="'"}, [Q].
tillquote([Q],[Q]) --> [Q].
tillquote(D,[Ch|T]) --> [Ch], tillquote(D,T).

identific(Id) --> lettera(L), lettere_cifre(Chars), {name(Id,[L|Chars])}
			| "_", lettere_cifre(Chars), {[L]="_", name(Id,[L|Chars])}
                                          |numero(N),".",numero(N1),{[L]=".",name(Id,[N|[L,N1]])}
			| lettera(D), cifre(Chars,L), {L \== -2, name(Id,[D|Chars])}.

assegnamento(Token) --> ass(Ch), ass1(Ch1), {name(Token,[Ch,Ch1])}.
ass(Ch)--> [Ch], {[Ch]=":";[Ch]="~";[Ch]="<";[Ch] = "?"}.
ass1(Ch)--> [Ch], {[Ch]="="; [Ch]="-";[Ch]=">";[Ch]="<";[Ch]="~";[Ch]="<";[Ch]="/";[Ch] = "?";[Ch]=":"}.

lettere_cifre([Ch|Chars]) --> ( "_", {[Ch]="_"} |lettera(Ch) |lettera(Ch) ), lettere_cifre(Chars).
lettere_cifre([]) --> "".


lettera(L) --> [L], { "A" =< [L], [L] =< "Z"; "a" =< [L], [L] =< "z";"0" =< [L], [L] =< "9"}.

numero(D) --> [D], { "0" =< [D], [D] =< "9"}.

implicitaz(('==>')) --> "=", "=", ">".
simb_univ(('=..'))-->"=",".",".","?".

separatore(Lexeme) --> sep(Ch) , {name(Lexeme,[Ch])}.
sep(Ch) --> [Ch],
		{[Ch] = "%";
		 [Ch] = "{"; [Ch] = "}";
                             [Ch] = ".";
		 [Ch] = "["; [Ch] = "]";
		 [Ch] = "("; [Ch] = ")";
		 [Ch] = ","; [Ch] = ";";
		 [Ch] = ":"; [Ch] = "|";
                 [Ch] = "/"; [Ch] = "~";
                 [Ch] = "<"; [Ch] = "/";[Ch] = "?"}. 

take_meta(L,F):-assert(eventi_esterni(0)),assert(deltaT(0)),assert(parentesi(0)), assert(buffer([])), name(F,Lf),append(Lf,[46,112,108],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true),
                        last(L,U),
repeat,
member(Me,L),
examine_all(Me),
Me==U,!,if(clause(residue(R),_),(name(R1,R),examine_all(R1)),true),
	if(clause(buffer(ParsedC),_), 
	( retractall(buffer(_)), name(Parsed,ParsedC), 
	  open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
	), (write('Errore take_meta'),nl)),re_file(Nf).

take_meta_fil(L,F):-assert(parentesi(0)),assert(buffer([])),name(F,Lf),append(Lf,[46,112,108],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true),
                        last(L,U),
repeat,
member(Me,L),
examine_all(Me),
Me==U,!,if(clause(residue(R),_),(name(R1,R),examine_all(R1,Nf)),true), 
if(clause(buffer(ParsedC),_), 
	( retractall(buffer(_)), name(Parsed,ParsedC), 
	  open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
	), (write('Errore take_meta_fil'),nl)).

examine_all(Me):-if(Me='EOL',true,examine_all1(Me)).


examine_all1(Me):-if(member(Me,['(',')']), conta_parentesi(Me),true),
                  if(tempo(Me), scrittura(Me),                 % controlla se è stato inserito il deltat, se si scrivo nel file pl e asserisco time_add
                    (if(variabile(Me),examine_variable(Me),
                        if(label(Me),examine_label(Me),write_NovarNolabel(Me)))

                 )).


variabile(Me):-name(Me,L),
                                    nth0(0,L,El),
                                         isa_variable(El).

isa_variable(El):-El>64,El<91.
isa_variable(El):-El=95.

%Verifica se è stato inserito il delta Temporale dall'agente

tempo(Me):- name(Me,L), nth0(0,L,El,L1), El==116, numbertime(L1).                       %controlla che il primo carattere è una t
numbertime(L1):- nth0(0,L1,El,L_rest),check_number(El), scorri(L_rest).                 %controlla se il primo elemento della lista è un numero e scorre la lista
check_number(El):- El>47, El<58.                                                        %range in ASCII per i numeri da 0 a 9
scorri(L_rest):-if(L_rest=[],true, scorri_list(L_rest)).                                %controlla che tutti gli elementi della lista sono numeri 
scorri_list(L_rest):- nth0(0,L_rest,X,L2), check_number(X),scorri(L2).
scrittura(Me):- name(Me,L),nth0(0,L,R,L3), append([100,101,108,116,97,116,40],L3,L1),   %scrittura sul file pl del deltat inserito dall'agente
                append(L1,[41],L2), clause(buffer(Parsed),_), retractall(buffer(_)),
		append(Parsed,L2,Parola),assert(buffer(Parola)),
		clause(deltaT(X),true),retractall(deltaT(X)),assert(deltaT(1)).         %asserisco deltat a 1 in modo tale da sapere che è stato inserito



re_write(L):-arg(1,L,N1),if(N1=39,aggiungi_39(L),non_aggiungi(L)).%%EDITED RIGA SOTTO
aggiungi_39(L):-append([39,39,39,39,39,39],L,Lf),append(Lf,[39,39,39,39,39,39],Lf1),
						clause(buffer(Parsed),_), retractall(buffer(_)),
						append(Parsed,Lf1, Parola), assert(buffer(Parola)).

non_aggiungi(L):-clause(buffer(Parsed),_), retractall(buffer(_)),append(Parsed,L,Parola),
						  assert(buffer(Parola)).

write_NovarNolabel(Me):-if((member(Me,[':-',':>',':<',',','.',';','~/','</','?/']), check_parentesi), write_parentesi, true),
                                     
                                        name(Me,L),re_write(L).
write_parentesi:-re_write([41]),retractall(evento_aperto),retractall(parentesi(_)),assert(parentesi(0)).
check_parentesi:-if((clause(evento_aperto,_),clause(parentesi(0),_)),true,false).

examine_variable(Me):-name(Me,L),append([118,97,114,95],L,Lt),re_write(Lt).

%ESAMINA LE ETICHETTE DEGLI EVENTI%
label(Me):-name(Me,L),nth0(0,L,El),piccolo(El),last(L,U),app_label(U).


piccolo(El):-El>96,El<123.
app_label(U):-U=65;U=69;U=73;U=71;U=84;U=80;U=78;U=82.


examine_label(Me):-name(Me,L),last(L,U),if(U=65,appA(L,U),if(U=69,appE(L,U),if(U=73,appI(L,U),
                      if(U=71,appG(L,U),if(U=84,appT(L,U),if(U=80,appP(L,U),if(U=78,appN(L,U),if(U=82,appR(L,U),true)))))))).
appA(L,U):-length(L,N),nth1(N,L,U,R),append([97,40],R,L1),
                re_write(L1),assert(evento_aperto).
appE(L,U):- conta_eventi_esterni,length(L,N),nth1(N,L,U,R),append([101,118,101,40],R,L1), %inserito conta_eventi_esterni 
                re_write(L1),assert(evento_aperto).   
appI(L,U):-length(L,N),nth1(N,L,U,R),append([101,118,105,40],R,L1),
                re_write(L1),assert(evento_aperto). 
appG(L,U):-length(L,N),nth1(N,L,U,R),append([111,98,103,40],R,L1),
                re_write(L1),assert(evento_aperto).        
appT(L,U):-length(L,N),nth1(N,L,U,R),append([116,101,115,103,40],R,L1),
                re_write(L1),assert(evento_aperto).  
appP(L,U):-length(L,N),nth1(N,L,U,R),append([101,118,112,40],R,L1),
                re_write(L1),assert(evento_aperto). 
appN(L,U):-length(L,N),nth1(N,L,U,R),append([101,110,40],R,L1),
                re_write(L1),assert(evento_aperto). 
appR(L,U):-length(L,N),nth1(N,L,U,R),append([114,101,109,40],R,L1),
                re_write(L1),assert(evento_aperto). 

conta_eventi_esterni:- clause(eventi_esterni(X),true), R is X+1, retractall(eventi_esterni(X)),assert(eventi_esterni(R)). %contatore degli eventi esterni

conta_parentesi(El):-name(El,L),append(L,[fine],Lf),
              repeat,
                member(M,Lf),
                    if(M=40,contatore_piu,true),
                    if(M=41,contatore_meno,true),
                M==fine,!.

contatore_piu:-clause(parentesi(X),_),R is X+1,retractall(parentesi(X)),assert(parentesi(R)).

contatore_meno:-clause(parentesi(X),_),R is X-1,retractall(parentesi(X)),assert(parentesi(R)).


re_file(Nf):- see(Nf),
                     repeat,
                     read(T),
                     expand_term(T,Te),
                     assert(rewrite_clause(Te)),
                    T==end_of_file, 
                   
	     seen,rewrite_program_clause(Nf).

rewrite_program_clause(Nf):-if(file_exists(Nf),delete_file(Nf),true),
                            findall(T,clause(rewrite_clause(T),_),L),
                            last(L,U),
                            open(Nf,append,Stream,[]),
                            repeat,
                                 member(Me,L),
                                 if(Me=end_of_file,true,write_prog_cl(Stream,Me)),
                                 
                            Me==U,!,
                            close(Stream),retractall(rewrite_clause(_)).
                             
write_prog_cl(Stream,Me):-write(Stream,Me),write(Stream,'.'),nl(Stream).        
      

%GESTIONE LEARNING CLAUSES
take_meta_update(L,F,F1):-assert(parentesi_le(0)),last(L,U),
                       repeat,
                      member(Me,L),
                      examine_clause(Me,F1),
                         Me==U,!,re_file_le(F,F1).


examine_clause(Me,Nf):-if(Me='EOL',true,examine_clause1(Me,Nf)).



examine_clause1(Me,Nf):-if(member(Me,['(',')']), conta_parentesi_le(Me),true),
		if(variabile(Me),examine_variable_le(Me,Nf),
			if(label(Me),examine_label_le(Me,Nf),write_NovarNolabel_le(Me,Nf))

		 ).

re_write1(L,Nf):-arg(1,L,N1),if(N1=39,aggiungi_39_le(L,Nf),non_aggiungi_le(L,Nf)).

aggiungi_39_le(L,Nf):-append([39,39,39,39,39,39],L,Lf),append(Lf,[39,39,39,39,39,39],Lf1),name(T,Lf1),open(Nf,append,Stream,[]),
                    write(Stream,T),close(Stream).
non_aggiungi_le(L,Nf):-name(T,L),open(Nf,append,Stream,[]),
                    write(Stream,T),close(Stream).

write_NovarNolabel_le(Me,Nf):-if((member(Me,[':-',':>',':<',',','.',';','.%','~/','</','?/','EOL']),check_parentesi_le),                write_parentesi_le(Nf), true),
                                     
                                        name(Me,L),re_write(L,Nf).
write_parentesi_le(Nf):-re_write1([41],Nf),retractall(evento_aperto_le),retractall(parentesi_le(_)),assert(parentesi_le(0)).
check_parentesi_le:-if((clause(evento_aperto_le,_),clause(parentesi_le(0),_)),true,false).

examine_variable_le(Me,Nf):-name(Me,L),append([118,97,114,95],L,Lt),re_write1(Lt,Nf).



%ESAMINA LE ETICHETTE DEGLI EVENTI%

examine_label_le(Me,Nf):-name(Me,L),last(L,U),if(U=65,app_leA(L,U,Nf),if(U=69,app_leE(L,U,Nf),if(U=73,app_leI(L,U,Nf),
                      if(U=71,app_leG(L,U,Nf),if(U=84,app_leT(L,U,Nf),if(U=80,app_leP(L,U,Nf),if(U=78,app_leN(L,U,Nf),if(U=82,app_leR(L,U,Nf),true)))))))).
app_leA(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([97,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le).
app_leE(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([101,118,101,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le).   
app_leI(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([101,118,105,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le). 
app_leG(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([111,98,103,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le).        
app_leT(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([116,101,115,103,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le).  
app_leP(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([101,118,112,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le). 
app_leN(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([101,110,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le). 
app_leR(L,U,Nf):-length(L,N),nth1(N,L,U,R),append([114,101,109,40],R,L1),
                re_write1(L1,Nf),assert(evento_aperto_le). 

conta_parentesi_le(El):-name(El,L),append(L,[fine],Lf),
              repeat,
                member(M,Lf),
                    if(M=40,contatore_piu_le,true),
                    if(M=41,contatore_meno_le,true),
                M==fine,!.

contatore_piu_le:-clause(parentesi_le(X),_),R is X+1,retractall(parentesi_le(X)),assert(parentesi_le(R)).

contatore_meno_le:-clause(parentesi_le(X),_),R is X-1,retractall(parentesi_le(X)),assert(parentesi_le(R)).

re_file_le(Nf,Nf1):- see(Nf1),
                     repeat,
                     read(T),
                     expand_term(T,Te),
                     assert(rewrite_clause_le(Te)), 
                    T==end_of_file, 
	         seen,rewrite_program_clause_le(Nf).



rewrite_program_clause_le(Nf):-name(Nf,Lnf),append(Lnf,[46,112,108],Lnff),
                 name(Tnf,Lnff),
                            findall(T,clause(rewrite_clause_le(T),_),L),                            last(L,U),
                            open(Tnf,append,Stream,[]),
                            repeat,
                                 member(Me,L),
                                 if(Me=end_of_file,true,write_prog_cl_le(Stream,Me)),
                                 
                            Me==U,!,retractall(rewrite_clause_le(_)),                            close(Stream).
                             
write_prog_cl_le(Stream,Me):-write(Stream,Me),write(Stream,'.'),nl(Stream),retractall(parentesi_le(0)).  


