% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

togli_var(F):-leggiFile_var(F,Fi),tokenize(Fi,L),take_meta_var(L,F).
togli_var_fil(F):-leggiFile_var(F,Fi),tokenize(Fi,L),take_meta_var_fil(L,F).
togli_var_clause(F,F1):-expand_le(F1,Te),if(file_exists(F1),delete_file(F1),true),riwrite_le(Te,F1),leggiFile_var_clause(F1,Li),tokenize(Li,L),if(file_exists(F1),delete_file(F1),true),take_meta_var_clause(L,F).


leggiFile_var(Infile,Txt) :-			%apertura file,lettura righe
					%e chiusura file
	
	atom_concat(Infile,'.pl',File),
	see(File),   
	leggiChars_var(Txt), !,
	seen.

togli_var_ple(F):-leggiFile_var_ple(F,Fi),tokenize(Fi,L),take_meta_var_ple(L,F).

leggiFile_var_ple(Infile,Txt) :-			%apertura file,lettura righe
					%e chiusura file
	
	atom_concat(Infile,'.ple',File),
	see(File),   
	leggiChars_var(Txt), !,
	seen.

%%Mia interpretazione del problema
%% 32 è il codice ascii del blank.

leggiChars_var(Final):-
	get_code(Kh), 
	if( charEof(Kh), Final=[], 
		(if( (charEol(Kh); charBlank(Kh)), leggiChars1_var(32,[], Final), leggiChars1_var(Kh, [Kh], Final)))
	)
.

leggiChars1_var(Prev, Temp, Final):-
	get_code(Kh),
	if( charEof(Kh), reverse(Temp,Final),
		(if( (charBlank(Kh);charEol(Kh)), leggiChars1_var(32,Temp,Final),
			(if( charBlank(Prev), 
				(Temp1 = [32|Temp], leggiChars1(Kh,[Kh|Temp1],Final)),
				(leggiChars1_var(Kh,[Kh|Temp],Final))
			))
		))
	)
.


%%''Il CODICE ASCII è .plv !!!
take_meta_var(L,F):- name(F,Lf),append(Lf,[46,112,108,118],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true),assert(buffer([])),
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var,
							  if(clause(buffer(ParsedC),_), 
									(  retractall(buffer(_)), name(Parsed,ParsedC), 
	  									open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var'),nl)
							  ), aprifile_head_mul(Nf). %% NF Sarebbe il plv


take_meta_var_fil(L,F):- name(F,Lf),append(Lf,[46,116,120,116],Lft),
                        name(Nf,Lft),write(Nf),nl,if(file_exists(Nf),delete_file(Nf),true),assert(buffer([])),
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var,
								if(clause(buffer(ParsedC),_), 
									( retractall(buffer(_)), name(Parsed,ParsedC), 
	 								  open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var_fil'),nl)
								).



take_meta_var_ple(L,F):- name(F,Lf),append(Lf,[46,112,108,101],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true), assert(buffer([])),
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var,
								if(clause(buffer(ParsedC),_), 
									( retractall(buffer(_)), name(Parsed,ParsedC), 
	 								 open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var_ple'),nl)
								).

examine0_var(Me):-if(Me='EOL',true,if(Me='. ', (point_write_var,nl_write_var),examine_var(Me))).
examine_var(Me):-name(Me,L),re_isa_cod_var(L).
re_isa_cod_var(L):-if(isa_code_var(L),disapp_variable_var(L),re_write_var(L)).
     
isa_code_var(L):-nth0(0,L,El0),nth0(1,L,El1),nth0(2,L,El2),nth0(3,L,El3),El0=118,El1=97,El2=114,El3=95.


disapp_variable_var(L):-append([118,97,114,95],Lt,L),re_write_var(Lt).
re_write_var(L):-non_aggiungi_var(L).

%%Scritture
aggiungi_39_var(L):-append([39,39],L,Lf),append(Lf,[39,39],Lf1),
							  clause(buffer(Parsed),_), retractall(buffer(_)),
							  append(Parsed,Lf1, Parola), assert(buffer(Parola)).

non_aggiungi_var(L):-clause(buffer(Parsed),_), retractall(buffer(_)),
							   append(Parsed,L, Parola), assert(buffer(Parola)).

nl_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
						append(Parsed,[10], Parola), assert(buffer(Parola)).
point_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
							append(Parsed,[46], Parola), assert(buffer(Parola)).
%%

app_residue_var:-if(clause(residue(R),_),(re_isa_cod_var(R),retractall(residue(R))),true).
res_write_var(R):-if(R=46,(point_write_var,nl_write_var),(re_write_var(R),nl_write_var)).


%GESTIONE LEARNING

expand_le(F1,Te):- open(F1,read,Stream,[]),
                read(Stream,T),
                expand_term(T,Te),
	        close(Stream).

riwrite_le(Te,F1):-open(F1,write,Stream,[]),
               write(Stream,Te),write(Stream,'.'),nl(Stream),
	        close(Stream).

                     
                     
leggiFile_var_clause(Infile,Txt) :-			%apertura file,lettura righe
                                            %e chiusura file
	see(Infile), 
	leggiChars_var(Txt), !,
	seen.

take_meta_var_clause(L,F):- name(F,Lf),append(Lf,[46,112,108,118],Lft),
                        name(Nf,Lft),
                        assert(buffer([10])),%%10 = newline.
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var(Nf),
								if(clause(buffer(ParsedC),_), 
									( retractall(buffer(_)), name(Parsed,ParsedC), 
	 								 open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var_ple'),nl)
								), azioni(Nf),compile(Nf).





