% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- module(remove_var, [remove_var/1, remove_var_fil/1, remove_var_clause/2, remove_var_ple/1, examine0_var/1]).

:- use_module(library(file_systems)).
:- use_module(tokefun, [leggiChars/1]).

:- ['tokefun.pl'].


remove_var(F):-readFile_var(F,Fi),tokenize(Fi,L),take_meta_var(L,F).
remove_var_fil(F):-
    readFile_var_fil(F,Fi),
    tokenize(Fi,L),
    take_meta_var_fil(L,F).
remove_var_clause(F,F1):-expand_le(F1,Te),if(file_exists(F1),delete_file(F1),true),rewrite_le(Te,F1),readFile_var_clause(F1,Li),tokenize(Li,L),if(file_exists(F1),delete_file(F1),true),take_meta_var_clause(L,F).

readFile_var(Infile,Txt) :-			%open file, read lines
					%e chiusura file
	
	atom_concat(Infile,'.pl',File),
	see(File),   
	readChars_var(Txt), !,
	seen.

readFile_var_fil(Infile,Txt) :-
    atom_concat(Infile,'.con',File),
    catch(
        (see(File), 
         leggiChars(Txt), !,
         seen),
        Error,
        (write('DEBUG [readFile_var_fil]: Errore: '), write(Error), nl, fail)
    ).

remove_var_ple(F):-readFile_var_ple(F,Fi),tokenize(Fi,L),take_meta_var_ple(L,F).

readFile_var_ple(Infile,Txt) :-			%apertura file,lettura righe
					%e chiusura file
	
	atom_concat(Infile,'.ple',File),
	see(File),   
	readChars_var(Txt), !,
	seen.

readChars_var(Final):-
    get_code(Kh), 
    if( charEof(Kh), Final=[], 
        (if( (charEol(Kh); charBlank(Kh)), readChars1_var(32,[], Final), readChars1_var(Kh, [Kh], Final)))
    ).

readChars1_var(Prev, Temp, Final):-
    get_code(Kh),
    if( charEof(Kh), reverse(Temp,Final),
        (if( (charBlank(Kh);charEol(Kh)), readChars1_var(32,Temp,Final),
            (if( charBlank(Prev), 
                (Temp1 = [32|Temp], readChars1_var(Kh,[Kh|Temp1],Final)),
                (readChars1_var(Kh,[Kh|Temp],Final))
            ))
        ))
    ).


%%''Il CODICE ASCII è .plv !!!
take_meta_var(L,F):- name(F,Lf),append(Lf,[46,112,108,118],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true),assert(buffer([])),
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var,
							  if(clause(buffer(ParsedC),_), 
									(  retractall(buffer(_)), name(Parsed,ParsedC), 
	  									open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var'),nl)
							  ), open_file_head_mul(Nf). %% NF Sarebbe il plv


take_meta_var_fil(L,F):- 
    name(F,Lf),append(Lf,[46,116,120,116],Lft),
    name(Nf,Lft),
    if(file_exists(Nf),delete_file(Nf),true),
    assert(buffer([])),
    last(L,U),
    repeat,
    member(Me,L),
    examine0_var(Me),
    Me==U,!,
    app_residue_var,
    (buffer(ParsedC) ->
        (is_list(ParsedC) ->
            (retractall(buffer(_)), 
             name(Parsed,ParsedC), 
             open(Nf, append, Stream, []), 
             write(Stream, Parsed), 
             close(Stream))
        ;
            fail)
        ;
        fail).



take_meta_var_ple(L,F):- name(F,Lf),append(Lf,[46,112,108,101],Lft),
                        name(Nf,Lft),if(file_exists(Nf),delete_file(Nf),true), assert(buffer([])),
                        last(L,U),repeat,member(Me,L),examine0_var(Me),Me==U,!,app_residue_var,
								if(clause(buffer(ParsedC),_), 
									( retractall(buffer(_)), name(Parsed,ParsedC), 
	 								 open(Nf, append, Stream, []), write(Stream, Parsed), close(Stream)
									), (write('Errore take_meta_var_ple'),nl)
								).

examine0_var(Me):-
    write('DEBUG [examine0_var]: Processando token: '), write(Me), nl,
    if(Me='EOL',
        (write('DEBUG [examine0_var]: Token EOL'), nl, true),
        if(Me='. ',
            (write('DEBUG [examine0_var]: Token punto'), nl, point_write_var,nl_write_var),
            (write('DEBUG [examine0_var]: Chiamata examine_var'), nl, examine_var(Me))
        )
    ).

examine_var(Me):-
    write('DEBUG [examine_var]: Processando token: '), write(Me), nl,
    name(Me,L),
    write('DEBUG [examine_var]: Codice ASCII: '), write(L), nl,
    re_isa_cod_var(L).

re_isa_cod_var(L):-
    write('DEBUG [re_isa_cod_var]: Verifica codice: '), write(L), nl,
    if(isa_code_var(L),
        (write('DEBUG [re_isa_cod_var]: Codice valido'), nl, disapp_variable_var(L)),
        (write('DEBUG [re_isa_cod_var]: Codice non valido'), nl, re_write_var(L))
    ).

isa_code_var(L):-nth0(0,L,El0),nth0(1,L,El1),nth0(2,L,El2),nth0(3,L,El3),El0=118,El1=97,El2=114,El3=95.


disapp_variable_var(L):-append([118,97,114,95],Lt,L),re_write_var(Lt).
re_write_var(L):-
    write('DEBUG [re_write_var]: Chiamata non_aggiungi_var con: '), write(L), nl,
    non_aggiungi_var(L).

%%Scritture
aggiungi_39_var(L):-append([39,39],L,Lf),append(Lf,[39,39],Lf1),
							  clause(buffer(Parsed),_), retractall(buffer(_)),
							  append(Parsed,Lf1, Parola), assert(buffer(Parola)).

non_aggiungi_var(L):-
    write('DEBUG [non_aggiungi_var]: Verifica buffer'), nl,
    clause(buffer(Parsed), _),
    write('DEBUG [non_aggiungi_var]: Buffer trovato, lunghezza: '), length(Parsed,Len), write(Len), nl,
    retractall(buffer(_)),
    write('DEBUG [non_aggiungi_var]: Aggiunta al buffer'), nl,
    append(Parsed,L, Parola),
    write('DEBUG [non_aggiungi_var]: Nuovo buffer, lunghezza: '), length(Parola,Len2), write(Len2), nl,
    assert(buffer(Parola)).

nl_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
						append(Parsed,[10], Parola), assert(buffer(Parola)).
point_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
							append(Parsed,[46], Parola), assert(buffer(Parola)).
%%

app_residue_var:-
    write('DEBUG [app_residue_var]: Inizio verifica residue'), nl,
    if(clause(residue(R), _),
        (write('DEBUG [app_residue_var]: Residue trovato: '), write(R), nl,
         re_isa_cod_var(R),
         write('DEBUG [app_residue_var]: Retract residue'), nl,
         retractall(residue(R))),
        (write('DEBUG [app_residue_var]: Nessun residue trovato'), nl)
    ).
res_write_var(R):-if(R=46,(point_write_var,nl_write_var),(re_write_var(R),nl_write_var)).


%GESTIONE LEARNING

expand_le(F1,Te):- open(F1,read,Stream,[]),
                read(Stream,T),
                expand_term(T,Te),
	        close(Stream).

rewrite_le(Te,F1):-open(F1,write,Stream,[]),
               write(Stream,Te),write(Stream,'.'),nl(Stream),
	        close(Stream).

                     
                     
readFile_var_clause(Infile,Txt) :-			%apertura file,lettura righe
                                            %e chiusura file
	see(Infile), 
	readChars_var(Txt), !,
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





