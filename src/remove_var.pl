% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- module(remove_var, [remove_variable/1, 
                       remove_file_var/1, 
                       remove_var_clause/2, 
                       remove_var_ple/1, 
                       examine0_var/1]).

:- use_module(library(file_systems)).
:- use_module(tokefun, [leggiChars/1, tokenize/2]).

remove_variable(F):-readFile_var(F,Fi),tokenize(Fi,L),take_meta_var(L,F).

remove_file_var(F):-
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
         leggiChars(Txt),
         seen),
        _,
        fail
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
take_meta_var(L,F):- 
    name(F,Lf),
    append(Lf,[46,112,108,118],Lft),
    name(Nf,Lft),
    (file_exists(Nf) -> delete_file(Nf) ; true),
    assert(buffer([])),
    last(L,U),
    repeat,
    member(Me,L),
    examine0_var(Me),
    Me==U,!,
    app_residue_var,
    (clause(buffer(ParsedC),_) -> 
        (retractall(buffer(_)), 
         name(Parsed,ParsedC),
         safe_write_to_file(Nf, Parsed))
    ; 
        (write('Errore take_meta_var'),nl)
    ),
    open_file_head_mul(Nf).


take_meta_var_fil(L,F):- 
    name(F,Lf),
    append(Lf,[46,116,120,116],Lft),
    name(Nf,Lft),
    (file_exists(Nf) -> delete_file(Nf) ; true),
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
             safe_write_to_file(Nf, Parsed))
        ;
            fail)
    ;
        fail).



take_meta_var_ple(L,F):- 
    name(F,Lf),
    append(Lf,[46,112,108,101],Lft),
    name(Nf,Lft),
    (file_exists(Nf) -> delete_file(Nf) ; true),
    assert(buffer([])),
    last(L,U),
    repeat,
    member(Me,L),
    examine0_var(Me),
    Me==U,!,
    app_residue_var,
    (clause(buffer(ParsedC),_) -> 
        (retractall(buffer(_)), 
         name(Parsed,ParsedC),
         safe_write_to_file(Nf, Parsed))
    ; 
        (write('Errore take_meta_var_ple'),nl)
    ).

examine0_var(Me):-
    if(Me='EOL',
        true,
        if(Me='. ',
            (point_write_var,nl_write_var),
            examine_var(Me))
    ).

examine_var(Me):-
    name(Me,L),
    re_isa_cod_var(L).

re_isa_cod_var(L):-
    if(isa_code_var(L),
        disapp_variable_var(L),
        re_write_var(L)
    ).

isa_code_var(L):-nth0(0,L,El0),nth0(1,L,El1),nth0(2,L,El2),nth0(3,L,El3),El0=118,El1=97,El2=114,El3=95.


disapp_variable_var(L):-append([118,97,114,95],Lt,L),re_write_var(Lt).
re_write_var(L):-
    non_aggiungi_var(L).

%%Scritture
aggiungi_39_var(L):-append([39,39],L,Lf),append(Lf,[39,39],Lf1),
							  clause(buffer(Parsed),_), retractall(buffer(_)),
							  append(Parsed,Lf1, Parola), assert(buffer(Parola)).

non_aggiungi_var(L):-
    clause(buffer(Parsed), _),
    retractall(buffer(_)),
    append(Parsed,L, Parola),
    assert(buffer(Parola)).

nl_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
						append(Parsed,[10], Parola), assert(buffer(Parola)).
point_write_var:-clause(buffer(Parsed),_), retractall(buffer(_)),
							append(Parsed,[46], Parola), assert(buffer(Parola)).
%%

app_residue_var:-
    if(clause(residue(R), _),
        (re_isa_cod_var(R),
         retractall(residue(R))),
        true
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


% Funzione di utilità per scrivere su file in modo sicuro
safe_write_to_file(File, Content) :-
    catch(
        (open(File, append, Stream, []),
         write(Stream, Content),
         close(Stream)),
        Error,
        (write('ERROR: Impossibile scrivere su file: '), write(File), write(' - '), write(Error), nl, fail)
    ).





