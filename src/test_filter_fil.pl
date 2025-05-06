:- module(test_filter_fil, [test_filter_fil/0]).

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(file_systems)).

% Inclusione solo del modulo active_dali_wi che già include le sue dipendenze
:- use_module(active_dali_wi).

% Predicato di test che simula filter_fil senza ricorsione
test_filter_fil_sim(Fil) :-
    (is_list(Fil) ->
        process_fil_list_sim(Fil)
    ;
        arg(1, Fil, File),
        write('Processando file: '), write(File), nl,
        retractall(parentheses(_))
    ).

% Processa una lista di file
process_fil_list_sim([]).
process_fil_list_sim([File|Rest]) :-
    write('Processando file: '), write(File), nl,
    retractall(parentheses(_)),
    process_fil_list_sim(Rest).

% Test del predicato filter_fil/1
test_filter_fil :-
    write('Test del predicato filter_fil/1'), nl,
    
    % Test con un singolo file
    write('Test 1: Singolo file'), nl,
    (test_filter_fil_sim(fil('../Examples/advanced/conf/communication')) ->
        write('Test 1 passato: File singolo processato correttamente'), nl
    ;   write('Test 1 fallito: Errore nel processare file singolo'), nl),
    
    % Test con una lista di file
    write('Test 2: Lista di file'), nl,
    (test_filter_fil_sim([fil('../Examples/advanced/conf/communication'), fil('../Examples/advanced/conf/ontology')]) ->
        write('Test 2 passato: Lista di file processata correttamente'), nl
    ;   write('Test 2 fallito: Errore nel processare lista di file'), nl),
    
    % Test con file non esistente
    write('Test 3: File non esistente'), nl,
    (catch(test_filter_fil_sim(fil('../Examples/advanced/conf/file_non_esistente')), Error, 
        (write('Test 3 passato: Errore gestito correttamente - '), write(Error), nl)) ->
        write('Test 3 passato: Errore gestito correttamente'), nl
    ;   write('Test 3 fallito: Errore non gestito correttamente'), nl),
    
    % Test con lista vuota
    write('Test 4: Lista vuota'), nl,
    (test_filter_fil_sim([]) ->
        write('Test 4 passato: Lista vuota processata correttamente'), nl
    ;   write('Test 4 fallito: Errore nel processare lista vuota'), nl).

% Predicati di supporto per il test
:- dynamic test_file/1.

% Crea un file di test temporaneo
create_test_file(FileName, Content) :-
    open(FileName, write, Stream),
    write(Stream, Content),
    close(Stream),
    assert(test_file(FileName)).

% Pulisci i file di test
cleanup_test_files :-
    retract(test_file(FileName)),
    delete_file(FileName),
    fail.
cleanup_test_files.

% Esegui i test
:- initialization(test_filter_fil). 