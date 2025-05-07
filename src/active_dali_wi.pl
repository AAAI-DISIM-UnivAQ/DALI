% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- module(active_dali_wi, [initialize_agent/1,initialize_agent_parameters/1]).

:- use_module(library(file_systems), [file_exists/1,delete_file/1, make_directory/1]).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library('linda/client')).
:- use_module(library(clpq)).
:- use_module(library(fdbg)).
:- use_module(remove_var, [remove_var_fil/1]).
:- use_module(utils, [delete_agent_files/1]).
:- use_module(tokefun, [leggiFile/2, token_fil/1]).

%%DEBUG
%%:-leash([exception]).
%%

:- multifile user:term_expansion/6.
:- set_prolog_flag(discontiguous_warnings,off),
   set_prolog_flag(single_var_warnings,off).

%%
:- use_module('communication_onto').
:- use_module('substitute').
:-dynamic prefixes/1.
:-dynamic repositories/1.
:-dynamic ontology/3.
%%

:- use_module('tokefun').

:- use_module('meta1').
:- use_module('remove_var').  % translated from 'togli_var.pl'
:- use_module('memory').

:- use_module('examine_past_constraints').

:-dynamic tesg/1.
:-dynamic ontology/2.
:-dynamic en/1.

:-dynamic told/6.
:-dynamic export_past/1.
:-dynamic export_past_do/1.
:-dynamic export_past_not_do/1.

:-dynamic deltat/1, deltatime/1, simultaneity_interval/1, wishlist/1, tstart/1, mem_current/1, mem_no_dup/1, mem_past/1, check_list/1.
:-dynamic user_profile_location/1,dali_onto_location/1,server_obj/1, specialization/1,own_language/1, agent_x/4, agent_x/1, time_charge/1, to_add/1.
:-dynamic rule_base/1, mul/1,no_check/0,past/3,tep/2,fact_mul/2,azi/1,even/1,evin/1,continue_mul_f/1,evN/1.

:-op(500,xfy,:>).
:-op(500,xfy,:<).
:-op(10,xfy,~/).
:-op(1200,xfy,</).
:-op(200,xfy,?/).

:-op(1200,xfx,[:-,:>]).
:-op(1200,xfx,[:-,:<]).
:-op(1200,xfx,[:-,~/]).
:-op(1200,xfx,[:-,</]).
:-op(1200,xfx,[:-,?/]).

:- use_module('read_mul').  % translated from 'leggi_mul.pl'

% Predicati di trace
trace_message(Message) :-
    write('DEBUG [Message]: '), write(Message), nl.

trace_event(Event) :-
    write('DEBUG [Event]: '), write(Event), nl.

trace_condition(Condition) :-
    write('DEBUG [Condition]: '), write(Condition), nl.

trace_action(Action) :-
    write('DEBUG [Action]: '), write(Action), nl.

trace_linda(Message) :-
    write('DEBUG [Linda]: '), write(Message), nl.

% Lettura di un carattere dall'input corrente
get0(Char) :-
    current_input(Stream),
    (at_end_of_stream(Stream) ->
        Char = end_of_file
    ;
        get_char(Stream, Char)
    ).

% Predicato privato per l'inizializzazione dei parametri dell'agente
initialize_agent_parameters(agent(File, AgentName, Ontolog, Lang, Fil, Lib, UP, DO, Specialization)) :-
    trace_message('Inizializzazione parametri agente'),
    trace_message(['File agente: ', File]),
    trace_message(['Nome agente: ', AgentName]),
    
    % Assicurati che la directory work esista
    (file_exists('work') -> 
        trace_message('Directory work esistente')
    ; 
        trace_message('Creazione directory work'),
        catch(
            make_directory('work'),
            error(SPIO_E_FILE_EXISTS, _),
            trace_message('Directory work esistente')
        )
    ),
    
    % Inizializza le configurazioni dell'agente
    (UP = no -> true ; assert(user_profile_location(UP))),
    (DO = no -> true ; assert(dali_onto_location(DO))),
    assert(server_obj('localhost':3010)),
    
    % Aggiungo tracciamento per filter_fil
    trace_message('Inizio processamento file di configurazione'),
    catch(
        filter_fil(Fil),
        Error,
        (trace_message('Errore nel processamento dei file di configurazione'), write(Error), nl, fail)
    ),
    trace_message('Fine processamento file di configurazione'),
    
    assert(specialization(Specialization)),
    (Ontolog = no -> true ; load_ontology_file(Ontolog, AgentName)),
    assert(own_language(Lang)),
    
    % Connessione al server LINDA
    trace_linda('Tentativo di connessione al server Linda'),
    catch(
        linda_client('localhost':3010),
        Error,
        (trace_linda('Errore di connessione al server Linda'), write(Error), nl, fail)
    ),
    trace_linda('Connessione al server Linda stabilita'),
    
    trace_linda('Invio messaggio di attivazione agente'),
    catch(
        out(activating_agent_x(AgentName)),
        Error,
        (trace_linda('Errore nell\'invio del messaggio di attivazione'), write(Error), nl, fail)
    ),
    trace_linda('Messaggio di attivazione inviato con successo'),
    
    % Inizializzazione dei file dell'agente
    trace_message('Inizio inizializzazione file agente'),
    delete_agent_files(File),
    trace_message('File agente eliminati'),
    token(File),
    trace_message('Tokenizzazione completata'),
    start1(File, AgentName, Lib, Fil),
    trace_message('Inizializzazione completata').

% Inizializzazione dell'agente
initialize_agent(FI) :-
    trace_message('Inizio inizializzazione agente'),
    % Leggi il file di configurazione
    (is_list(FI) -> 
        atom_codes(Atom, FI),
        open(Atom, read, Stream)
    ; 
        open(FI, read, Stream)
    ),
    read(Stream, Term),
    close(Stream),
    write('my config: '), write(Term), nl,
    initialize_agent_parameters(Term).


% Wrapper per token_fil che usa call/1
token_fil_wrapper(File) :-
    trace_message(['Chiamata token_fil_wrapper su file: ', File]),
    catch(
        call(token_fil(File)),
        Error,
        (trace_message(['Errore in token_fil_wrapper: ', Error]), fail)
    ).

% Filtra i file di configurazione
filter_fil(Fil) :-
    trace_message('Inizio filter_fil'),
    trace_message(['File da processare: ', Fil]),
    (is_list(Fil) ->
        trace_message('Processamento lista di file'),
        process_fil_list(Fil)
    ;
        trace_message('Processamento singolo file'),
        trace_message(['File da processare: ', Fil]),
        trace_message('Chiamata token_fil'),
        catch(
            token_fil_wrapper(Fil),
            Error,
            (trace_message(['Errore in token_fil: ', Error]), fail)
        ),
        trace_message('Fine token_fil'),
        retractall(parentheses(_)),
        trace_message('Chiamata remove_var_fil'),
        catch(
            remove_var_fil(Fil),
            Error,
            (trace_message(['Errore in remove_var_fil: ', Error]), fail)
        ),
        trace_message('Fine remove_var_fil')
    ),
    trace_message('Fine filter_fil').

% Processa una lista di file
process_fil_list([]) :-
    trace_message('Lista file vuota').
process_fil_list([File|Rest]) :-
    trace_message(['Processamento file: ', File]),
    trace_message('Chiamata token_fil'),
    catch(
        token_fil_wrapper(File),
        Error,
        (trace_message(['Errore in token_fil: ', Error]), fail)
    ),
    trace_message('Fine token_fil'),
    retractall(parentheses(_)),
    trace_message('Chiamata remove_var_fil'),
    catch(
        remove_var_fil(File),
        Error,
        (trace_message(['Errore in remove_var_fil: ', Error]), fail)
    ),
    trace_message('Fine remove_var_fil'),
    process_fil_list(Rest).

% Carica il file di ontologia
load_ontology_file(Ontolog, Agent) :-
    open(Ontolog, read, Stream, []),
    write('Ontologia: '), write(Ontolog), nl,
    read(Stream, PrefixesC),
    write('Prefixes: '), write(PrefixesC), nl,
    read(Stream, RepositoryC),
    write('Repository: '), write(RepositoryC), nl,
    read(Stream, HostC),
    write('Host: '), write(HostC), nl,
                 close(Stream),
    name(Repository, RepositoryC),
    name(Prefixes, PrefixesC),
    name(Host, HostC),
    assert(ontology(Prefixes, [Repository, Host], Agent)).

% Gestione dei messaggi
receive_message :-
    trace_message('Controllo nuovi messaggi'),
    clause(agent_x(Ag,Ind,_,_), _),
    trace_linda('Tentativo di lettura messaggio dal server Linda'),
    catch(
        (rd_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)) ->
            trace_message('Messaggio ricevuto'),
            receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con) ; 
            trace_message('Nessun messaggio disponibile')),
        Error,
        (trace_linda('Errore nella lettura del messaggio'), write(Error), nl, fail)
    ).

receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con) :-
    trace_message('Elaborazione messaggio'),
    assert_this(ext_agent_x(AgM,IndM,Ontology,Language)),
    trace_linda('Rimozione messaggio dalla tupla space'),
    catch(
          in_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
        Error,
        (trace_linda('Errore nella rimozione del messaggio'), write(Error), nl, fail)
    ),
    (clause(receive(Con), _) ->
        trace_message('Messaggio ricevibile'),
        call_con(AgM,IndM,Language,Ontology,Con) ;
        trace_message('Messaggio non ricevibile'),
        not_receivable_meta(AgM,IndM,Language,Ontology,Con)).

% Gestione degli eventi
process_events :-
    trace_event('Processamento eventi'),
    findall(Event, clause(event(Event), _), Events),
    process_event_list(Events).

process_event_list([]).
process_event_list([Event|Rest]) :-
    trace_event('Elaborazione evento'),
    process_single_event(Event),
    process_event_list(Rest).

process_single_event(Event) :-
    trace_event('Controllo condizioni evento'),
    (clause(event_condition(Event, Condition), _) ->
        (call(Condition) -> 
            trace_condition('Condizione soddisfatta'),
            process_event_action(Event) ; 
            trace_condition('Condizione non soddisfatta')) ;
        process_event_action(Event)).

process_event_action(Event) :-
    trace_action('Esecuzione azione evento'),
    (clause(event_action(Event, Action), _) ->
        call(Action) ;
        trace_action('Nessuna azione definita')).

% Gestione degli eventi esterni
process_external_events :-
    trace_event('Processamento eventi esterni'),
    clause(agent_x(_,_,S,_), _),
    read_line_from_file(S,1),
    clause(eventE(Es), _),
    (Es = [] -> 
        trace_event('Nessun evento esterno') ; 
        (process_high_events, process_normal_events)).

% Gestione degli eventi ad alta priorità
process_high_events :-
    trace_event('Controllo eventi alta priorita'),
    (clause(ev_high(_,_,_), _) -> 
        trace_event('Eventi alta priorita trovati'),
        process_high_events1 ; 
        trace_event('Nessun evento alta priorita')).

process_high_events1 :-
    findall(ev_high(AgM,E,T), clause(ev_high(AgM,E,T), _), L),
    last(L, ev_high(Ag,E,T)),
    (once(eve_cond(E)) -> 
        trace_event('Condizione evento alta priorita soddisfatta'),
        process_high_event(Ag,E,T) ; 
        trace_event('Condizione evento alta priorita non soddisfatta'),
        no_process_high_event(Ag,E,T)),
    Ag = Ag, E = E, T = T.

% Gestione degli eventi normali
process_normal_events :-
    trace_event('Controllo eventi normali'),
    (clause(ev_normal(_,_,_), _) -> 
        trace_event('Eventi normali trovati'),
        process_normal_events1 ; 
        trace_event('Nessun evento normale')).

process_normal_events1 :-
    clause(ev_normal(AgM,E,T), _),
    (once(eve_cond(E)) -> 
        trace_event('Condizione evento normale soddisfatta'),
        process_normal_event(AgM,E,T) ; 
        trace_event('Condizione evento normale non soddisfatta'),
        no_process_normal_event(AgM,E,T)).

% Gestione degli eventi interni
internal_event :- 
    trace_event('Controllo eventi interni'),
    clause(agent_x(_,_,S,_), _),
    read_line_from_file(S,2),
    clause(evintI(L), _),
    (L \= [] -> 
        trace_event('Eventi interni trovati'),
        check_internal_event1(L,S) ; 
        trace_event('Nessun evento interno')).

% Gestione degli obiettivi
manage_goals :-
    (clause(goal(_), _) -> manage_goals1 ; true).

manage_goals1 :-
    findall(goal(G), clause(goal(G), _), L),
    last(L, U),
    process_goals(L, U).

process_goals([], _).
process_goals([Me|Rest], U) :-
    (clause(goal_completed(Me), _) -> true ; manage_goals2(Me)),
    (Me = U -> true ; process_goals(Rest, U)).

% Gestione del tempo
manage_time :-
    (clause(time(_), _) -> manage_time1 ; true).

manage_time1 :-
    findall(time(T), clause(time(T), _), L),
    last(L, U),
    process_times(L, U).

process_times([], _).
process_times([Me|Rest], U) :-
    (clause(time_completed(Me), _) -> true ; manage_time2(Me)),
    (Me = U -> true ; process_times(Rest, U)).

% Gestione delle condizioni
check_conditions :-
    (clause(condition(_), _) -> check_conditions1 ; true).

check_conditions1 :-
    findall(condition(C), clause(condition(C), _), L),
    last(L, U),
    process_conditions(L, U).

process_conditions([], _).
process_conditions([Me|Rest], U) :-
    (clause(condition_completed(Me), _) -> true ; check_conditions2(Me)),
    (Me = U -> true ; process_conditions(Rest, U)).

% Funzioni di supporto per la lettura dei file
read_line_from_file(File, Line) :-
    safe_open_file(File, read, Stream),
    read_lines_from_stream(Stream, Line, 1),
    safe_close_stream(Stream).

read_lines_from_stream(Stream, Line, Current) :-
    (Current = Line ->
        read(Stream, Term),
        assert(Term) ;
        read(Stream, _),
        Next is Current + 1,
        read_lines_from_stream(Stream, Line, Next)).

% Gestione delle condizioni
check_conditions2(Me) :-
    (clause(condition_precondition(Me,Pre), _) ->
        check_conditions3(Me,Pre) ;
        check_conditions4(Me)).

check_conditions3(Me,Pre) :-
    (Pre ->
        check_conditions4(Me) ;
        true).

check_conditions4(Me) :-
    (clause(condition_do(Me,Do), _) ->
        check_conditions5(Me,Do) ;
        true).

check_conditions5(Me,Do) :-
    (Do ->
        check_conditions6(Me) ;
        true).

check_conditions6(Me) :-
    (clause(condition_postcondition(Me,Post), _) ->
        check_conditions7(Me,Post) ;
        check_conditions8(Me)).

check_conditions7(Me,Post) :-
    (Post ->
        check_conditions8(Me) ;
        true).

check_conditions8(Me) :-
    assert_this(condition_completed(Me)).

cancel_condition(Me) :-
    retractall(condition(Me)),
    retractall(condition_precondition(Me,_)),
    retractall(condition_do(Me,_)),
    retractall(condition_postcondition(Me,_)),
    retractall(condition_completed(Me)).

% Gestione degli stream
safe_open_file(File, Mode, Stream) :-
    catch(open(File, Mode, Stream),
          error(Error, _),
          (write('Error opening file: '), write(Error), nl, fail)).

safe_close_stream(Stream) :-
    catch(close(Stream),
          error(Error, _),
          (write('Error closing stream: '), write(Error), nl)).

% Gestione dei file
process_file(File) :-
    safe_open_file(File, read, Stream),
    process_stream(Stream),
    safe_close_stream(Stream).

process_stream(Stream) :-
    read(Stream, Term),
    (Term = end_of_file -> true ;
        process_term(Term),
        process_stream(Stream)).

process_term(Term) :-
    (clause(process_term(Term), _) ->
        call(process_term(Term)) ;
        assert(Term)).

% Gestione dei cicli
process_loop(Goal) :-
    call(Goal),
    process_loop(Goal).

process_loop_until(Goal, Condition) :-
    call(Goal),
    (call(Condition) -> true ; process_loop_until(Goal, Condition)).

% Gestione delle liste
process_list([], _).
process_list([H|T], Pred) :-
    call(Pred, H),
    process_list(T, Pred).

% ... existing code ...
