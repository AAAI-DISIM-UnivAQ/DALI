% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

:- module(active_dali_wi, [
    initialize_agent/1,
    initialize_agent_parameters/1,
    run_agent/0,
    internal_event/0,
    process_high_events/0,
    process_normal_events/0,
    manage_goals/0,
    manage_time/0,
    check_conditions/0
]).

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
initialize_agent_parameters(agent(File,AgentName,Ontolog,Lang,Fil,Lib,UP,DO,Specialization)) :-
    trace_message('Inizializzazione parametri agente'),
    trace_message(['File agente: ', File]),
    trace_message(['Nome agente: ', AgentName]),
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
    (UP = no -> true ; assertz(user_profile_location(UP))),
    (DO = no -> true ; assertz(dali_onto_location(DO))),
    assertz(server_obj('localhost':3010)),
    trace_message('Inizio processamento file di configurazione'),
    catch(
        filter_fil(Fil),
        Error,
        (trace_message('Errore nel processamento dei file di configurazione'), write(Error), nl, fail)
    ),
    trace_message('Fine processamento file di configurazione'),
    assertz(specialization(Specialization)),
    (Ontolog = no -> true ; load_ontology_file(Ontolog, AgentName)),
    assertz(own_language(Lang)),
    trace_linda('Tentativo di connessione al server Linda'),
    catch(
        linda_client('localhost':3010),
        Error,
        (trace_linda('Errore di connessione al server Linda'), write(Error), nl, fail)
    ),
    trace_linda('Connessione al server Linda stabilita'),
    trace_linda('Invio messaggio di attivazione agente'),
    catch(
        out(activating_agent(AgentName)),
        Error,
        (trace_linda('Errore nell\'invio del messaggio di attivazione'), write(Error), nl, fail)
    ),
    trace_linda('Messaggio di attivazione inviato con successo'),
    trace_message('Inizio inizializzazione file agente'),
    delete_agent_files(File),
    trace_message('File agente eliminati'),
    token(File),
    trace_message('Tokenizzazione completata'),
    start1(File, AgentName, Lib, Fil),
    trace_message('Inizializzazione completata'),
    trace_message('Creazione clausola dali_agent_/4'),
    atom_concat('work/', AgentName, AgentFile),
    open(AgentFile, read, Stream),
    assertz(dali_agent_(AgentName,1,Stream,[])),
    trace_message(['Clausola dali_agent_/4 creata: ', dali_agent_(AgentName,1,Stream,[])]),
    trace_message('Avvio ciclo agente'),
    catch(
        run_agent,
        Error,
        (trace_message(['Errore nell\'avvio del ciclo agente: ', Error]), fail)
    ).

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

% Predicato per l'inizializzazione dei file dell'agente
start1(File, AgentName, Lib, Fil) :-
    trace_message(['Inizio start1 per agente: ', AgentName]),
    % Assicurati che la directory work esista nella posizione corretta
    atom_concat('./', File, FullPath),
    trace_message(['Percorso completo: ', FullPath]),
    % Crea il file dell'agente
    open(FullPath, write, Stream),
    write(Stream, ':- module('), write(Stream, AgentName), write(Stream, ', []).'), nl(Stream),
    write(Stream, ':- use_module(library(lists)).'), nl(Stream),
    % Aggiungi le librerie
    write_libraries(Stream, Lib),
    close(Stream),
    trace_message('File agente creato con successo').

% Predicato per scrivere le librerie nel file
write_libraries(_, []).
write_libraries(Stream, [Lib|Rest]) :-
    write(Stream, ':- use_module('), write(Stream, Lib), write(Stream, ').'), nl(Stream),
    write_libraries(Stream, Rest).

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
    assert_this(ext_agent(AgM,IndM,Ontology,Language)),
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

% Gestione degli eventi ad alta priorita'
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
        no_process_high_event(Ag,E,T)).

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
    trace_message('Inizio controllo eventi interni'),
    clause(dali_agent_(_,_,S,_), _),
    trace_message(['Stream per eventi interni: ', S]),
    trace_message('Lettura linea dal file'),
    read_line_from_file(S, 2),
    trace_message('Linea letta'),
    clause(evintI(L), _),
    trace_message(['Eventi interni trovati: ', L]),
    (L \= [] -> 
        trace_message('Processamento eventi interni'),
        check_internal_event1(L, S)
    ; 
        trace_message('Nessun evento interno trovato')
    ).

% Gestione degli eventi interni
check_internal_event1([], _).
check_internal_event1([Event|Rest], S) :-
    trace_event(['Processamento evento interno: ', Event]),
    process_internal_event(Event, S),
    check_internal_event1(Rest, S).

% Processa un singolo evento interno
process_internal_event(Event, S) :-
    trace_event(['Elaborazione evento: ', Event]),
    (clause(event_condition(Event, Condition), _) ->
        (call(Condition) -> 
            trace_event('Condizione evento soddisfatta'),
            process_event_action(Event) ; 
            trace_event('Condizione evento non soddisfatta')) ;
        process_event_action(Event)).

% Processa l'azione di un evento
process_event_action(Event) :-
    trace_event(['Esecuzione azione per evento: ', Event]),
    (clause(event_action(Event, Action), _) ->
        call(Action) ;
        trace_event('Nessuna azione definita per l\'evento')).

% Gestione degli obiettivi
manage_goals :-
    trace_message('Inizio gestione obiettivi'),
    (clause(goal(_), _) -> 
        trace_message('Obiettivi trovati'),
        manage_goals1 ; 
        trace_message('Nessun obiettivo')),
    trace_message('Fine gestione obiettivi').

manage_goals1 :-
    trace_message('Ricerca obiettivi'),
    findall(goal(G), clause(goal(G), _), L),
    trace_message(['Lista obiettivi trovati: ', L]),
    last(L, U),
    trace_message(['Ultimo obiettivo: ', U]),
    process_goals(L, U).

process_goals([], _) :-
    trace_message('Nessun obiettivo da processare').
process_goals([Me|Rest], U) :-
    trace_message(['Processamento obiettivo: ', Me]),
    (clause(goal_completed(Me), _) -> 
        trace_message('Obiettivo gia completato') ; 
        (trace_message('Obiettivo non completato, gestione...'),
         manage_goals2(Me))),
    (Me = U -> 
        trace_message('Ultimo obiettivo raggiunto') ; 
        (trace_message('Passaggio al prossimo obiettivo'),
         process_goals(Rest, U))).

manage_goals2(Me) :-
    trace_message(['Gestione obiettivo: ', Me]),
    (clause(goal_precondition(Me,Pre), _) ->
        (trace_message('Verifica precondizioni'),
         (Pre -> 
             trace_message('Precondizioni soddisfatte'),
             manage_goals3(Me) ; 
             trace_message('Precondizioni non soddisfatte'))) ;
        manage_goals3(Me)).

manage_goals3(Me) :-
    trace_message(['Esecuzione azioni obiettivo: ', Me]),
    (clause(goal_do(Me,Do), _) ->
        (trace_message('Esecuzione azioni'),
         (Do -> 
             trace_message('Azioni completate'),
             manage_goals4(Me) ; 
             trace_message('Azioni non completate'))) ;
        manage_goals4(Me)).

manage_goals4(Me) :-
    trace_message(['Verifica postcondizioni obiettivo: ', Me]),
    (clause(goal_postcondition(Me,Post), _) ->
        (trace_message('Verifica postcondizioni'),
         (Post -> 
             trace_message('Postcondizioni soddisfatte'),
             manage_goals5(Me) ; 
             trace_message('Postcondizioni non soddisfatte'))) ;
        manage_goals5(Me)).

manage_goals5(Me) :-
    trace_message(['Completamento obiettivo: ', Me]),
    assert_this(goal_completed(Me)),
    trace_message('Obiettivo completato').

% Gestione del tempo
manage_time :-
    trace_event('Controllo tempo'),
    (clause(time(_), _) -> 
        trace_event('Eventi temporali trovati'),
        manage_time1 ; 
        trace_event('Nessun evento temporale')),
    trace_event('Fine controllo tempo').

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
    trace_event('Controllo condizioni'),
    (clause(condition(_), _) -> 
        trace_event('Condizioni trovate'),
        check_conditions1 ; 
        trace_event('Nessuna condizione')),
    trace_event('Fine controllo condizioni').

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
    trace_message(['Apertura file: ', File, ' per leggere linea: ', Line]),
    safe_open_file(File, read, Stream),
    trace_message('File aperto con successo'),
    read_lines_from_stream(Stream, Line, 1),
    trace_message('Linee lette con successo'),
    safe_close_stream(Stream),
    trace_message('File chiuso').

read_lines_from_stream(Stream, Line, Current) :-
    trace_message(['Lettura linea corrente: ', Current, ' di ', Line]),
    (Current = Line ->
        read(Stream, Term),
        trace_message(['Termine letto: ', Term]),
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
    trace_message(['Tentativo apertura file: ', File, ' in modalita: ', Mode]),
    catch(open(File, Mode, Stream),
          error(Error, _),
          (trace_message(['Errore apertura file: ', Error]), fail)),
    trace_message('File aperto con successo').

safe_close_stream(Stream) :-
    trace_message('Tentativo chiusura stream'),
    catch(close(Stream),
          error(Error, _),
          (trace_message(['Errore chiusura stream: ', Error]))),
    trace_message('Stream chiuso con successo').

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

% Ciclo principale dell'agente
run_agent :-
    trace_message('Inizio ciclo agente'),
    % 1. Gestione eventi esterni (alta priorita')
    trace_message('Fase 1: Gestione eventi alta priorita'),
    process_high_events,
    trace_message('Fine fase 1'),
    
    % 2. Gestione eventi interni
    trace_message('Fase 2: Gestione eventi interni'),
    internal_event,
    trace_message('Fine fase 2'),
    
    % 3. Gestione obiettivi
    trace_message('Fase 3: Gestione obiettivi'),
    manage_goals,
    trace_message('Fine fase 3'),
    
    % 4. Gestione tempo
    trace_message('Fase 4: Gestione tempo'),
    manage_time,
    trace_message('Fine fase 4'),
    
    % 5. Gestione condizioni
    trace_message('Fase 5: Gestione condizioni'),
    check_conditions,
    trace_message('Fine fase 5'),
    
    % Attendi un breve periodo prima del prossimo ciclo
    trace_message('Attesa prima del prossimo ciclo'),
    sleep(1),
    % Continua il ciclo
    trace_message('Riavvio ciclo'),
    run_agent.

% ... existing code ...
