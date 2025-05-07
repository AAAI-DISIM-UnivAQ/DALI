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
    check_conditions/0,
    start1/4,
    write_libraries/2,
    token_fil_wrapper/1,
    filter_fil/1,
    process_fil_list/1,
    load_ontology_file/2,
    receive_message/0,
    receive_message0/7,
    process_events/0,
    process_event_list/1,
    process_single_event/1,
    process_event_action/1,
    process_external_events/0,
    process_high_events1/0,
    process_normal_events1/0,
    check_internal_event1/2,
    process_internal_event/2,
    manage_goals1/0,
    process_goals/2,
    manage_goals2/1,
    manage_goals3/1,
    manage_goals4/1,
    manage_goals5/1,
    manage_time1/0,
    process_times/2,
    check_conditions1/0,
    process_conditions/2,
    read_line_from_file/2,
    read_lines_from_stream/3,
    check_conditions2/1,
    check_conditions3/2,
    check_conditions4/1,
    check_conditions5/2,
    check_conditions6/1,
    check_conditions7/2,
    check_conditions8/1,
    cancel_condition/1,
    safe_open_file/3,
    safe_close_stream/1,
    process_file/1,
    process_stream/1,
    process_term/1,
    process_loop/1,
    process_loop_until/2,
    process_list/2,
    trace_message/1
]).

% Predicato per il tracciamento dei messaggi
trace_message(Message) :-
    (is_list(Message) ->
        maplist(write, Message)
    ;
        write(Message)
    ),
    nl.

:- use_module(library(file_systems), [file_exists/1,delete_file/1, make_directory/1]).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library('linda/client')).
:- use_module(library(clpq)).
:- use_module(library(fdbg)).

% Caricamento esplicito dei moduli con debug
:- trace_message('Caricamento modulo tokefun...'),
   use_module(tokefun),
   trace_message('Modulo tokefun caricato').

:- trace_message('Caricamento modulo remove_var...'),
   use_module(remove_var),
   trace_message('Modulo remove_var caricato'),
   trace_message('Verifica predicati remove_var...'),
   (predicate_property(remove_var:remove_file_var(_), defined) ->
       trace_message('Predicato remove_file_var/1 trovato nel modulo remove_var')
   ;
       trace_message('ERROR: Predicato remove_file_var/1 non trovato nel modulo remove_var')
   ).

:- use_module(utils, [delete_agent_files/1]).

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
    write('DEBUG: Inizializzazione parametri:'), nl,
    write('DEBUG:   - File: '), write(File), nl,
    write('DEBUG:   - Nome agente: '), write(AgentName), nl,
    write('DEBUG:   - Ontologia: '), write(Ontolog), nl,
    write('DEBUG:   - Lingua: '), write(Lang), nl,
    write('DEBUG:   - File configurazione: '), write(Fil), nl,
    write('DEBUG:   - Librerie: '), write(Lib), nl,
    
    % Gestione directory work con controlli robusti
    write('DEBUG: Controllo directory work...'), nl,
    catch(
        (file_exists('work') -> 
            (write('DEBUG: Directory work esistente'), nl,
             true) 
        ; 
            (write('DEBUG: Creazione directory work...'), nl,
             make_directory('work'),
             write('DEBUG: Directory work creata'), nl)
        ),
        error(SPIO_E_FILE_EXISTS, _),
        (write('DEBUG: Directory work gia esistente (gestito)'), nl,
         true)
    ),
    
    % Controllo che la directory sia accessibile
    write('DEBUG: Verifica accessibilita directory work...'), nl,
    catch(
        (open('work/.test', write, TestStream),
         close(TestStream),
         delete_file('work/.test'),
         write('DEBUG: Directory work accessibile'), nl),
        Error,
        (write('ERROR: Directory work non accessibile: '), write(Error), nl, fail)
    ),
    
    % Resto del codice
    write('DEBUG: Configurazione parametri aggiuntivi...'), nl,
    (UP = no -> 
        write('DEBUG: Nessun user profile specificato'), nl
    ; 
        (write('DEBUG: Impostazione user profile: '), write(UP), nl,
         assertz(user_profile_location(UP)))
    ),
    
    (DO = no -> 
        write('DEBUG: Nessuna ontologia DALI specificata'), nl
    ; 
        (write('DEBUG: Impostazione ontologia DALI: '), write(DO), nl,
         assertz(dali_onto_location(DO)))
    ),
    
    write('DEBUG: Impostazione server locale...'), nl,
    assertz(server_obj('localhost':3010)),
    
    write('DEBUG: Filtro file di configurazione...'), nl,
    catch(
        filter_fil(Fil),
        Error,
        (write('ERROR: Errore nel filtraggio file: '), write(Error), nl, fail)
    ),
    
    write('DEBUG: Impostazione specializzazione...'), nl,
    assertz(specialization(Specialization)),
    
    (Ontolog = no -> 
        write('DEBUG: Nessuna ontologia da caricare'), nl
    ; 
        (write('DEBUG: Caricamento ontologia: '), write(Ontolog), nl,
         load_ontology_file(Ontolog, AgentName))
    ),
    
    write('DEBUG: Impostazione lingua...'), nl,
    assertz(own_language(Lang)),
    
    write('DEBUG: Connessione a Linda...'), nl,
    catch(
        linda_client('localhost':3010),
        Error,
        (write('ERROR: Errore connessione Linda: '), write(Error), nl, fail)
    ),
    
    write('DEBUG: Attivazione agente...'), nl,
    catch(
        out(activating_agent(AgentName)),
        Error,
        (write('ERROR: Errore attivazione agente: '), write(Error), nl, fail)
    ),
    
    write('DEBUG: Pulizia file agente...'), nl,
    delete_agent_files(File),
    
    write('DEBUG: Tokenizzazione file...'), nl,
    token(File),
    
    write('DEBUG: Inizializzazione agente...'), nl,
    start1(File, AgentName, Lib, Fil),
    
    % Controllo che la directory work sia ancora accessibile prima di creare il file dell'agente
    write('DEBUG: Creazione file agente...'), nl,
    catch(
        (atom_concat('work/', AgentName, AgentFile),
         write('DEBUG: File agente: '), write(AgentFile), nl,
         open(AgentFile, write, Stream),
         close(Stream),
         open(AgentFile, read, Stream),
         assertz(dali_agent_(AgentName,1,Stream,[]))),
        Error,
        (write('ERROR: Impossibile creare/accedere al file agente in work/: '), write(Error), nl, fail)
    ),
    
    write('DEBUG: Avvio agente...'), nl,
    catch(
        run_agent,
        Error,
        (write('ERROR: Errore avvio agente: '), write(Error), nl, fail)
    ).

% Inizializzazione dell'agente
initialize_agent(FI) :-
    write('DEBUG: =========================================='), nl,
    write('DEBUG: INIZIO INIZIALIZZAZIONE AGENTE'), nl,
    write('DEBUG: =========================================='), nl,
    
    % Leggi il file di configurazione
    write('DEBUG: Lettura file di configurazione...'), nl,
    (is_list(FI) -> 
        (write('DEBUG: Input come lista di codici, conversione...'), nl,
         atom_codes(Atom, FI),
         write('DEBUG: File da aprire: '), write(Atom), nl,
         open(Atom, read, Stream))
    ; 
        (write('DEBUG: Input come atomo, file da aprire: '), write(FI), nl,
         open(FI, read, Stream))
    ),
    write('DEBUG: Lettura termine di configurazione...'), nl,
    read(Stream, Term),
    close(Stream),
    write('DEBUG: Configurazione letta: '), write(Term), nl,
    write('my config: '), write(Term), nl,
    
    write('DEBUG: Inizializzazione parametri agente...'), nl,
    initialize_agent_parameters(Term),
    write('DEBUG: =========================================='), nl,
    write('DEBUG: FINE INIZIALIZZAZIONE AGENTE'), nl,
    write('DEBUG: =========================================='), nl.

% Predicato per l'inizializzazione dei file dell'agente
start1(File, AgentName, Lib, Fil) :-
    % Assicurati che la directory work esista nella posizione corretta
    atom_concat('./', File, FullPath),
    % Crea il file dell'agente
    open(FullPath, write, Stream),
    write(Stream, ':- module('), write(Stream, AgentName), write(Stream, ', []).'), nl(Stream),
    write(Stream, ':- use_module(library(lists)).'), nl(Stream),
    % Aggiungi le librerie
    write_libraries(Stream, Lib),
    close(Stream),
    token(File),
    start1(File, AgentName, Lib, Fil).

% Predicato per scrivere le librerie nel file
write_libraries(_, []).
write_libraries(Stream, [Lib|Rest]) :-
    write(Stream, ':- use_module('), write(Stream, Lib), write(Stream, ').'), nl(Stream),
    write_libraries(Stream, Rest).

% Wrapper per token_fil che usa call/1
token_fil_wrapper(File) :-
    trace_message(['Chiamata token_fil_wrapper per file: ', File]),
    catch(
        (call(token_fil(File)),
         trace_message('token_fil_wrapper completato con successo')),
        Error,
        (trace_message(['ERROR in token_fil_wrapper: ', Error]),
         fail)
    ).

% Filtra i file di configurazione
filter_fil(Fil) :-
    (is_list(Fil) ->
        process_fil_list(Fil)
    ;
        trace_message('Processamento singolo file'),
        trace_message(['File da processare: ', Fil]),
        trace_message('Chiamata token_fil'),
        catch(
            (token_fil_wrapper(Fil),
             trace_message('token_fil completato con successo')),
            Error,
            (trace_message(['ERROR in token_fil: ', Error]),
             fail)
        ),
        trace_message('Fine token_fil'),
        retractall(parentheses(_)),
        trace_message('Chiamata remove_file_var'),
        catch(
            (trace_message('Verifica modulo remove_var...'),
             current_module(remove_var) -> 
                (trace_message('Modulo remove_var trovato'),
                 trace_message(['Verifica predicato remove_file_var/1...']),
                 (predicate_property(remove_var:remove_file_var(_), defined) ->
                    (trace_message('Predicato remove_file_var/1 trovato'),
                     trace_message(['Chiamata remove_file_var con file: ', Fil]),
                     remove_file_var(Fil),
                     trace_message('remove_file_var completato con successo'))
                 ;
                    (trace_message('ERROR: Predicato remove_file_var/1 non trovato nel modulo remove_var'),
                     trace_message('Lista dei predicati disponibili nel modulo remove_var:'),
                     findall(P, (predicate_property(remove_var:P, defined), functor(P, F, A)), Preds),
                     maplist(write_predicate, Preds),
                     trace_message('Predicati esportati dal modulo remove_var:'),
                     module_property(remove_var, exports(Exports)),
                     maplist(write_predicate, Exports),
                     fail)))
             ;
                (trace_message('ERROR: Modulo remove_var non trovato'),
                 fail)),
            Error,
            (trace_message(['ERROR in remove_file_var: ', Error]),
             trace_message(['Tipo di errore: ', Error]),
             fail)
        ),
        trace_message('Fine remove_file_var')
    ),
    trace_message('Fine filter_fil').

% Predicato helper per scrivere i predicati in modo leggibile
write_predicate(P) :-
    functor(P, F, A),
    format('  - ~w/~w~n', [F, A]).

% Processa una lista di file
process_fil_list([]) :-
    trace_message('Lista file vuota').
process_fil_list([File|Rest]) :-
    trace_message(['Processamento file: ', File]),
    trace_message('Chiamata token_fil'),
    catch(
        (token_fil_wrapper(File),
         trace_message('token_fil completato con successo')),
        Error,
        (trace_message(['ERROR in token_fil: ', Error]),
         fail)
    ),
    trace_message('Fine token_fil'),
    retractall(parentheses(_)),
    trace_message('Chiamata remove_file_var'),
    catch(
        (trace_message('Verifica modulo remove_var...'),
         current_module(remove_var) -> 
            (trace_message('Modulo remove_var trovato'),
             trace_message(['Verifica predicato remove_file_var/1...']),
             (predicate_property(remove_var:remove_file_var(_), defined) ->
                (trace_message('Predicato remove_file_var/1 trovato'),
                 trace_message(['Chiamata remove_file_var con file: ', File]),
                 remove_file_var(File),
                 trace_message('remove_file_var completato con successo'))
             ;
                (trace_message('ERROR: Predicato remove_file_var/1 non trovato nel modulo remove_var'),
                 fail)))
         ;
            (trace_message('ERROR: Modulo remove_var non trovato'),
             fail)),
        Error,
        (trace_message(['ERROR in remove_file_var: ', Error]),
         trace_message(['Tipo di errore: ', Error]),
         fail)
    ),
    trace_message('Fine remove_file_var'),
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
    clause(agent_x(Ag,Ind,_,_), _),
    catch(
        (rd_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)) ->
            receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con) ; 
            trace_message('Nessun messaggio disponibile')),
        Error,
        fail
    ).

receive_message0(Ag,Ind,AgM,IndM,Language,Ontology,Con) :-
    assert_this(ext_agent(AgM,IndM,Ontology,Language)),
    catch(
          in_noblock(message(Ind,Ag,IndM,AgM,Language,Ontology,Con)),
        Error,
        fail
    ),
    (clause(receive(Con), _) ->
        call_con(AgM,IndM,Language,Ontology,Con) ;
        not_receivable_meta(AgM,IndM,Language,Ontology,Con)).

% Gestione degli eventi
process_events :-
    findall(Event, clause(event(Event), _), Events),
    process_event_list(Events).

process_event_list([]).
process_event_list([Event|Rest]) :-
    process_single_event(Event),
    process_event_list(Rest).

process_single_event(Event) :-
    (clause(event_condition(Event, Condition), _) ->
        (call(Condition) -> 
            process_event_action(Event) ; 
            true) ;
        process_event_action(Event)).

process_event_action(Event) :-
    (clause(event_action(Event, Action), _) ->
        call(Action) ;
        true).

% Gestione degli eventi esterni
process_external_events :-
    clause(agent_x(_,_,S,_), _),
    read_line_from_file(S,1),
    clause(eventE(Es), _),
    (Es = [] -> 
        true ; 
        (process_high_events, process_normal_events)).

% Gestione degli eventi ad alta priorita'
process_high_events :-
    write('DEBUG:   - Controllo eventi alta priorita...'), nl,
    (clause(ev_high(_,_,_), _) -> 
        (write('DEBUG:   - Eventi alta priorita trovati, elaborazione...'), nl,
         process_high_events1) ; 
        write('DEBUG:   - Nessun evento alta priorita trovato'), nl).

process_high_events1 :-
    findall(ev_high(AgM,E,T), clause(ev_high(AgM,E,T), _), L),
    last(L, ev_high(Ag,E,T)),
    write('DEBUG:   - Elaborazione evento alta priorita: '), write(ev_high(Ag,E,T)), nl,
    (once(eve_cond(E)) -> 
        (write('DEBUG:   - Condizione evento verificata, elaborazione...'), nl,
         process_high_event(Ag,E,T)) ; 
        (write('DEBUG:   - Condizione evento non verificata'), nl,
         no_process_high_event(Ag,E,T))).

% Gestione degli eventi normali
process_normal_events :-
    write('DEBUG:   - Controllo eventi normali...'), nl,
    (clause(ev_normal(_,_,_), _) -> 
        (write('DEBUG:   - Eventi normali trovati, elaborazione...'), nl,
         process_normal_events1) ; 
        write('DEBUG:   - Nessun evento normale trovato'), nl).

process_normal_events1 :-
    clause(ev_normal(AgM,E,T), _),
    write('DEBUG:   - Elaborazione evento normale: '), write(ev_normal(AgM,E,T)), nl,
    (once(eve_cond(E)) -> 
        (write('DEBUG:   - Condizione evento verificata, elaborazione...'), nl,
         process_normal_event(AgM,E,T)) ; 
        (write('DEBUG:   - Condizione evento non verificata'), nl,
         no_process_normal_event(AgM,E,T))).

% Gestione degli eventi interni
internal_event :-
    write('DEBUG:   - Controllo eventi interni...'), nl,
    clause(dali_agent_(_,_,S,_), _),
    read_line_from_file(S, 2),
    clause(evintI(L), _),
    (L \= [] -> 
        (write('DEBUG:   - Eventi interni trovati: '), write(L), nl,
         check_internal_event1(L, S)) ; 
        write('DEBUG:   - Nessun evento interno trovato'), nl).

% Gestione degli eventi interni
check_internal_event1([], _).
check_internal_event1([Event|Rest], S) :-
    process_internal_event(Event, S),
    check_internal_event1(Rest, S).

% Processa un singolo evento interno
process_internal_event(Event, S) :-
    (clause(event_condition(Event, Condition), _) ->
        (call(Condition) -> 
            process_event_action(Event) ; 
            true) ;
        process_event_action(Event)).

% Gestione degli obiettivi
manage_goals :-
    (clause(goal(_), _) -> 
        manage_goals1 ; 
        true),
    trace_message('Fine gestione obiettivi').

manage_goals1 :-
    findall(goal(G), clause(goal(G), _), L),
    last(L, U),
    process_goals(L, U).

process_goals([], _) :-
    trace_message('Nessun obiettivo da processare').
process_goals([Me|Rest], U) :-
    (clause(goal_completed(Me), _) -> 
        true ; 
        (trace_message('Obiettivo non completato, gestione...'),
         manage_goals2(Me))),
    (Me = U -> 
        true ; 
        (trace_message('Passaggio al prossimo obiettivo'),
         process_goals(Rest, U))).

manage_goals2(Me) :-
    (clause(goal_precondition(Me,Pre), _) ->
        (Pre -> manage_goals3(Me) ; true)
    ;
        manage_goals3(Me)).

manage_goals3(Me) :-
    (clause(goal_do(Me,Do), _) ->
        (Do -> manage_goals4(Me) ; true)
    ;
        manage_goals4(Me)).

manage_goals4(Me) :-
    (clause(goal_postcondition(Me,Post), _) ->
        (Post -> manage_goals5(Me) ; true)
    ;
        manage_goals5(Me)).

manage_goals5(Me) :-
    assert_this(goal_completed(Me)),
    trace_message('Obiettivo completato').

% Gestione del tempo
manage_time :-
    (clause(time(_), _) -> 
        manage_time1 ; 
        true),
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
    (clause(condition(_), _) -> 
        check_conditions1 ; 
        true),
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
          fail),
    trace_message('File aperto con successo').

safe_close_stream(Stream) :-
    catch(close(Stream),
          error(Error, _),
          fail),
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
    write('DEBUG: =========================================='), nl,
    write('DEBUG: INIZIO NUOVO CICLO AGENTE'), nl,
    write('DEBUG: =========================================='), nl,
    
    % 1. Gestione eventi esterni (alta priorita')
    write('DEBUG: [1/5] Gestione eventi esterni alta priorita...'), nl,
    process_high_events,
    write('DEBUG: [1/5] Fine gestione eventi esterni alta priorita'), nl,
    
    % 2. Gestione eventi interni
    write('DEBUG: [2/5] Gestione eventi interni...'), nl,
    internal_event,
    write('DEBUG: [2/5] Fine gestione eventi interni'), nl,
    
    % 3. Gestione obiettivi
    write('DEBUG: [3/5] Gestione obiettivi...'), nl,
    manage_goals,
    write('DEBUG: [3/5] Fine gestione obiettivi'), nl,
    
    % 4. Gestione tempo
    write('DEBUG: [4/5] Gestione tempo...'), nl,
    manage_time,
    write('DEBUG: [4/5] Fine gestione tempo'), nl,
    
    % 5. Gestione condizioni
    write('DEBUG: [5/5] Gestione condizioni...'), nl,
    check_conditions,
    write('DEBUG: [5/5] Fine gestione condizioni'), nl,
    
    % Attendi un breve periodo prima del prossimo ciclo
    write('DEBUG: Attesa 1 secondo...'), nl,
    sleep(1),
    
    % Continua il ciclo
    write('DEBUG: =========================================='), nl,
    write('DEBUG: FINE CICLO AGENTE - RIAVVIO'), nl,
    write('DEBUG: =========================================='), nl, nl,
    run_agent.
