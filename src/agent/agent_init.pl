% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Agent initialization module for DALI agents

:- module(agent_init, [
    start0/1,
    start1/4,
    load_ontology_file/2,
    filtra_fil/1,
    agent_from_config/1,
    file_exists/1,
    linda_client/1,
    out/1,
    in_noblock/1,
    delete_file/1
]).

:- use_module('../utils/dali_file_utils').
:- use_module('../utils/dali_debug_utils').
:- use_module('../parsing/rule_parser').
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).
% SICStus Prolog specific libraries
% :- use_module(library('linda/client')).  % May be available in SICStus with Linda
% :- use_module(library(clpq)).
% :- use_module(library(fdbg)).

%% Define file_exists/1 for SICStus Prolog
file_exists(File) :-
    catch(
        (open(File, read, Stream, []), close(Stream)),
        _,
        fail
    ).

%% Linda system compatibility predicates
% These provide compatibility for the Linda distributed communication system
% In SICStus with Linda library: uses real Linda predicates
% Otherwise: provides placeholders to allow system operation

linda_client(Server) :-
    trace_point('Linda client initialized', Server),
    % For now, just succeed - actual Linda functionality would require
    % the full Linda library or alternative communication mechanism
    true.

out(Term) :-
    % Placeholder for Linda out operation
    % In a full implementation, this would send Term to the Linda tuple space
    trace_point('Linda out', Term),
    true.

in_noblock(Term) :-
    % Placeholder for non-blocking Linda in operation
    % In a full implementation, this would try to retrieve Term from tuple space
    trace_point('Linda in_noblock attempted', Term),
    fail.  % Always fail since no tuple space is implemented

%% Utility predicates used by included DALI files
% if/3 is built-in in SICStus Prolog, no custom definition needed

%% SICStus Prolog compatible delete_file
delete_file(File) :-
    catch(
        (file_exists(File) -> 
            delete_file_builtin(File)
        ; 
            true
        ),
        _,
        trace_point('Cannot delete file', File)
    ).

% Try different ways to delete file in SICStus
delete_file_builtin(File) :-
    % In SICStus, try system command
    atom_concat('rm -f ', File, Command),
    system(Command).

%% Note: SICStus Prolog has see/1, seen/0, tell/1, told/0 as built-in predicates
%% No need to redefine them for SICStus compatibility

% Include original DALI files for compatibility
:- include('../tokefun.pl').
:- include('../remove_variables.pl').
:- include('../meta1.pl').
:- include('../memory.pl').

% Dynamic predicates for agent state
:- dynamic user_profile_location/1.
:- dynamic dali_onto_location/1.
:- dynamic server_obj/1.
:- dynamic specialization/1.
:- dynamic own_language/1.
:- dynamic agente/4.
:- dynamic agent/1.
:- dynamic time_charge/1.
:- dynamic ontology/3.

%% Main agent initialization
start0(FI) :-
    trace_point('start0: Beginning agent initialization', FI),
    set_prolog_flag(redefine_warnings, off),
    set_prolog_flag(discontiguous_warnings, off),
    trace_point('start0: Reading config file', FI),
    open(FI, read, Stream, []), 
    read(Stream, Me), 
    close(Stream),
    Me \= end_of_file,
    agent(File, AgentName, Ontolog, Lang, Fil, Lib, UP, DO, Specialization) = Me,
    trace_point('start0: Config loaded for agent', AgentName),
    open('server.txt', read, Stream2, []),
    read(Stream2, T),
    close(Stream2),
    trace_point('start0: Server config read', T),
    (UP = no -> true; assert(user_profile_location(UP))),
    (DO = no -> true; assert(dali_onto_location(DO))),
    assert(server_obj('localhost':3010)),
    trace_point('start0: Starting filtra_fil', Fil),
    filtra_fil(Fil),
    trace_point('start0: filtra_fil completed'),
    assert(specialization(Specialization)),
    (Ontolog = no -> true; load_ontology_file(Ontolog, AgentName)),
    assert(own_language(Lang)),
    trace_point('start0: Initializing Linda client'),
    linda_client('localhost':3010),
    out(activating_agent(AgentName)),
    trace_point('start0: Deleting agent files'),
    delete_agent_files(File),
    trace_point('start0: Processing token file'),
    token(File),
    trace_point('start0: Starting start1 phase'),
    start1(File, AgentName, Lib, Fil).

%% Agent configuration reader
agent_from_config(ConfigFile) :-
    open(ConfigFile, read, Stream, []),
    read(Stream, Config),
    close(Stream),
    Config = agent(File, AgentName, Ontology, Language, FileList, LibList, UserProfile, DaliOnto, Spec),
    trace_point('Loading agent configuration', Config),
    start_agent_with_config(File, AgentName, Ontology, Language, FileList, LibList, UserProfile, DaliOnto, Spec).

start_agent_with_config(File, AgentName, Ontology, Language, FileList, LibList, UserProfile, DaliOnto, Spec) :-
    (UserProfile = no -> true; assert(user_profile_location(UserProfile))),
    (DaliOnto = no -> true; assert(dali_onto_location(DaliOnto))),
    assert(specialization(Spec)),
    (Ontology = no -> true; load_ontology_file(Ontology, AgentName)),
    assert(own_language(Language)),
    assert(agent(AgentName)),
    filtra_fil(FileList),
    start1(File, AgentName, LibList, FileList).

%% Load ontology from file
load_ontology_file(Ontolog, Agent) :-
    open(Ontolog, read, Stream, []),
    read(Stream, PrefixesC),
    read(Stream, RepositoryC),
    read(Stream, HostC),
    close(Stream),
    name(Repository, RepositoryC),
    name(Prefixes, PrefixesC),
    name(Host, HostC),
    assert(ontology(Prefixes, [Repository, Host], Agent)).

%% Filter file list
filtra_fil(FI) :-
    arg(1, FI, File),
    token_fil(File),
    retractall(parentesi(_)),
    remove_var_file(File).



%% Extended initialization phase
start1(Fe, AgentName, Libr, Fil) :-
    set_prolog_flag(discontiguous_warnings, off),
    (Libr = no -> true; libreria(Fe, Libr, Fil)),

    pl_from_name(Fe, FilePl),
    ple_from_name(Fe, FilePle),
    plv_from_name(Fe, FilePlv),
    plf_from_name(Fe, FilePlf),
    txt_from_name(Fe, FileTxt),

    aprifile(FilePlv),
    aprifile_res(FilePlv),
    load_program_rules(FilePlv),

    remove_var(Fe),
    remove_var_ple(Fe),

    (file_exists(FilePlf) ->
        controlla_ev_all(FilePle)
    ;
        (inizializza_plf(FilePle), check_messaggio(FilePle, FilePlf))
    ),

    load_directives(FilePlf),
    server_obj(Tee),
    linda_client(Tee),
    assert(agente(AgentName, Tee, FilePle, FilePl)),
    clause(specialization(Sp), _),
    out(specialized_to(AgentName, Tee, Sp)),
    assert(agent(AgentName)),
    in_noblock(activating_agent(AgentName)),
    out(agente_attivo(AgentName, Tee)),
    assert(time_charge(5)),

    actions(FilePlv),
    cond_esterni(FilePlv), !,

    asser_evN(FilePle),
    obg_goal(FilePlv),
    aprifile_en(FilePl),
    ass_internal_repeat,
    ass_stringhe_mul(FilePlv),
    compile(FilePlv),

    apri_learn(FileTxt),
    start_learn,
    manage_export_past,
    manage_export_past_not_do,
    manage_export_past_do,
    check_constr_all,
    delete_agent_log_file(AgentName),
    print('..................   Actived Agent '),
    print(AgentName),
    print(' ...................'),
    nl.

%% Library loading
libreria(F, L0, Fil) :-
    name(F, Lf),
    append(Lf, [46,112,108], Ltf),  % .pl
    append(L0, Fil, L),
    name(F1, Ltf),
    (L = [] -> true; libreria1(F1, L)).

libreria1(F, L) :-
    last(L, U),
    repeat,
    member(Me, L),
    appendi_regole0(Me),
    Me == U, !,
    versa(F).

appendi_regole0(Me) :-
    name(Me, L),
    % First try .con file (communication configuration)
    append(L, [46,99,111,110], Ltf_con),  % .con
    name(T_con, Ltf_con),
    (file_exists(T_con) -> appendi_regole(T_con); true),
    % Then try .txt file
    append(L, [46,116,120,116], Ltf),  % .txt
    name(T, Ltf),
    (file_exists(T) -> appendi_files(T); true),
    % Finally try .pl file
    append(L, [46,112,108], Ltf1),  % .pl
    name(T1, Ltf1),
    (file_exists(T1) -> appendi_regole(T1); true).

appendi_files(T) :-
    set_prolog_flag(redefine_warnings, off),
    compile(T).

appendi_regole(Mef) :-
    open(Mef, read, Stream, []),
    repeat,
    read(Stream, T),
    (T = end_of_file ->
        true
    ;
        assert(da_agg(T))
    ),
    T == end_of_file, !,
    close(Stream).

%% Write communication file
versa(F) :-
    findall(X, clause(da_agg(X), _), L),
    last(L, U),
    open(F, append, Stream, []),
    nl(Stream),
    repeat,
    member(T, L),
    write(Stream, T),
    write(Stream, '.'),
    nl(Stream),
    T == U, !,
    close(Stream),
    retractall(da_agg(_)). 