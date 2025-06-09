% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Main DALI agent system coordinator

:- module(dali_core, [
    start_dali_agent/1,
    go/0,
    pari/0,
    internal/0,
    external/0
]).

% Load all utility modules
:- use_module('utils/dali_list_utils').
:- use_module('utils/dali_file_utils').
:- use_module('utils/dali_debug_utils').
:- use_module('utils/dali_time_utils').

% Load core agent modules
:- use_module('agent/agent_init').

% Load parsing modules
:- use_module('parsing/rule_parser').

% Load existing includes from original file
% Note: These files will be modularized in future phases
% :- use_module('communication_onto.pl').
% :- use_module('substitute.pl').
% :- use_module('tokefun.pl').
% :- use_module('meta1.pl').
% :- use_module('remove_variables.pl').
% :- use_module('memory.pl').
% :- use_module('examine_past_constraints.pl').
% :- use_module('multiple_events_processor.pl').
% :- use_module('utils.pl').

% Standard library modules
:- use_module(library(random)).
% Note: lists library conflicts with our custom list_utils
% :- use_module(library(lists)).
:- use_module(library(system)).
% Note: Specialized libraries may not be available in all SWI-Prolog installations
% :- use_module(library('linda/client')).
% :- use_module(library(clpq)).
% :- use_module(library(fdbg)).
% :- use_module(library(file_systems)).

% Set compilation flags
:- multifile user:term_expansion/6.
:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).

% Define operators from original file
:- op(500, xfy, :>).
:- op(500, xfy, :<).
:- op(10, xfy, ~/).
:- op(1200, xfy, </).
:- op(200, xfy, ?/).
:- op(1200, xfx, [:-, :>]).
:- op(1200, xfx, [:-, :<]).
:- op(1200, xfx, [:-, ~/]).
:- op(1200, xfx, [:-, </]).
:- op(1200, xfx, [:-, ?/]).

% Dynamic predicates
:- dynamic tesg/1.
:- dynamic ontology/2.
:- dynamic en/1.
:- dynamic told/6.
:- dynamic export_past/1.
:- dynamic export_past_do/1.
:- dynamic export_past_not_do/1.
:- dynamic deltat/1.
:- dynamic deltatime/1.
:- dynamic simultaneity_interval/1.
:- dynamic wishlist/1.
:- dynamic tstart/1.
:- dynamic mem_current/1.
:- dynamic mem_no_dup/1.
:- dynamic mem_past/1.
:- dynamic verifica_lista/1.
:- dynamic past/3.
:- dynamic tep/2.
:- dynamic fatto_mul/2.
:- dynamic continue_mul_f/1.
:- dynamic eve/1.
:- dynamic eve_cond/1.

% Term expansion rules from original file
user:term_expansion((X,Y), [], [], ([X,Y]), [], []).
user:term_expansion((X;Y), [], [], ([X,Y]), [], []).
user:term_expansion((H:>B), [], [], (H:-B), [], []).
user:term_expansion((X->Y), [], [], ([X,Y]), [], []).
user:term_expansion((H:<B), [], [], (cd(H):-B), [], []).
user:term_expansion((H~/B), [], [], (export_past(H):-decompose(H,B)), [], []).
user:term_expansion((H</B), [], [], (export_past_not_do(H):-decompose_not_do(H,B)), [], []).
user:term_expansion((H?/B), [], [], (export_past_do(H):-decompose_if_it_is(H,B)), [], []).
user:term_expansion((H:B), [], [], (ct(H,B)), [], []).
user:term_expansion((H:at(B)), [], [], (ct(H,B)), [], []).

%% Main entry point - initialize DALI agent
start_dali_agent(ConfigFile) :-
    trace_point('Starting DALI agent system'),
    agent_init:start0(ConfigFile),
    trace_point('Agent initialized, starting main loop'),
    go.

%% Main agent execution loop
go :-
    repeat,
    (pari, side_goal),
    fail, !.

%% Random execution order of internal/external processing
pari :-
    sleep(1),
    random(1, 10, R),
    R1 is R mod 2,
    (R1 = 0 ->
        (internal, external)
    ;
        (external, internal)
    ).

%% Internal event processing cycle
internal :-
    blocco_constr,
    ricmess,
    ev_int,
    ev_goal,
    ev_int0,
    blocco_numero_ev_int,
    blocco_frequenza,
    ev_int2,
    controlla_freq_tent,
    svuota_coda_priority,
    controlla_freq_iv,
    scatena,
    blocco_constr,
    keep_action,
    execute_do_action_propose,
    prendi_action_normal,
    blocco_constr,
    controlla_vita.

%% External event processing cycle
external :-
    blocco_constr,
    ricmess,
    processa_eve,
    rule_parser:examine_mul,
    keep_action,
    svuota_coda_priority,
    prendi_action_normal,
    blocco_constr,
    controlla_vita.

%% Side goal processing
side_goal :-
    obtaining_goals,
    residue_goal.

%% Constraint evaluation
blocco_constr :-
    evaluate_evp_constr,
    evaluate_evp_do,
    evaluate_evp_not_do.

%% Memory and past events management
controlla_vita :-
    controlla_vita_past_base,
    controlla_vita_remember.

%% Utility predicates
en(X) :- en(X, _).
not(X) :- (X -> false; true).

%% Past event predicates
evp(E) :- clause(past(E), _).
evp(E) :- clause(past(E, _, _), _).
rem(E) :- clause(remember(E, _, _), _).
isa(E) :- clause(isa(E, _, _), _).

%% Test goals
tesg(X) :-
    clause(past(X), _);
    clause(isa(X, _, _), _);
    clause(past(X, _, _), _).

% Placeholder predicates that need to be implemented in separate modules
% These will be moved to appropriate modules in later phases

ricmess :- true.  % Communication module
ev_int :- true.   % Internal events module
ev_goal :- true.  % Goal management module
ev_int0 :- true.  % Internal events processing
blocco_numero_ev_int :- true.  % Internal events numbering
blocco_frequenza :- true.      % Frequency management
ev_int2 :- true.  % Internal events phase 2
controlla_freq_tent :- true.   % Frequency control
svuota_coda_priority :- true.  % Priority queue management
controlla_freq_iv :- true.     % Internal frequency control
scatena :- true.  % Event triggering
keep_action :- true.           % Action management
execute_do_action_propose :- true.  % Action execution
prendi_action_normal :- true.  % Normal action processing
processa_eve :- true.          % External event processing
% examine_mul/0 is provided by rule_parser module (line 154: rule_parser:examine_mul)
obtaining_goals :- true.       % Goal obtaining
residue_goal :- true.          % Residual goal processing
evaluate_evp_constr :- true.   % Constraint evaluation
evaluate_evp_do :- true.       % Do constraint evaluation
evaluate_evp_not_do :- true.   % Not-do constraint evaluation
controlla_vita_past_base :- true.   % Past events lifecycle
controlla_vita_remember :- true.    % Remember events lifecycle

% Print startup message
:- initialization((
    write('DALI Agent System - Modular Version'), nl,
    write('Modules loaded successfully'), nl,
    write('Use start_dali_agent(ConfigFile) to start an agent'), nl
)). 