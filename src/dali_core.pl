% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Main DALI agent system coordinator - Runtime execution system

:- module(dali_core, [
    start_dali_agent/1,
    go/0,
    pari/0,
    internal/0,
    external/0
]).

% Load common DALI components
:- use_module('dali_common').

% Load core agent modules for runtime
:- use_module('agent/agent_init').

% Import debug utilities directly for trace_point
:- use_module('utils/dali_debug_utils').

%% Main entry point - initialize DALI agent with full execution
start_dali_agent(ConfigFile) :-
    trace_point('Starting DALI agent system with full execution', ConfigFile),
    agent_init:start0(ConfigFile),
    trace_point('Agent initialized, starting main execution loop'),
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

%% Internal processing cycle
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

%% External processing cycle
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

% Print startup message
:- write('DALI Core Runtime System loaded'), nl.
