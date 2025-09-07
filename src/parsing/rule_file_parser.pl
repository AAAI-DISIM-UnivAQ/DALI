% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Rule file parsing module for DALI agents
% Handles file I/O and basic rule loading

:- module(rule_file_parser, [
    aprifile/1,
    aprifile_en/1,
    take/0,
    load_program_rules/1
]).

:- use_module('../utils/dali_list_utils').
:- use_module('../utils/dali_debug_utils').
% Import standard lists predicates that don't conflict
:- use_module(library(lists), [last/2, append/3, member/2]).

% Dynamic predicates for rule processing
:- dynamic rule_base/1.
:- dynamic clause_man/1.

%% Main file parser
aprifile(F) :-
    see(F),
    repeat,
    read(T),
    expand_term(T, Te),
    (T = end_of_file ->
        true
    ;
        (assert(rule_base(Te)), fail)
    ),
    seen,
    !.

%% Alternative file parser for clause management
aprifile_en(F) :-
    see(F),
    repeat,
    read(T),
    (T = end_of_file ->
        true
    ;
        (assert(clause_man(T)), fail)
    ),
    seen,
    !.

%% Process loaded rules
take :-
    retract(rule_base(X)),
    call(X),
    fail.
take.

%% Load program rules from file
load_program_rules(FilePl) :-
    trace_point('Loading program rules from file', FilePl),
    see(FilePl),
    repeat,
    read(Clause),
    (Clause = end_of_file ->
        seen, !
    ;
        (assert(Clause), fail)
    ).

% Print module loaded message
:- write('DALI Rule File Parser loaded'), nl.
