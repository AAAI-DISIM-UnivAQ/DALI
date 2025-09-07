% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Rule event processing module for DALI agents
% Handles event processing and recovery functions

:- module(rule_event_processor, [
    ass_mul_first/0,
    keep_past_ass_first/1,
    examine_head_ass_first/1,
    recupera_funE/1,
    transf_eventi_esterni/0,
    recupera_fun_even/1,
    recupera_funI/1,
    recupera_fun_evin/1,
    recupera_funA/1,
    recupera_fun_azioni/1,
    recupera_funN/1,
    recupera_fun_evN/1,
    recupera_funC/1,
    recupera_fun_cond/1,
    recupera_funG/1,
    recupera_fun_goal/1,
    recupera_funTG/1,
    recupera_fun_test_goal/1,
    recupera_funR/1,
    recupera_fun_rem/1,
    lista_assente/1
]).

:- use_module('../utils/dali_list_utils').
:- use_module('../utils/dali_debug_utils').
% Import standard lists predicates that don't conflict
:- use_module(library(lists), [last/2, append/3, member/2]).

% Dynamic predicates for event processing
:- dynamic mul/1.
:- dynamic even/1.
:- dynamic evin/1.
:- dynamic azi/1.
:- dynamic evN/1.
:- dynamic cond/1.
:- dynamic obt_goal/1.
:- dynamic test_goal/1.
:- dynamic fact_rem/1.
:- dynamic app_even/1.
:- dynamic app_evN/1.
:- dynamic app_tes_goal/1.

%% Note: eve_mul_first/1 and continue_mul_f/1 are now defined in rule_clause_analyzer.pl
%% to avoid circular dependencies and conflicts

%% Assert helper (defined here to avoid circular dependencies)
asse_cosa(C) :-
    (clause(C, _) -> true; assert(C)).

ass_mul_first :-
    findall(L, clause(mul(L), _), S),
    last(S, E),
    repeat,
    member(Me, S),
    keep_past_ass_first(Me),
    Me == E, !,
    retractall(mul(_)).

keep_past_ass_first(Me) :-
    append([eve], L, Me),
    examine_head_ass_first(L).

examine_head_ass_first(L) :-
    last(L, U),
    repeat,
    member(Me, L),
    asse_cosa(even(Me)),
    Me == U, !.

%% Recovery functions for different element types
recupera_funE(F) :-
    (clause(even(_), _) ->
        (transf_eventi_esterni, recupera_fun_even(F))
    ;
        lista_assente(F)
    ).

transf_eventi_esterni :-
    findall(X, clause(even(X), _), Ls),
    last(Ls, U),
    repeat,
    member(Me, Ls),
    functor(Me, Fu, _),
    assert(app_even(Fu)),
    Me == U, !.

recupera_fun_even(F) :-
    name(F, L),
    append(L, [101], T),  % 'e'
    name(Y, T),
    findall(X, clause(app_even(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(app_even(_)).

%% Internal events recovery
recupera_funI(F) :-
    (clause(evin(_), _) ->
        recupera_fun_evin(F)
    ;
        lista_assente(F)
    ).

recupera_fun_evin(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(evin(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(evin(_)).

%% Actions recovery
recupera_funA(F) :-
    (clause(azi(_), _) ->
        recupera_fun_azioni(F)
    ;
        lista_assente(F)
    ).

recupera_fun_azioni(F) :-
    name(F, L),
    append(L, [97], T),  % 'a'
    name(Y, T),
    findall(X, clause(azi(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(azi(_)).

%% Events N recovery
recupera_funN(F) :-
    (clause(evN(_), _) ->
        recupera_fun_evN(F)
    ;
        lista_assente(F)
    ).

recupera_fun_evN(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(evN(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(evN(_)).

%% Conditions recovery
recupera_funC(F) :-
    (clause(cond(_), _) ->
        recupera_fun_cond(F)
    ;
        lista_assente(F)
    ).

recupera_fun_cond(F) :-
    name(F, L),
    append(L, [118], T),  % 'v'
    name(Y, T),
    findall(X, clause(cond(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(cond(_)).

%% Goals recovery
recupera_funG(F) :-
    (clause(obt_goal(_), _) ->
        recupera_fun_goal(F)
    ;
        lista_assente(F)
    ).

recupera_fun_goal(F) :-
    name(F, L),
    append(L, [103], T),  % 'g'
    name(Y, T),
    findall(X, clause(obt_goal(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(obt_goal(_)).

%% Test goals recovery
recupera_funTG(F) :-
    (clause(test_goal(_), _) ->
        recupera_fun_test_goal(F)
    ;
        lista_assente(F)
    ).

recupera_fun_test_goal(F) :-
    name(F, L),
    append(L, [116, 103], T),  % 'tg'
    name(Y, T),
    findall(X, clause(test_goal(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(test_goal(_)).

%% Facts removal recovery
recupera_funR(F) :-
    (clause(fact_rem(_), _) ->
        recupera_fun_rem(F)
    ;
        lista_assente(F)
    ).

recupera_fun_rem(F) :-
    name(F, L),
    append(L, [102], T),  % 'f'
    name(Y, T),
    findall(X, clause(fact_rem(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(fact_rem(_)).

%% Handle absent lists
lista_assente(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    open(Y, append, Stream, []),
    write(Stream, []),
    write(Stream, '.'),
    nl(Stream),
    close(Stream).

% Print module loaded message
:- write('DALI Rule Event Processor loaded'), nl.
