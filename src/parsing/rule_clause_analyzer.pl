% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Rule clause analysis module for DALI agents
% Handles clause parsing, analysis, and construct building

:- module(rule_clause_analyzer, [
    spezza/1,
    ejec/2,
    costruisci0/1,
    examine_mul/0,
    examine1_mul/0,
    keep_past/1,
    examine_head/1,
    check_past/1,
    chiama_cong/1,
    is_clausola/2,
    prova_spezza/2,
    caso_if/3,
    go_to_if/2,
    asse_cosa/1,
    discriminate_learn/1,
    discriminate_learn1/1,
    discriminate_learn2/2,
    discriminate_learn3/0,
    eve_mul_first/1,
    continue_mul_f/1
]).

:- use_module('../utils/dali_list_utils').
:- use_module('../utils/dali_debug_utils').
% Import standard lists predicates that don't conflict
:- use_module(library(lists), [last/2, append/3, member/2]).

% Dynamic predicates for rule processing
:- dynamic mul/1.
:- dynamic no_check/0.
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

%% Multiple events examination
examine_mul :-
    examine1_mul.

examine1_mul :-
    retract(mul(Me)),
    (keep_past(Me) ->
        examine_head(Me)
    ;
        true
    ),
    fail.
examine1_mul.

keep_past(Me) :-
    check_past(Me),
    chiama_cong(Me).

examine_head(L) :-
    append(_, [H], L),
    functor(H, F, _),
    (F = past -> retract(mul(L)); true).

check_past(Me) :-
    \+ member(past(_), Me).

chiama_cong(Me) :-
    spezza(Me),
    fail.
chiama_cong(_).

%% Eve mul first processing (defined here to avoid circular dependency)
eve_mul_first(Head) :-
    functor(Head, _, N),
    Head =.. L_eve,
    ((arg(1, L_eve, _), N > 1) ->
        continue_mul_f(L_eve)
    ;
        true
    ).

continue_mul_f(L_eve) :-
    arg(1, L_eve, X_eve),
    ((X_eve = eve, is_list(L_eve)) ->
        asse_cosa(mul(L_eve))
    ;
        true
    ).

%% Split and analyze clauses
spezza(C) :-
    arg(1, C, Head),
    C =.. L,
    eve_mul_first(Head),
    (member(':-', L) -> is_clausola(L, Head); true).

is_clausola(L, Head) :-
    append([':-'], L1, L),
    ejec(L1, Head).

%% Execute clause components
ejec([], _).
ejec([S1|Resto], Head) :-
    ejec(S1, Head), !,
    ejec(Resto, Head).
ejec((X,Y), Head) :-
    expand_term((X,Y), Z),
    ejec(Z, Head).
ejec((X;Y), Head) :-
    expand_term((X;Y), Z),
    ejec(Z, Head).
ejec((X->Y), Head) :-
    expand_term((X->Y), Z),
    ejec(Z, Head).
ejec(X, Head) :-
    (X = [] -> true; prova_spezza(X, Head)).

%% Process different clause types
prova_spezza(M, Head) :-
    functor(M, F, _),
    (F = a ->
        (arg(1, M, Az), discriminate_learn(Az))
    ; F = eve ->
        (arg(1, M, Es), asse_cosa(even(Es)))
    ; F = evi ->
        (arg(1, M, Iv), asse_cosa(evin(Iv)))
    ; F = cd ->
        (arg(1, M, Co), asse_cosa(cond(Co)))
    ; F = en ->
        (arg(1, M, En), asse_cosa(evN(En)))
    ; F = rem ->
        (arg(1, M, Re), asse_cosa(fact_rem(Re)))
    ;
        caso_if(M, F, Head)
    ).

caso_if(M, F, Head) :-
    (F = if ->
        go_to_if(M, Head)
    ; F = obg ->
        (arg(1, M, G), asse_cosa(obt_goal(G)))
    ; F = tesg ->
        (arg(1, M, Go), asse_cosa(test_goal(Go)))
    ;
        true
    ).

go_to_if(M, Head) :-
    arg(2, M, A1),
    arg(3, M, A2),
    ejec(A1, Head),
    ejec(A2, Head).

%% Assert helper
asse_cosa(C) :-
    (clause(C, _) -> true; assert(C)).

%% Discriminate learning actions
discriminate_learn(Az) :-
    functor(Az, F, _),
    (F = message ->
        discriminate_learn1(Az)
    ;
        asse_cosa(azi(Az))
    ).

discriminate_learn1(Az) :-
    arg(2, Az, Pe),
    functor(Pe, F, _),
    (F = confirm ->
        discriminate_learn2(Pe, Az)
    ;
        asse_cosa(azi(Az))
    ).

discriminate_learn2(Pe, Az) :-
    arg(1, Pe, Le),
    functor(Le, F, _),
    (F = learn ->
        discriminate_learn3
    ;
        asse_cosa(azi(Az))
    ).

discriminate_learn3 :-
    assert(azi(message(_254802, confirm(learn(_254655), _254800)))).

%% Build constructs from parsed elements
costruisci0(C) :-
    arg(1, C, Head),
    C =.. L,
    eve_mul_first(Head),
    (member(':-', L) -> costruisci1(L, Head); true).

costruisci1(L, Head) :-
    append([':-'], L1, L),
    costruisci2(L1, Head).

costruisci2([], _).
costruisci2([S1|Resto], Head) :-
    costruisci2(S1, Head), !,
    costruisci2(Resto, Head).
costruisci2((X,Y), Head) :-
    expand_term((X,Y), Z),
    costruisci2(Z, Head).
costruisci2((X;Y), Head) :-
    expand_term((X;Y), Z),
    costruisci2(Z, Head).
costruisci2((X->Y), Head) :-
    expand_term((X->Y), Z),
    costruisci2(Z, Head).
costruisci2(X, Head) :-
    (X = [] -> true; costruisci3(X, Head)).

costruisci3(M, Head) :-
    functor(M, F, _),
    (F = a ->
        (arg(1, M, Az), asse_cosa(app_even(Az)))
    ; F = eve ->
        (arg(1, M, Es), asse_cosa(app_even(Es)))
    ; F = evi ->
        (arg(1, M, Iv), asse_cosa(app_even(Iv)))
    ; F = cd ->
        true
    ; F = en ->
        (arg(1, M, En), asse_cosa(app_evN(En)))
    ; F = rem ->
        true
    ; F = if ->
        costruisci_if(M, Head)
    ; F = obg ->
        true
    ; F = tesg ->
        (arg(1, M, Go), asse_cosa(app_tes_goal(Go)))
    ;
        true
    ).

costruisci_if(M, Head) :-
    arg(2, M, A1),
    arg(3, M, A2),
    costruisci2(A1, Head),
    costruisci2(A2, Head).

% Print module loaded message
:- write('DALI Rule Clause Analyzer loaded'), nl.
