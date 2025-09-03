% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Rule parsing module for DALI agents

:- module(rule_parser, [
    aprifile/1,
    aprifile_en/1,
    take/0,
    spezza/1,
    ejec/2,
    costruisci0/1,
    examine_mul/0,
    eve_mul_first/1,
    load_program_rules/1
]).

:- use_module('../utils/dali_list_utils').
:- use_module('../utils/dali_debug_utils').
% Import standard lists predicates that don't conflict
:- use_module(library(lists), [last/2, append/3, member/2]).

% Dynamic predicates for rule processing
:- dynamic rule_base/1.
:- dynamic clause_man/1.
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

%% Main file parser
aprifile(F) :-
    see(F),
    repeat,
    read(T),
    expand_term(T, Te),
    (T = end_of_file ->
        true
    ;
        assert(rule_base(Te))
    ),
    T == end_of_file,
    !,
    seen,
    take,
    costruisci0(F).

%% Alternative file parser for clause management
aprifile_en(F) :-
    see(F),
    repeat,
    read(T),
    assert(clause_man(T)),
    T == end_of_file,
    !,
    seen.

%% Process loaded rules
take :-
    findall(T, clause(rule_base(T), _), L),
    last(L, U),
    repeat,
    member(M, L),
    spezza(M),
    M == U, !,
    retractall(rule_base(_)),
    (clause(mul(_), _) -> ass_mul_first; true).

%% Load program rules from file
load_program_rules(FilePl) :-
    open(FilePl, read, Stream, []),
    repeat,
    read(Stream, Term),
    (Term = end_of_file ->
        true
    ;
        (assertz(Term), trace_point('Loaded rule', Term))
    ),
    Term == end_of_file, !,
    close(Stream).

%% Multiple events examination
examine_mul :-
    (clause(mul(_), _) -> examine1_mul; true).

examine1_mul :-
    findall(L, clause(mul(L), _), S),
    last(S, E),
    repeat,
    member(Me, S),
    keep_past(Me),
    (clause(no_check, _) ->
        retractall(no_check)
    ;
        chiama_cong(Me)
    ),
    Me == E, !.

keep_past(Me) :-
    append([eve], L, Me),
    examine_head(L).

examine_head(L) :-
    last(L, U),
    repeat,
    member(Me, L),
    check_past(Me),
    Me == U, !.

check_past(Me) :-
    (clause(past(Me, _, _), _) -> true; assert(no_check)).

chiama_cong(Me) :-
    T =.. Me,
    append([eve], L, Me),
    controlla_time_ep(L, T),
    take_time_ep(L).

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
costruisci0(F) :-
    ((((clause(even(_), _); clause(evin(_), _)); clause(azi(_), _)); clause(evN(_), _)) ->
        recupera_fun0(F)
    ;
        true
    ).

recupera_fun0(F) :-
    recupera_funE(F),
    recupera_funI(F),
    recupera_funA(F),
    recupera_tot_cd(F),
    recupera_funEn(F),
    recupera_gol_obt(F),
    recupera_gol_test(F),
    recupera_tot_rem(F),
    recupera_E(F).

%% Multiple events first processing
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
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(azi(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(azi(_)).

%% Conditions recovery
recupera_tot_cd(F) :-
    (clause(cond(_), _) ->
        recupera_cd(F)
    ;
        lista_assente(F)
    ).

recupera_cd(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(cond(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(cond(_)).

%% Present events recovery
recupera_funEn(F) :-
    (clause(evN(_), _) ->
        (transf_eventi_presente, recupera_fun_evN(F))
    ;
        lista_assente(F)
    ).

transf_eventi_presente :-
    findall(X, clause(evN(X), _), Ls),
    last(Ls, U),
    repeat,
    member(Me, Ls),
    functor(Me, Fu, _),
    assert(app_evN(Fu)),
    Me == U, !.

recupera_fun_evN(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(app_evN(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(app_evN(_)),
    retractall(evN(_)).

%% Goals to obtain recovery
recupera_gol_obt(F) :-
    (clause(obt_goal(_), _) ->
        recupera_fun_obt_goal(F)
    ;
        lista_assente(F)
    ).

recupera_fun_obt_goal(F) :-
    name(F, L),
    append(L, [101], T),
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
recupera_gol_test(F) :-
    (clause(test_goal(_), _) ->
        (transf_tes_goal, recupera_fun_tes_goal(F))
    ;
        lista_assente(F)
    ).

transf_tes_goal :-
    findall(X, clause(test_goal(X), _), Ls),
    last(Ls, U),
    repeat,
    member(Me, Ls),
    functor(Me, Fu, _),
    assert(app_tes_goal(Fu)),
    Me == U, !.

recupera_fun_tes_goal(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(app_tes_goal(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(app_tes_goal(_)).

%% Remember facts recovery
recupera_tot_rem(F) :-
    (clause(fact_rem(_), _) ->
        recupera_rem(F)
    ;
        lista_assente(F)
    ).

recupera_rem(F) :-
    findall(X, clause(fact_rem(X), _), Ls1),
    remove_dups(Ls1, Ls),
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    open(Y, append, Stream, []),
    write(Stream, Ls),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(fact_rem(_)).

%% External events recovery
recupera_E(F) :-
    (clause(even(_), _) ->
        recupera_tot_even(F)
    ;
        lista_assente(F)
    ).

recupera_tot_even(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    findall(X, clause(even(X), _), LA1),
    remove_dups(LA1, LA),
    open(Y, append, Stream, []),
    write(Stream, LA),
    write(Stream, '.'),
    nl(Stream),
    close(Stream),
    retractall(even(_)).

%% Empty list for absent elements
lista_assente(F) :-
    name(F, L),
    append(L, [101], T),
    name(Y, T),
    open(Y, append, Stream, []),
    write(Stream, []),
    write(Stream, '.'),
    nl(Stream),
    close(Stream). 