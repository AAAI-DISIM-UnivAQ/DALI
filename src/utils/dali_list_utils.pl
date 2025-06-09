% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% List utility predicates for DALI agents

:- module(dali_list_utils, [
    remove_dups/2,
    diff/3,
    intersection/3,
    concatena_items_poi_in_string/2,
    list_in_ascii/2,
    ascii_in_list/2
]).

% Note: Removed library(lists) import to avoid conflicts with our custom intersection/3
% :- use_module(library(lists)).

%% Remove duplicates from list
remove_dups([], []).
remove_dups([H|T], [H|T1]) :-
    \+ member(H, T), !,
    remove_dups(T, T1).
remove_dups([_|T], T1) :-
    remove_dups(T, T1).

%% List difference - elements in first list but not in second
diff([], _, []).
diff([H|Tail], L2, [H|Tail1]) :-
    \+ member(H, L2), !,
    diff(Tail, L2, Tail1).
diff([_|Tail], L2, Tail1) :-
    diff(Tail, L2, Tail1).

%% List intersection
intersection([], _, []).
intersection([H|T], S2, [H|T3]) :- 
    member(H, S2),
    intersection(T, S2, T3).
intersection([_|T], S2, S3) :-
    intersection(T, S2, S3).

%% Concatenate list items into string
concatena_items_poi_in_string(L, S) :-
    assert(conc_item003('')),
    retractall(k_item003(_)),
    assert(k_item003(1)),
    length(L, N),
    (N > 1 ->
        (parte_prima_conc_items(L, N),
         parte_seconda(L, N, S))
    ;
        parte_seconda(L, N, S)
    ).

parte_prima_conc_items(L, N) :-
    repeat,
    clause(k_item003(K), _),
    nth1(K, L, M),
    clause(conc_item003(X), _),
    atom_concat(X, M, M1),
    atom_concat(M1, ',', M2),
    retractall(conc_item003(X)),
    assert(conc_item003(M2)),
    R is K + 1,
    assert(k_item003(R)),
    retractall(k_item003(K)),
    K == N, !.

parte_seconda(L, N, S) :-
    nth1(N, L, M3),
    clause(conc_item003(D), _),
    atom_concat(D, M3, S),
    retractall(k_item003(_)),
    retractall(conc_item003(_)).

%% Transform list to ASCII representation
list_in_ascii(Ln, La) :-
    trasform_list_in_ascii(Ln),
    create_new_ascii_list(La).

trasform_list_in_ascii(Ln) :-
    last(Ln, U),
    repeat,
    member(Mn, Ln),
    name(Mn, Ma),
    assert(list_ascii(Ma)),
    Mn == U, !.

create_new_ascii_list(La) :-
    findall(X, clause(list_ascii(X), _), La),
    retractall(list_ascii(_)).

%% Transform ASCII representation to list
ascii_in_list(La, Ln) :-
    trasform_ascii_in_list(La),
    create_new_term_list(Ln).

trasform_ascii_in_list(La) :-
    last(La, U),
    repeat,
    member(Ma, La),
    name(Tn, Ma),
    assert(new_this_term(Tn)),
    Ma == U, !.

create_new_term_list(Ln) :-
    findall(X, clause(new_this_term(X), _), Ln),
    retractall(new_this_term(_)). 