:- use_module('../utils/dali_json_utils').

:- dynamic test_count/1.
:- dynamic passed_count/1.

% Inizializza i contatori
init_test_counters :-
    retractall(test_count(_)),
    retractall(passed_count(_)),
    assert(test_count(0)),
    assert(passed_count(0)).

% Esegui un test
run_test(Test) :-
    test_count(Count),
    retract(test_count(Count)),
    NewCount is Count + 1,
    assert(test_count(NewCount)),
    write('Test '), write(NewCount), write(': '),
    (   call(Test)
    ->  passed_count(Passed),
        retract(passed_count(Passed)),
        NewPassed is Passed + 1,
        assert(passed_count(NewPassed)),
        write('PASSED'), nl
    ;   write('FAILED'), nl
    ).

% Stampa il riepilogo dei test
print_summary :-
    test_count(Total),
    passed_count(Passed),
    nl,
    write('Test Summary:'), nl,
    write('Total tests: '), write(Total), nl,
    write('Passed: '), write(Passed), nl.

% Test cases
test_null :-
    json_from_atom('null', Term),
    json_to_atom(Term, JSON, [compact(true)]),
    JSON = 'null'.

test_boolean :-
    json_from_atom('true', Term1),
    json_from_atom('false', Term2),
    json_to_atom(Term1, JSON1, [compact(true)]),
    json_to_atom(Term2, JSON2, [compact(true)]),
    JSON1 = 'true',
    JSON2 = 'false'.

test_number :-
    json_from_atom('42', Term1),
    json_from_atom('-3.14', Term2),
    json_to_atom(Term1, JSON1, [compact(true)]),
    json_to_atom(Term2, JSON2, [compact(true)]),
    JSON1 = '42',
    JSON2 = '-3.14'.

test_string :-
    json_from_atom('"Hello, World!"', Term),
    json_to_atom(Term, JSON, [compact(true)]),
    JSON = '"Hello, World!"'.

test_array :-
    json_from_atom('[1, 2, 3]', Term1),
    json_from_atom('["a", "b", "c"]', Term2),
    json_to_atom(Term1, JSON1, [compact(true)]),
    json_to_atom(Term2, JSON2, [compact(true)]),
    JSON1 = '[1,2,3]',
    JSON2 = '["a","b","c"]'.

test_object :-
    json_from_atom('{"name": "Mario", "age": 30}', Term),
    json_to_atom(Term, JSON, [compact(true)]),
    JSON = '{"name":"Mario","age":30}'.

test_roundtrip :-
    JSON = '{"name": "Mario", "age": 30, "active": true}',
    json_from_atom(JSON, Term),
    json_to_atom(Term, JSON2, [compact(true)]),
    json_from_atom(JSON2, Term2),
    Term = Term2.

% Esegui tutti i test
run_all_tests :-
    init_test_counters,
    run_test(test_null),
    run_test(test_boolean),
    run_test(test_number),
    run_test(test_string),
    run_test(test_array),
    run_test(test_object),
    run_test(test_roundtrip),
    print_summary.

% Entry point
:- initialization(run_all_tests). 