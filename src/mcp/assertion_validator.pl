:- module(assertion_validator, [
    validate_assertion/2,
    parse_llm_response/2,
    convert_to_dali_fact/2
]).

% Assertion validation
validate_assertion(Assertion, ValidatedAssertion) :-
    check_syntax(Assertion),
    check_semantics(Assertion),
    check_consistency(Assertion),
    ValidatedAssertion = Assertion.

% Parsing LLM response into assertions
parse_llm_response(LLMResponse, Assertions) :-
    (   (atom(LLMResponse); string(LLMResponse))
    ->  catch(atom_to_term(LLMResponse, Term, _), _, Term = 'PARSING_ERROR'),
        ( Term == 'PARSING_ERROR' -> Assertions = []
        ; ( is_list(Term) -> Terms = Term ; Terms = [Term] ),
          extract_assertions(Terms, RawAssertions),
          maplist(validate_assertion, RawAssertions, Assertions)
        )
    ;   extract_assertions(LLMResponse, RawAssertions),
        maplist(validate_assertion, RawAssertions, Assertions)
    ).

% Conversion to DALI facts
convert_to_dali_fact(Assertion, DaliFact) :-
    assertion_to_fact(Assertion, DaliFact),
    validate_dali_fact(DaliFact).

% Helper predicates
check_syntax(Assertion) :-
    is_valid_syntax(Assertion),
    !.

check_semantics(Assertion) :-
    is_valid_semantics(Assertion),
    !.

check_consistency(Assertion) :-
    is_consistent(Assertion),
    !.

% Syntax validation
is_valid_syntax(Assertion) :-
    nonvar(Assertion),
    compound(Assertion),
    functor(Assertion, _, Arity),
    Arity > 0.

% Semantic validation
is_valid_semantics(Assertion) :-
    valid_predicate(Assertion),
    valid_arguments(Assertion).

% Consistency check
is_consistent(Assertion) :-
    \+ contradicts_existing(Assertion).

% Extraction of assertions from LLM response
extract_assertions(LLMResponse, Assertions) :-
    findall(Assertion,
            (member(Line, LLMResponse),
             extract_assertion_from_line(Line, Assertion)),
            Assertions).

% Extract assertion from single line/term
extract_assertion_from_line(Line, Assertion) :-
    (   compound(Line) -> Assertion = Line
    ;   atom(Line) -> 
        catch(atom_to_term(Line, Assertion, _), _, fail)
    ;   fail
    ).

% Conversion from assertion to DALI fact
assertion_to_fact(Assertion, DaliFact) :-
    convert_predicate(Assertion, DaliFact).

% DALI fact validation
validate_dali_fact(Fact) :-
    valid_dali_predicate(Fact),
    valid_dali_arguments(Fact).

% Basic validation predicates
valid_predicate(Assertion) :-
    compound(Assertion),
    functor(Assertion, Name, _),
    atom(Name).

valid_arguments(Assertion) :-
    compound(Assertion),
    Assertion =.. [_|Args],
    maplist(valid_argument, Args).

valid_argument(Arg) :-
    (atom(Arg) ; number(Arg) ; compound(Arg)).

valid_dali_predicate(Fact) :-
    compound(Fact),
    functor(Fact, Name, _),
    atom(Name).

valid_dali_arguments(Fact) :-
    compound(Fact),
    Fact =.. [_|Args],
    maplist(valid_argument, Args).

contradicts_existing(Assertion) :-
    retract(Assertion),
    assert(Assertion),
    fail.
contradicts_existing(_). 