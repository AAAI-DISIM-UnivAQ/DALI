:- module(assertion_validator, [
    validate_assertion/2,
    parse_llm_response/2,
    convert_to_dali_fact/2
]).

:- use_module(library(error)).

% Assertion validation
validate_assertion(Assertion, ValidatedAssertion) :-
    check_syntax(Assertion),
    check_semantics(Assertion),
    check_consistency(Assertion),
    ValidatedAssertion = Assertion.

% Parsing LLM response into assertions
parse_llm_response(LLMResponse, Assertions) :-
    extract_assertions(LLMResponse, RawAssertions),
    maplist(validate_assertion, RawAssertions, Assertions).

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

% Conversion from assertion to DALI fact
assertion_to_fact(Assertion, DaliFact) :-
    convert_predicate(Assertion, DaliFact).

% DALI fact validation
validate_dali_fact(Fact) :-
    valid_dali_predicate(Fact),
    valid_dali_arguments(Fact). 