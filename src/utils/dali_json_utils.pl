:- module(dali_json_utils, [
    json_to_codes/2,
    json_to_codes/3,
    json_to_atom/2,
    json_to_atom/3,
    json_from_codes/2,
    json_from_codes/3,
    json_from_atom/2,
    json_from_atom/3
]).

:- use_module(library(json), [json_read/2,json_read/3,json_write/2,json_write/3]).
:- use_module(library(codesio), [open_codes_stream/2,with_output_to_codes/4]).

:-  json_to_codes/2 is det.
:-  json_to_codes/3 is det.
:-  json_to_atom/2 is det.
:-  json_to_atom/3 is det.
:-  json_from_codes/2 is det.
:-  json_from_codes/3 is det.
:-  json_from_atom/2 is det.
:-  json_from_atom/3 is det.

% Write the Term as JSON and unify JSONList with a list of the resulting character codes, using json_write(Stream, Term, Options).
json_to_codes(Term, JSONCodes, Options) :-
        with_output_to_codes(json_write(Stream, Term, Options), Stream, JSONCodes, []).

% Write the Term as JSON and unify JSONList with a list of the resulting character codes, using json_write(Stream, Term).
json_to_codes(Term, JSONCodes) :-
        with_output_to_codes(json_write(Stream, Term), Stream, JSONCodes, []).

% Write the Term as JSON and unify JSONAtom with an atom consisting of the resulting character codes, using json_write(Stream, Term, Options).
json_to_atom(Term, JSONAtom, Options) :-
        json_to_codes(Term, JSONCodes, Options),
        atom_codes(JSONAtom, JSONCodes).

% Write the Term as JSON and unify JSONAtom with an atom consisting of the resulting character codes, using json_write(Stream, Term).
json_to_atom(Term, JSONAtom) :-
        json_to_codes(Term, JSONCodes),
        atom_codes(JSONAtom, JSONCodes).

% Convert the JSON text, represented as the list of character codes JSONCodes, into the corresponding Prolog representation, using json_read(Stream, Term, Options).
json_from_codes(JSONCodes, Term, Options) :-
        open_codes_stream(JSONCodes, Stream),
        call_cleanup(json_read(Stream, Term, Options), close(Stream, [force(true)])).

% Convert the JSON text, represented as the list of character codes JSONCodes, into the corresponding Prolog representation, using json_read(Stream, Term).
json_from_codes(JSONCodes, Term) :-
        open_codes_stream(JSONCodes, Stream),
        call_cleanup(json_read(Stream, Term), close(Stream, [force(true)])).

% Convert the JSON text, represented as the character codes in JSONAtom, into the corresponding Prolog representation, using json_read(Stream, Term, Options).
json_from_atom(JSONAtom, Term, Options) :-
        atom_codes(JSONAtom, JSONCodes),
        json_from_codes(JSONCodes, Term, Options).

% Convert the JSON text, represented as the character codes in JSONAtom, into the corresponding Prolog representation, using json_read(Stream, Term).
json_from_atom(JSONAtom, Term) :-
        atom_codes(JSONAtom, JSONCodes),
        json_from_codes(JSONCodes, Term).
