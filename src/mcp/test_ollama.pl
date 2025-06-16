:- use_module(library(process)).
:- use_module(library(lists)).

ollama_hi :-
    % JSON richiesta per Ollama
    MessageCodes = "{\"model\":\"llama3.1:8b\", \"messages\":[{\"role\":\"user\", \"content\":\"Hi\"}]}",
    atom_codes(DataAtom, MessageCodes),

    % Chiamata HTTP con curl
    process_create(path(curl),
                   ['-s', '-X', 'POST',
                    '-H', 'Content-Type: application/json',
                    '-d', DataAtom,
                    'http://localhost:11434/api/chat'],
                   [stdout(pipe(Out)), process(PID)]),

    % Leggi tutti i codici dal pipe
    read_stream_to_codes(Out, Codes),
    close(Out),
    process_wait(PID, _),

    % Converte in atomo
    atom_codes(JsonAtom, Codes),

    % Estrae contenuti da "content":"..."
    extract_all_contents(JsonAtom, Fragments),

    % Unisce tutto in una frase
    join_with_separator(Fragments, ' ', Sentence),
    write('Messaggio ricevuto: '), write(Sentence), nl.

% Lettura completa dello stream
read_stream_to_codes(Stream, Codes) :-
    get_code(Stream, C),
    ( C == -1 -> Codes = []
    ; Codes = [C|Rest],
      read_stream_to_codes(Stream, Rest)
    ).

% Estrazione ricorsiva dei content
extract_all_contents(Json, Contents) :-
    extract_all_contents(Json, Contents, Json).

extract_all_contents(_, [], Remainder) :-
    \+ sub_atom(Remainder, _, _, _, '"content":"'), !.

extract_all_contents(Full, [Content|Rest], Remainder) :-
    sub_atom(Remainder, Start, _, _, '"content":"'),
    Pos is Start + 10,
    sub_atom(Remainder, Pos, _, AfterLen, AfterStart),
    sub_atom(AfterStart, 0, Len, _, '"'),
    sub_atom(AfterStart, 0, Len, _, Content),
    Content \= '',  % ← scarta content vuoti
    PosNext is Pos + Len + 1,
    sub_atom(Remainder, PosNext, _, 0, RemainingTail),
    extract_all_contents(Full, Rest, RemainingTail).
extract_all_contents(Full, Rest, Remainder) :-
    % Salta contenuti vuoti
    sub_atom(Remainder, Start, _, _, '"content":""'),
    Pos is Start + 10 + 2,
    sub_atom(Remainder, Pos, _, 0, RemainingTail),
    extract_all_contents(Full, Rest, RemainingTail).

% Concatena atomi con separatore
join_with_separator([], _, '').
join_with_separator([A], _, A) :- !.
join_with_separator([H|T], Sep, Result) :-
    join_with_separator(T, Sep, Partial),
    atom_concat(H, Sep, Temp),
    atom_concat(Temp, Partial, Result).