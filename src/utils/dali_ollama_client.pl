:- module(dali_ollama_client, [
    ollama_chat/3,
    read_streaming_response/2,
    read_line_to_codes/2,
    extract_content/2,
    atomic_list_concat/2
]).

:- use_module(library(process)).
:- use_module('../utils/dali_json_utils').

% Lettura di una singola riga dallo stream
read_line_to_codes(Stream, Codes) :-
    get_code(Stream, C),
    ( C == -1 -> Codes = []
    ; C == 10 -> Codes = []  % newline
    ; Codes = [C|Rest],
      read_line_to_codes(Stream, Rest)
    ).

% Lettura di una singola risposta JSON dallo stream
read_single_response(Stream, Response) :-
    read_line_to_codes(Stream, Codes),
    Codes \= [],
    json_from_codes(Codes, Response).

% Estrazione del contenuto dalla risposta JSON
extract_content(json(Response), Content) :-
    member(message=json(Message), Response),
    member(content=Content, Message).

% Lettura e processamento della risposta streaming
read_streaming_response(Stream, Content) :-
    read_streaming_response(Stream, [], Content).

read_streaming_response(Stream, Acc, Content) :-
    read_single_response(Stream, Response), !,
    ( member(done= @(true), Response) ->
        reverse(Acc, Tokens),
        atomic_list_concat(Tokens, Content)
    ; extract_content(Response, Token),
      read_streaming_response(Stream, [Token|Acc], Content)
    ).
read_streaming_response(_Stream, Acc, Content) :-
    reverse(Acc, Tokens),
    atomic_list_concat(Tokens, Content).

% Custom implementation of reverse/2
reverse(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).

% Custom implementation of atomic_list_concat/2
atomic_list_concat([], '').
atomic_list_concat([H|T], Result) :-
    atomic_list_concat(T, Rest),
    atom_concat(H, Rest, Result).

% Predicato principale per la chat con Ollama
ollama_chat(Model, Message, Response) :-
    Request = json([
        model=Model,
        messages=[json([
            role='user',
            content=Message
        ])]
    ]),
    json_to_atom(Request, DataAtom),
    process_create(path(curl),
                   ['-s', '-X', 'POST',
                    '-H', 'Content-Type: application/json',
                    '-d', DataAtom,
                    'http://localhost:11434/api/chat'],
                   [stdout(pipe(Out)), process(PID)]),
    read_streaming_response(Out, Response),
    close(Out),
    process_wait(PID, _).
