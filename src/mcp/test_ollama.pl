:- module(test_ollama, [
    ollama_hi/0,
    read_streaming_response/2,
    read_stream_to_codes/2,
    extract_content/2
]).

:- use_module(library(process)).
:- use_module('../utils/dali_json_utils').
:- use_module('../utils/dali_list_utils').

% test with:
% /usr/local/sicstus4.6.0/bin/sicstus -l src/mcp/test_ollama.pl --goal "leash(off), trace, ollama_hi, notrace."

% Lettura completa dello stream
read_stream_to_codes(Stream, Codes) :-
    get_code(Stream, C),
    ( C == -1 -> Codes = []
    ; Codes = [C|Rest],
      read_stream_to_codes(Stream, Rest)
    ).

% Estrazione del contenuto dalla risposta JSON
extract_content(json(Response), Content) :-
    member(message=json(Message), Response),
    member(content=Content, Message).

% Lettura e processamento della risposta streaming
read_streaming_response(Stream, Content) :-
    findall(Token,
            (read_stream_to_codes(Stream, Codes),
             Codes \= [],
             json_from_codes(Codes, Response),
             extract_content(Response, Token),
             \+ (member(done='@true', Response))),
            Tokens),
    dali_list_utils:atomic_list_concat(Tokens, Content).

ollama_hi :-
    % Creazione della struttura JSON per la richiesta
    Request = json([
        model='llama3.1:8b',
        messages=[json([
            role='user',
            content='Hi'
        ])]
    ]),
    
    % Conversione della struttura in JSON
    json_to_atom(Request, DataAtom),

    % Chiamata HTTP con curl
    process_create(path(curl),
                   ['-s', '-X', 'POST',
                    '-H', 'Content-Type: application/json',
                    '-d', DataAtom,
                    'http://localhost:11434/api/chat'],
                   [stdout(pipe(Out)), process(PID)]),

    % Leggi e processa la risposta streaming
    read_streaming_response(Out, Content),
    close(Out),
    process_wait(PID, _),
    
    write('Messaggio ricevuto: '), write(Content), nl.

