:- module(test_ollama, [
    ollama_hi/0
]).

:- use_module('../utils/dali_ollama_client').

% test with:
% /usr/local/sicstus4.6.0/bin/sicstus -l src/mcp/test_ollama.pl --goal "leash(off), trace, ollama_hi, notrace."

ollama_hi :-
    ollama_chat('llama3.1:8b', 'Write a Prolog program that prints "Hello, world!"', Response),
    write('Messaggio completo ricevuto: '), write(Response), nl.

