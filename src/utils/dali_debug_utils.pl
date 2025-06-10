% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Debug and logging utilities for DALI agents

:- module(dali_debug_utils, [
    trace_point/1,
    trace_point/2,
    save_on_log_file/1,
    debug_on/0
]).

:- dynamic debug_on/0.

%% Debug tracing
trace_point(Message) :-
    (debug_on ->
        (write('DEBUG: '), write(Message), nl)
    ;
        true
    ).

trace_point(Message, Args) :-
    (debug_on ->
        (write('DEBUG: '), write(Message), write(' - '), write(Args), nl)
    ;
        true
    ).

%% Save predicate to agent log file
save_on_log_file(P) :-
    clause(agent(N), _),
    name('log/log_', L0),
    name(N, L1),
    name('.txt', L2),
    append(L0, L1, L01),
    append(L01, L2, L02),
    name(Q, L02),
    open(Q, append, Stream, []),
    write(Stream, P),
    nl(Stream),
    close(Stream). 