% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Time and date utilities for DALI agents

:- module(dali_time_utils, [
    calcola_time/2,
    calcola_date/2,
    dali_datime/1,
    dali_now/1
]).

:- use_module(library(system)).

%% Get current time
dali_now(Time) :-
    statistics(walltime, [Time, _]).

%% Get current date/time structure
dali_datime(DateTime) :-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, local).

%% Convert numeric time to time structure
calcola_time(D, Dn) :-
    R is integer(D),
    name(R, L),
    length(L, N),
    (N = 6 ->
        calcola_time_6(L, Dn)
    ;
        calcola_time_tree1(L, N, Dn)
    ).

calcola_time_tree1(L, N, Dn) :-
    (N = 5 ->
        calcola_time_5(L, Dn)
    ;
        calcola_time_tree2(L, N, Dn)
    ).

calcola_time_tree2(L, N, Dn) :-
    (N = 4 ->
        calcola_time_4(L, Dn)
    ;
        calcola_time_tree3(L, N, Dn)
    ).

calcola_time_tree3(L, N, Dn) :-
    (N = 3 ->
        calcola_time_3(L, Dn)
    ;
        calcola_time_tree4(L, N, Dn)
    ).

calcola_time_tree4(L, N, Dn) :-
    (N = 2 ->
        calcola_time_2(L, Dn)
    ;
        calcola_time_tree5(L, N, Dn)
    ).

calcola_time_tree5(L, N, Dn) :-
    (N = 1 ->
        calcola_time_1(L, Dn)
    ;
        true
    ).

calcola_time_6(L, Dn) :-
    nth1(1, L, H1), nth1(2, L, H2), 
    nth1(3, L, M1), nth1(4, L, M2),
    nth1(5, L, S1), nth1(6, L, S2),
    name('time(', W0), name(')', U0),
    append([H1], [H2], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_time_5(L, Dn) :-
    nth1(1, L, H2), nth1(2, L, M1), 
    nth1(3, L, M2), nth1(4, L, S1), nth1(5, L, S2),
    name('time(', W0), name(')', U0),
    append([48], [H2], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_time_4(L, Dn) :-
    nth1(1, L, M1), nth1(2, L, M2),
    nth1(3, L, S1), nth1(4, L, S2),
    name('time(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_time_3(L, Dn) :-
    nth1(1, L, M2), nth1(2, L, S1), nth1(3, L, S2),
    name('time(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_time_2(L, Dn) :-
    nth1(1, L, S1), nth1(2, L, S2),
    name('time(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [48], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_time_1(L, Dn) :-
    nth1(1, L, S2),
    name('time(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [48], HM2),
    append(HM2, [44], HMv), append(HMv, [48], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

%% Convert numeric date to date structure
calcola_date(D, Dn) :-
    name(D, L),
    length(L, N),
    (N = 6 ->
        calcola_date_6(L, Dn)
    ;
        calcola_date_tree1(L, N, Dn)
    ).

calcola_date_tree1(L, N, Dn) :-
    (N = 5 ->
        calcola_date_5(L, Dn)
    ;
        calcola_date_tree2(L, N, Dn)
    ).

calcola_date_tree2(L, N, Dn) :-
    (N = 4 ->
        calcola_date_4(L, Dn)
    ;
        calcola_date_tree3(L, N, Dn)
    ).

calcola_date_tree3(L, N, Dn) :-
    (N = 3 ->
        calcola_date_3(L, Dn)
    ;
        calcola_date_tree4(L, N, Dn)
    ).

calcola_date_tree4(L, N, Dn) :-
    (N = 2 ->
        calcola_date_2(L, Dn)
    ;
        calcola_date_tree5(L, N, Dn)
    ).

calcola_date_tree5(L, N, Dn) :-
    (N = 1 ->
        calcola_date_1(L, Dn)
    ;
        true
    ).

calcola_date_6(L, Dn) :-
    nth1(1, L, H1), nth1(2, L, H2),
    nth1(3, L, M1), nth1(4, L, M2),
    nth1(5, L, S1), nth1(6, L, S2),
    name('date(', W0), name(')', U0),
    append([H1], [H2], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_date_5(L, Dn) :-
    nth1(1, L, H2), nth1(2, L, M1),
    nth1(3, L, M2), nth1(4, L, S1), nth1(5, L, S2),
    name('date(', W0), name(')', U0),
    append([48], [H2], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_date_4(L, Dn) :-
    nth1(1, L, M1), nth1(2, L, M2),
    nth1(3, L, S1), nth1(4, L, S2),
    name('date(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [M1], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_date_3(L, Dn) :-
    nth1(1, L, M2), nth1(2, L, S1), nth1(3, L, S2),
    name('date(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [M2], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_date_2(L, Dn) :-
    nth1(1, L, S1), nth1(2, L, S2),
    name('date(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [48], HM2),
    append(HM2, [44], HMv), append(HMv, [S1], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf).

calcola_date_1(L, Dn) :-
    nth1(1, L, S2),
    name('date(', W0), name(')', U0),
    append([48], [48], H), append(H, [44], Hv),
    append(Hv, [48], HM1), append(HM1, [48], HM2),
    append(HM2, [44], HMv), append(HMv, [48], HMS1),
    append(HMS1, [S2], HMS2),
    append(W0, HMS2, P0), append(P0, U0, HMSf),
    name(Dn, HMSf). 