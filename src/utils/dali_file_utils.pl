% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% File utility predicates for DALI agents

:- module(dali_file_utils, [
    pl_from_name/2,
    ple_from_name/2,
    plv_from_name/2,
    plf_from_name/2,
    txt_from_name/2,
    log_from_name/2,
    delete_agent_files/1,
    delete_agent_log_file/1
]).

% Note: file_systems library not available in all SWI-Prolog versions
% :- use_module(library(file_systems)).

%% Generate file names with different extensions
pl_from_name(Base, PlFile) :-
    name(Base, L),
    append(L, [46,112,108], Lpl),  % .pl
    name(PlFile, Lpl).

ple_from_name(Base, PleFile) :-
    name(Base, L),
    append(L, [46,112,108,101], Lple),  % .ple
    name(PleFile, Lple).

plv_from_name(Base, PlvFile) :-
    name(Base, L),
    append(L, [46,112,108,118], Lplv),  % .plv
    name(PlvFile, Lplv).

plf_from_name(Base, PlfFile) :-
    name(Base, L),
    append(L, [46,112,108,102], Lplf),  % .plf
    name(PlfFile, Lplf).

txt_from_name(Base, TxtFile) :-
    name(Base, L),
    append(L, [46,116,120,116], Ltxt),  % .txt
    name(TxtFile, Ltxt).

log_from_name(Name, LogFile) :-
    name('log/log_', L0),
    name(Name, L1),
    name('.txt', L2),
    append(L0, L1, L01),
    append(L01, L2, L02),
    name(LogFile, L02).

%% Delete agent-related files
delete_agent_files(File) :-
    pl_from_name(File, PlFile),
    ple_from_name(File, PleFile),
    plv_from_name(File, PlvFile),
    plf_from_name(File, PlfFile),
    (file_exists(PlFile) -> delete_file(PlFile); true),
    (file_exists(PleFile) -> delete_file(PleFile); true),
    (file_exists(PlvFile) -> delete_file(PlvFile); true),
    (file_exists(PlfFile) -> delete_file(PlfFile); true).

delete_agent_log_file(AgentName) :-
    name('log/log_', L0),
    name(AgentName, L1),
    name('.txt', L2),
    append(L0, L1, L01),
    append(L01, L2, L02),
    name(LogFile, L02),
    (file_exists(LogFile) -> delete_file(LogFile); true).

%% Note: aprifile_res/1, leggiFile/2, and tokenize/2 are provided by the original DALI files
%% included in agent_init.pl (tokefun.pl, memory.pl, etc.)

%% Define file_exists/1 for compatibility between SWI-Prolog and SICStus
file_exists(File) :-
    catch(
        (exists_file(File) -> true; (open(File, read, Stream, []), close(Stream))),
        _,
        fail
    ). 