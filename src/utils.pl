% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it


pl_from_name(Name, NameExt):- name(Name,L), append(L,[46,112,108],L1), name(NameExt,L1).
plv_from_name(Name, NameExt):- name(Name,L), append(L,[46,112,108,118],L1), name(NameExt,L1).
ple_from_name(Name, NameExt):- name(Name,L), append(L,[46,112,108,101],L1), name(NameExt,L1).
plf_from_name(Name, NameExt):- name(Name,L), append(L,[46,112,108,102],L1), name(NameExt,L1).
txt_from_name(Name, NameExt):- name(Name,L), append(L,[46,116,120,116],L1), name(NameExt,L1).
log_from_name(Name, NameExt):- name('log/log_',L0), name(Name,L1), name('.txt',L2), append(L0,L1,L01), append(L01,L2,L02), name(NameExt,L02).

%% Eliminates agent files except plf
delete_agent_files(F):-
        pl_from_name(F, Pl),    if(file_exists(Pl),     delete_file(Pl),        true),
        plv_from_name(F, Plv),  if(file_exists(Plv),    delete_file(Plv),       true),
        ple_from_name(F, Ple),  if(file_exists(Ple),    delete_file(Ple),       true).

delete_agent_log_file(Name):-
        log_from_name(Name,Log), if(file_exists(Log),delete_file(Log),true).