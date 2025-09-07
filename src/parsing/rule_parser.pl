% Licensed with Apache Public License
% by AAAI Research Group
% Department of Information Engineering and Computer Science and Mathematics
% University of L'Aquila, ITALY
% http://www.disim.univaq.it

% Modular rule parsing coordinator for DALI agents
% This module coordinates the three specialized parsing modules

:- module(rule_parser, [
    aprifile/1,
    aprifile_en/1,
    take/0,
    spezza/1,
    ejec/2,
    costruisci0/1,
    examine_mul/0,
    eve_mul_first/1,
    load_program_rules/1
]).

% Import the three specialized modules with specific predicates
:- use_module('rule_file_parser', [
    aprifile/1,
    aprifile_en/1,
    take/0,
    load_program_rules/1
]).

:- use_module('rule_clause_analyzer', [
    spezza/1,
    ejec/2,
    costruisci0/1,
    examine_mul/0,
    eve_mul_first/1
]).

:- use_module('rule_event_processor', [
    ass_mul_first/0,
    recupera_funE/1,
    recupera_funI/1,
    recupera_funA/1,
    recupera_funN/1,
    recupera_funC/1,
    recupera_funG/1,
    recupera_funTG/1,
    recupera_funR/1
]).

:- use_module('../utils/dali_debug_utils').

% Print coordinator loaded message
:- trace_point('DALI Modular Rule Parser Coordinator loaded - 3 modules integrated').
:- write('DALI Modular Rule Parser loaded (3 specialized modules)'), nl.
