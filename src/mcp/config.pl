:- module(mcp_config, [
    get_llm_config/1,
    get_weather_api_key/1,
    get_geolocation_api_key/1,
    get_image_analysis_api_key/1
]).

:- use_module(library(system)).

% Set the following environment variables to test the MCP
%    export WEATHER_API_KEY="test_key"
%    export GEOLOCATION_API_KEY="test_key" 
%    export IMAGE_ANALYSIS_API_KEY="test_key"

% Base configuration for LLM services
get_llm_config(Config) :-
    Config = config(
        provider(ollama),
        'llama3.1:8b',
        temperature(0.7),
        max_tokens(1000),
        base_url('http://localhost:11434')
    ).

% API keys from environment variables
get_weather_api_key(APIKey) :-
    (   environ('WEATHER_API_KEY', APIKey)
    ->  true
    ;   write('Warning: WEATHER_API_KEY environment variable not set'), nl,
        fail
    ).

get_geolocation_api_key(APIKey) :-
    (   environ('GEOLOCATION_API_KEY', APIKey)
    ->  true
    ;   write('Warning: GEOLOCATION_API_KEY environment variable not set'), nl,
        fail
    ).

get_image_analysis_api_key(APIKey) :-
    (   environ('IMAGE_ANALYSIS_API_KEY', APIKey)
    ->  true
    ;   write('Warning: IMAGE_ANALYSIS_API_KEY environment variable not set'), nl,
        fail
    ).

% Caching system configuration
:- dynamic cache_entry/3.
cache_ttl(3600). % 1 hour in seconds 
