:- module(mcp_config, [
    get_llm_config/1,
    get_weather_api_key/1,
    get_geolocation_api_key/1,
    get_image_analysis_api_key/1
]).

% Base configuration for LLM services
get_llm_config(Config) :-
    Config = [
        provider = 'openai',
        model = 'gpt-4',
        temperature = 0.7,
        max_tokens = 1000
    ].

% API keys for various services
get_weather_api_key('YOUR_WEATHER_API_KEY').
get_geolocation_api_key('YOUR_GEOLOCATION_API_KEY').
get_image_analysis_api_key('YOUR_IMAGE_ANALYSIS_API_KEY').

% Caching system configuration
:- dynamic cache_entry/3.
cache_ttl(3600). % 1 hour in seconds 