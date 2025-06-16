:- module(llm_interface, [
    query_llm/3,
    process_weather_data/2,
    process_geolocation/2,
    process_image_analysis/2
]).

:- use_module(mcp_config).
:- use_module(library(http/http_client)).
:- use_module(library(json)).

% Main interface for LLM queries
query_llm(Prompt, Context, Response) :-
    get_llm_config(Config),
    format_llm_request(Prompt, Context, Config, Request),
    make_http_request(Request, Response).

% Weather data processing
process_weather_data(Location, WeatherInfo) :-
    get_weather_api_key(APIKey),
    format_weather_request(Location, APIKey, Request),
    make_http_request(Request, RawResponse),
    parse_weather_response(RawResponse, WeatherInfo).

% Geolocation processing
process_geolocation(Location, GeoInfo) :-
    get_geolocation_api_key(APIKey),
    format_geolocation_request(Location, APIKey, Request),
    make_http_request(Request, RawResponse),
    parse_geolocation_response(RawResponse, GeoInfo).

% Image analysis processing
process_image_analysis(ImageData, Analysis) :-
    get_image_analysis_api_key(APIKey),
    format_image_analysis_request(ImageData, APIKey, Request),
    make_http_request(Request, RawResponse),
    parse_image_analysis_response(RawResponse, Analysis).

% Helper predicates
format_llm_request(Prompt, Context, Config, Request) :-
    Request = [
        method = post,
        url = 'https://api.openai.com/v1/chat/completions',
        headers = [
            'Content-Type' = 'application/json',
            'Authorization' = 'Bearer YOUR_API_KEY'
        ],
        body = json([
            model = Config.provider,
            messages = [
                [role = system, content = Context],
                [role = user, content = Prompt]
            ],
            temperature = Config.temperature,
            max_tokens = Config.max_tokens
        ])
    ].

make_http_request(Request, Response) :-
    http_post(Request.url, Request.body, Response,
              [headers(Request.headers)]).

% Response parsing
parse_weather_response(RawResponse, WeatherInfo) :-
    json_read_dict(RawResponse, Dict),
    extract_weather_info(Dict, WeatherInfo).

parse_geolocation_response(RawResponse, GeoInfo) :-
    json_read_dict(RawResponse, Dict),
    extract_geo_info(Dict, GeoInfo).

parse_image_analysis_response(RawResponse, Analysis) :-
    json_read_dict(RawResponse, Dict),
    extract_image_analysis(Dict, Analysis). 