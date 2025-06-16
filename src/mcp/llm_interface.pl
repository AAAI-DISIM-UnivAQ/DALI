:- module(llm_interface, [
    query_llm/3,
    process_weather_data/2,
    process_geolocation/2,
    process_image_analysis/2
]).

:- use_module(mcp_config).
:- use_module(library(sockets)).  % SICStus socket library
:- use_module(library(lists)).

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
        body = [
            model = Config.provider,
            messages = [
                [role = system, content = Context],
                [role = user, content = Prompt]
            ],
            temperature = Config.temperature,
            max_tokens = Config.max_tokens
        ]
    ].

make_http_request(Request, Response) :-
    socket_client_open('api.openai.com:443', Stream, []),
    format(Stream, 'POST ~w HTTP/1.1\r\n', [Request.url]),
    format(Stream, 'Host: api.openai.com\r\n', []),
    format(Stream, 'Content-Type: ~w\r\n', [Request.headers.'Content-Type']),
    format(Stream, 'Authorization: ~w\r\n', [Request.headers.Authorization]),
    format(Stream, '\r\n', []),
    format(Stream, '~w', [Request.body]),
    flush_output(Stream),
    read_http_response(Stream, Response),
    close(Stream).

% Response parsing
parse_weather_response(RawResponse, WeatherInfo) :-
    parse_json(RawResponse, Dict),
    extract_weather_info(Dict, WeatherInfo).

parse_geolocation_response(RawResponse, GeoInfo) :-
    parse_json(RawResponse, Dict),
    extract_geo_info(Dict, GeoInfo).

parse_image_analysis_response(RawResponse, Analysis) :-
    parse_json(RawResponse, Dict),
    extract_image_analysis(Dict, Analysis).

% JSON parsing helper
parse_json(JSONString, Dict) :-
    atom_codes(JSONString, Codes),
    phrase(json(Dict), Codes).

% HTTP response reading helper
read_http_response(Stream, Response) :-
    read_line(Stream, Line),
    read_http_headers(Stream, Headers),
    read_http_body(Stream, Headers, Body),
    Response = [status = Line, headers = Headers, body = Body].

read_http_headers(Stream, []) :-
    peek_char(Stream, '\r'),
    !,
    get_char(Stream, _),
    get_char(Stream, _).
read_http_headers(Stream, [Header|Headers]) :-
    read_line(Stream, Header),
    read_http_headers(Stream, Headers).

read_http_body(Stream, Headers, Body) :-
    member(content_length = Length, Headers),
    !,
    read_n_chars(Stream, Length, Body).
read_http_body(_, _, ''). 