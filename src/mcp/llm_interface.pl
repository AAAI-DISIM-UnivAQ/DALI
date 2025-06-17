:- module(llm_interface, [
    query_llm/3,
    process_weather_data/2,
    process_geolocation/2,
    process_image_analysis/2
]).

:- use_module('./config.pl', [
    get_llm_config/1,
    get_weather_api_key/1,
    get_geolocation_api_key/1,
    get_image_analysis_api_key/1
]).
:- use_module('../utils/dali_ollama_client').

% Main interface for LLM queries
query_llm(Prompt, Context, Response) :-
    get_llm_config(Config),
    arg(2, Config, Model),
    format_prompt(Prompt, Context, FormattedPrompt),
    ollama_chat(Model, FormattedPrompt, Response),
    write('Prompt: '), write(FormattedPrompt), nl,
    write('Response: '), write(Response), nl.

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

% Placeholder implementations for weather/geo/image APIs
format_weather_request(Location, APIKey, Request) :-
    Request = weather_request(Location, APIKey).

format_geolocation_request(Location, APIKey, Request) :-
    Request = geo_request(Location, APIKey).

format_image_analysis_request(ImageData, APIKey, Request) :-
    Request = image_request(ImageData, APIKey).

make_http_request(Request, Response) :-
    Response = '{"status":"placeholder","data":"test"}'.

extract_weather_info(Dict, WeatherInfo) :-
    WeatherInfo = weather_placeholder(Dict).

extract_geo_info(Dict, GeoInfo) :-
    GeoInfo = geo_placeholder(Dict).

extract_image_analysis(Dict, Analysis) :-
    Analysis = image_placeholder(Dict).

% Helper predicates
format_prompt(Prompt, Context, FormattedPrompt) :-
    atom_concat(Prompt, '\n\nContext: ', Temp),
    atom_concat(Temp, Context, FormattedPrompt).

% Utility per stream di memoria compatibile Sicstus
open_mem_write_stream(Stream, Atom) :-
    open_null_stream(Null),
    with_output_to(atom(Atom), (
        set_output(Null), % workaround per Sicstus, output va su atom
        true
    )),
    close(Null),
    open_atom_stream(Atom, write, Stream).

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
    Dict = json([status=placeholder, data=test]).

% HTTP response reading helper
read_http_response(Stream, Response) :-
    read_line(Stream, Line),
    read_http_headers(Stream, Headers),
    read_http_body(Stream, Headers, Body),
    Response = response(
        status(Line),
        headers(Headers),
        body(Body)
    ).

read_http_headers(Stream, []) :-
    peek_char(Stream, '\r'),
    !,
    get_char(Stream, _),
    get_char(Stream, _).
read_http_headers(Stream, [Header|Headers]) :-
    read_line(Stream, Header),
    read_http_headers(Stream, Headers).

read_http_body(Stream, Headers, Body) :-
    member(content_length(Length), Headers),
    !,
    read_n_chars(Stream, Length, Body).
read_http_body(_, _, ''). 