:- module(llm_interface, [
    query_llm/3,
    process_weather_data/2,
    process_geolocation/2,
    process_image_analysis/2
]).

:- use_module('./config.pl', [get_llm_config/1]).
:- use_module(library(sockets)).
:- use_module(library(lists)).

% Main interface for LLM queries
query_llm(Prompt, Context, Response) :-
    get_llm_config(Config),
    format_ollama_request(Prompt, Context, Config, Request),
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
format_ollama_request(Prompt, Context, Config, Request) :-
    arg(2, Config, Model),
    arg(3, Config, Temperature),
    arg(4, Config, MaxTokens),
    Request = request(
        method(post),
        url('http://localhost:11434/api/generate'),
        headers([content_type('application/json')]),
        body([
            model(Model),
            prompt(Context),
            system(Prompt),
            temperature(Temperature),
            max_tokens(MaxTokens)
        ])
    ).

make_http_request(Request, Response) :-
    arg(2, Request, URL),
    arg(3, Request, Headers),
    arg(4, Request, Body),
    get_llm_config(Config),
    arg(5, Config, BaseURL),
    arg(1, BaseURL, URLString),
    extract_host_port(URLString, Host0, Port),
    (atom(Host0) -> Host = Host0 ; atom_string(Host, Host0)),
    format('DEBUG: Host=~w, Port=~w~n', [Host, Port]),
    catch(
        (socket(internet, stream, Socket),
         socket_connect(Socket, Host:Port),
         socket_stream(Socket, Stream),
         format(Stream, 'POST ~w HTTP/1.1\r\n', [URL]),
         format(Stream, 'Host: ~w\r\n', [Host]),
         nth0(0, Headers, ContentType),
         arg(1, ContentType, ContentTypeValue),
         format(Stream, 'Content-Type: ~w\r\n', [ContentTypeValue]),
         format(Stream, '\r\n', []),
         format(Stream, '~w', [Body]),
         flush_output(Stream),
         read_http_response(Stream, Response),
         close(Stream),
         socket_close(Socket)),
        Error,
        (format(user_error, 'Error in HTTP request: ~w~n', [Error]),
         fail)
    ).

% Helper predicate to extract host and port from URL
extract_host_port(URL, Host, Port) :-
    atom_codes(URL, Codes),
    phrase(parse_url(Host, Port), Codes).

parse_url(Host, Port) -->
    "http://",
    host_chars(HostChars),
    {atom_codes(Host, HostChars)},
    (":", integer_chars(PortChars), {number_codes(Port, PortChars)} -> []; {Port = 80}).

host_chars([C|Cs]) -->
    [C],
    {C \= 0':, C \= 0'/, C \= 0'?},
    host_chars(Cs).
host_chars([]) --> [].

integer_chars([C|Cs]) -->
    [C],
    {C >= 0'0, C =< 0'9},
    integer_chars(Cs).
integer_chars([]) --> [].

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