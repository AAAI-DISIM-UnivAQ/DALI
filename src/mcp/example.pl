:- module(mcp_example, [
    example_weather_analysis/0,
    example_geolocation_analysis/0,
    example_image_analysis/0
]).

:- use_module(llm_interface).
:- use_module(assertion_validator).

% Weather analysis example
example_weather_analysis :-
    process_weather_data('Rome', WeatherInfo),
    format_llm_prompt(WeatherInfo, Prompt),
    query_llm(Prompt, 'Analyze weather conditions and generate logical assertions', Response),
    parse_llm_response(Response, Assertions),
    maplist(convert_to_dali_fact, Assertions, DaliFacts),
    assert_dali_facts(DaliFacts).

% Geolocation analysis example
example_geolocation_analysis :-
    process_geolocation('Rome', GeoInfo),
    format_llm_prompt(GeoInfo, Prompt),
    query_llm(Prompt, 'Analyze location data and generate logical assertions', Response),
    parse_llm_response(Response, Assertions),
    maplist(convert_to_dali_fact, Assertions, DaliFacts),
    assert_dali_facts(DaliFacts).

% Image analysis example
example_image_analysis :-
    process_image_analysis('path/to/image.jpg', ImageInfo),
    format_llm_prompt(ImageInfo, Prompt),
    query_llm(Prompt, 'Analyze image content and generate logical assertions', Response),
    parse_llm_response(Response, Assertions),
    maplist(convert_to_dali_fact, Assertions, DaliFacts),
    assert_dali_facts(DaliFacts).

% Helper predicates
format_llm_prompt(Info, Prompt) :-
    format(string(Prompt), 'Analyze the following information and generate logical assertions: ~w', [Info]).

assert_dali_facts([]).
assert_dali_facts([Fact|Facts]) :-
    assert(Fact),
    assert_dali_facts(Facts). 