:- module(test_mcp, []).

:- use_module(llm_interface).
:- use_module(assertion_validator).

% Test predicates
test_weather_analysis :-
    write('Testing weather analysis...'), nl,
    process_weather_data('Rome', WeatherInfo),
    write('Weather info received: '), write(WeatherInfo), nl.

test_geolocation_analysis :-
    write('Testing geolocation analysis...'), nl,
    process_geolocation('Rome', GeoInfo),
    write('Geolocation info received: '), write(GeoInfo), nl.

test_image_analysis :-
    write('Testing image analysis...'), nl,
    process_image_analysis('test_image.jpg', ImageInfo),
    write('Image analysis info received: '), write(ImageInfo), nl.

% Run all tests
run_all_tests :-
    write('Starting MCP tests...'), nl,
    test_weather_analysis,
    test_geolocation_analysis,
    test_image_analysis,
    write('All tests completed.'), nl. 