:- module(day05, [day05_solve/0]).

:- use_module(util).

:- dynamic mapping/5, seed_range/2.

%%  day05_solve
%
%   Prints answers for day 5.
day05_solve :-
    retractall(mapping(_,_,_,_,_)),
    retractall(seed_range(_,_)),

    read_file_to_string(input('day05.txt'), S, []),
    string_lines(S, Lines),
    parse_input(Lines, Seeds),
    part1(Seeds, Answer1),
    write(Answer1), nl,
    part2(Seeds, Answer2),
    write(Answer2), nl.

%%  part1(+Seeds, -Answer)
%
%   Answer is the answer for part 1, with Seeds as the input.
part1(Seeds, Answer) :-
    maplist(map_to_location, Seeds, Locations),
    min_list(Locations, Answer).

%%  map_to_location(+Seed, -Location)
%
%   True if there is a mapping from Seed to Location.
map_to_location(Seed, Location) :-
    map(seed, soil, Seed, Soil),
    map(soil, fertilizer, Soil, Fertilizer),
    map(fertilizer, water, Fertilizer, Water),
    map(water, light, Water, Light),
    map(light, temperature, Light, Temperature),
    map(temperature, humidity, Temperature, Humidity),
    map(humidity, location, Humidity, Location).

%%  map(+FromName, +ToName, ?From, ?To)
%
%   True if there is a mapping from FromName to ToName that maps From to To.
%   Either From or To must be instantiated.
map(FromName, ToName, From, To) :-
    mapping(FromName, ToName, FromStart, ToStart, Len),
    (   var(To)
    ->  FromStart =< From,
        From < FromStart + Len,
        !,
        Diff is From - FromStart,
        To is ToStart + Diff
    ;   ToStart =< To,
        To < ToStart + Len,
        !,
        Diff is To - ToStart,
        From is FromStart + Diff
    ).
map(_, _, From, From).

%%  part2(+Seeds, -Answer)
%
%   Answer is the answer for part 2, with Seeds as the input.
part2(Seeds, Answer) :-
    translate_seeds(Seeds),
    find_lowest_location(0, Answer).

%%  translate_seeds(+Seeds)
%
%   Translates the given seed numbers according to the rule of part 2
%   I.e., each pair represents the seed start and the range.
translate_seeds([S1,S2|T]) :-
    !,
    assertz(seed_range(S1, S2)),
    translate_seeds(T).
translate_seeds(_).

%%  find_lowest_location(+N, -Result)
%
%   Finds the lowest location number, Result, starting from N, that maps to a
%   seed.
find_lowest_location(N, N) :-
    map_to_seed(N, Seed),
    seed_range(Start, Len),
    Start =< Seed,
    Seed < Start + Len,
    !.
find_lowest_location(N, Result) :-
    N1 is N + 1,
    find_lowest_location(N1, Result).

%%  map_to_seed(+Location, -Seed)
%
%   True if there is a mapping back from Location to Seed.
map_to_seed(Location, Seed) :-
    map(humidity, location, Humidity, Location),
    map(temperature, humidity, Temperature, Humidity),
    map(light, temperature, Light, Temperature),
    map(water, light, Water, Light),
    map(fertilizer, water, Fertilizer, Water),
    map(soil, fertilizer, Soil, Fertilizer),
    map(seed, soil, Seed, Soil).

%%  parse_input(+Lines, -Seeds)
%
%   Parses the given input, Lines.
%   Seeds is the list of seeds from the input.
parse_input(Lines, Seeds) :-
    append([[SeedLine], [""],
            ["seed-to-soil map:"], Chunk1, [""],
            ["soil-to-fertilizer map:"], Chunk2, [""],
            ["fertilizer-to-water map:"], Chunk3, [""],
            ["water-to-light map:"], Chunk4, [""],
            ["light-to-temperature map:"], Chunk5, [""],
            ["temperature-to-humidity map:"], Chunk6, [""],
            ["humidity-to-location map:"], Chunk7
           ],
           Lines),
    !,
    parse_seeds_line(SeedLine, Seeds),
    parse_mapping_lines(seed, soil, Chunk1),
    parse_mapping_lines(soil, fertilizer, Chunk2),
    parse_mapping_lines(fertilizer, water, Chunk3),
    parse_mapping_lines(water, light, Chunk4),
    parse_mapping_lines(light, temperature, Chunk5),
    parse_mapping_lines(temperature, humidity, Chunk6),
    parse_mapping_lines(humidity, location, Chunk7).

%%  parse_seeds_line(+Line, -Seeds)
%
%   Parses the given seed line. Seeds is the list of seeds.
parse_seeds_line(Line, Seeds) :-
    atomic_list_concat(['seeds:'|Strs], ' ', Line),
    maplist(atom_number, Strs, Seeds).

%%  parse_mapping_lines(+FromName, +ToName, +Lines)
%
%   Parses lines that map FromName to ToName.
parse_mapping_lines(_, _, []) :- !.
parse_mapping_lines(FromName, ToName, [Line|Lines]) :-
    atomic_list_concat([ToStr, FromStr, LenStr], ' ', Line),
    atom_number(ToStr, To),
    atom_number(FromStr, From),
    atom_number(LenStr, Len),
    assertz(mapping(FromName, ToName, From, To, Len)),
    parse_mapping_lines(FromName, ToName, Lines).
