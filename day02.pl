:- module(day02, [day02_solve/0]).

:- use_module(util).

%%  day02_solve
%
%   Prints answers for day 2.
day02_solve :-
    read_file_to_string(input('day02.txt'), S, []),
    string_lines(S, Lines),
    maplist(parse_game, Lines, Games),
    part1(Games, Answer1),
    write(Answer1), nl,
    part2(Games, Answer2),
    write(Answer2), nl.

%%  part1(+Games, -Answer)
%
%   Answer is the answer for part 1 with Games.
part1(Games, Answer) :-
    include(game_possible, Games, Possibles),
    maplist(game_id, Possibles, IDs),
    sum_list(IDs, Answer).

%%  parse_game(+Line, -Game)
%
%   Game is the parsed game from Line.
parse_game(Line, Game) :-
    string_chars(Line, Chars),
    phrase(game(Game), Chars),
    !.

%%  game_id(+Game, -ID)
%
%   ID is the ID of the Game.
game_id(game(ID, _), ID).

%%  game_possible(+Game)
%
%   True if Game is possible.
game_possible(game(_, Sets)) :-
    maplist(set_possible, Sets).

%%  set_possible(+Set)
%
%   True if the Set of cubes is possible.
set_possible(Set) :-
    maplist(cube_possible, Set).

%%  cube_possible(+Cube)
%
%   True if the Cube is possible.
cube_possible(cube(N, red)) :-
    N =< 12.
cube_possible(cube(N, green)) :-
    N =< 13.
cube_possible(cube(N, blue)) :-
    N =< 14.

%%  part2(+Games, -Answer)
%
%   Answer is the answer for part 2 with Games.
part2(Games, Answer) :-
    maplist(game_power, Games, Powers),
    sum_list(Powers, Answer).

%%  game_power(+Game, -Power)
%
%   Power is the value of power of the sets of cubes in the Game.
game_power(game(_, Sets), Power) :-
    maplist(set_num_colors, Sets, ColorsList),
    max_colors(ColorsList, [R, G, B]),
    Power is R * G * B.

%%  set_num_colors(+Set, -Colors)
%
%   Colors is the numbers of each cube color in the game Set.
%   Colors is a list of numbers of 3 cube colors, red, green, and blue.
set_num_colors(Set, [N_red, N_green, N_blue]) :-
    num_color(Set, red, N_red),
    num_color(Set, green, N_green),
    num_color(Set, blue, N_blue).

%%  num_color(+Set, +Color, -Num)
%
%   Num is the number of cubes of the given Color in the game Set.
%   Color is one of red, green, and blue.
num_color(Set, Color, Num) :-
    (   memberchk(cube(N, Color), Set)
    ->  Num = N
    ;   Num = 0
    ).

%%  max_colors(+ColorsList, -Colors)
%
%   Colors is the list of the maximum number of cubes in each color
%   in the given list of colors, ColorsList.
%   ColorsList looks like [[R1,G1,B1],[R2,G2,B2],...].
%   Colors looks like [R_max,G_max,B_max].
max_colors(ColorsList, Colors) :-
    foldl(maplist([X,Acc0,Acc]>>(Acc is max(X, Acc0))),
          ColorsList,
          [0,0,0],
          Colors).

% Parsers.

game(game(ID, Sets)) -->
    ['G',a,m,e], sp, number(ID), [:], sp, sets(Sets).

sets([Set]) --> set(Set).
sets([Set|Sets]) --> set(Set), [';'], sp, sets(Sets).

set([cube(N, Color)]) --> cube(N, Color).
set([cube(N, Color)|Cubes]) -->
    cube(N, Color), comma, sp, set(Cubes).

cube(N, Color) --> number(N), sp, color(Color).

color(red) --> [r,e,d].
color(green) --> [g,r,e,e,n].
color(blue) --> [b,l,u,e].

number(N) --> digits(Ds), { number_chars(N, Ds) }.

digits([D]) --> digit(D).
digits([D|Ds]) --> digit(D), digits(Ds).

digit(C) --> [C], { char_type(C, digit) }.

sp --> [' '].
comma --> [','].
