:- module(day11, [day11_solve/0]).

:- use_module(util).

:- dynamic galaxy/2.

%%  day11_solve
%
%   Prints answers for day 11.
day11_solve :-
    part1(Answer1),
    write(Answer1), nl,
    part2(Answer2),
    write(Answer2), nl.

part1(Answer) :-
    parse_input,
    expand(2, Answer).

part2(Answer) :-
    parse_input,
    expand(1_000_000, Answer).

parse_input :-
    retractall(galaxy(_,_)),
    read_file_to_string(input('day11.txt'), S, []),
    string_lines(S, Lines),
    parse_lines(0, Lines).

expand(Delta, Answer) :-
    max_x_y(Xmax, Ymax),
    expand_universe(Xmax, Ymax, Delta),
    findall(X-Y, galaxy(X, Y), Positions),
    distances(Positions, [], Distances),
    sum_list(Distances, Answer).

parse_lines(_, []) :- !.
parse_lines(Y, [Line|Lines]) :-
    string_chars(Line, Chars),
    parse_line(0, Y, Chars),
    Y1 is Y + 1,
    parse_lines(Y1, Lines).

parse_line(_, _, []) :- !.
parse_line(X, Y, [Char|Chars]) :-
    (   Char = '#'
    ->  assertz(galaxy(X, Y))
    ;   true
    ),
    X1 is X + 1,
    parse_line(X1, Y, Chars).

max_x_y(Xmax, Ymax) :-
    findall(X, galaxy(X, _), Xs),
    findall(Y, galaxy(_, Y), Ys),
    max_list(Xs, Xmax),
    max_list(Ys, Ymax).

expand_universe(Xmax, Ymax, Delta) :-
    expand_universe_x(Xmax, Delta, 0),
    expand_universe_y(Ymax, Delta, 0).

expand_universe_x(Xmax, _, X) :-
    X > Xmax, !.
expand_universe_x(Xmax, Delta, X) :-
    (   galaxy(X, _)
    ->  X1 is X + 1,
        expand_universe_x(Xmax, Delta, X1)
    ;   expand_right_of_x(X, Delta),
        X1 is X + Delta,
        Xmax1 is Xmax + Delta - 1,
        expand_universe_x(Xmax1, Delta, X1)
    ).

expand_right_of_x(X, Delta) :-
    findall(Xg-Yg, galaxy(Xg, Yg), Positions),
    include({X}/[Xg-_]>>(Xg >= X), Positions, ToRights),
    maplist({Delta}/[Xg-Yg]>>(
                retractall(galaxy(Xg, Yg)),
                Xg1 is Xg + Delta - 1,
                assertz(galaxy(Xg1, Yg))
            ), ToRights).

expand_universe_y(Ymax, _, Y) :-
    Y > Ymax, !.
expand_universe_y(Ymax, Delta, Y) :-
    (   galaxy(_, Y)
    ->  Y1 is Y + 1,
        expand_universe_y(Ymax, Delta, Y1)
    ;   expand_below_y(Y, Delta),
        Y1 is Y + Delta,
        Ymax1 is Ymax + Delta - 1,
        expand_universe_y(Ymax1, Delta, Y1)
    ).

expand_below_y(Y, Delta) :-
    findall(Xg-Yg, galaxy(Xg, Yg), Positions),
    include({Y}/[_-Yg]>>(Yg >= Y), Positions, Belows),
    maplist({Delta}/[Xg-Yg]>>(
                retractall(galaxy(Xg, Yg)),
                Yg1 is Yg + Delta - 1,
                assertz(galaxy(Xg, Yg1))
            ), Belows).

distances([], Acc, Acc) :- !.
distances([Pos|Poss], Acc, Distances) :-
    distances(Pos, Poss, Acc, Acc1),
    distances(Poss, Acc1, Distances).

distances(_, [], Acc, Acc) :- !.
distances(X-Y, [X1-Y1|Poss], Acc, Distances) :-
    Dist is abs(X - X1) + abs(Y - Y1),
    distances(X-Y, Poss, [Dist|Acc], Distances).
