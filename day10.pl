:- module(day10, [day10_solve/0]).

:- use_module(util).

:- dynamic tile/3, loop_tile/2.

%%  day10_solve
%
%   Prints answers for day 10.
day10_solve :-
    retractall(tile(_,_,_)),
    retractall(loop_tile(_,_)),

    read_file_to_string(input('day10.txt'), S, []),
    string_lines(S, Lines),
    parse_lines(Lines, 0),

    part1(Answer1),
    write(Answer1), nl,

    length(Lines, Y_max),
    Lines = [Line|_],
    string_chars(Line, Chars),
    length(Chars, X_max),
    part2(X_max, Y_max, Answer2),
    write(Answer2), nl.

%%  part1(-Answer)
%
%   Answer is the answer for part 1.
part1(Answer) :-
    tile(X0, Y0, 'S'),
    findall(Next, find_next(X0-Y0, Next), [Begin, End]),
    msort([Begin, End], [Begin1, End1]),
    fix_start(X0-Y0, Begin1, End1),
    list_to_assoc([(X0-Y0)-true], Acc),
    follow_tiles(Begin, End, Acc, Steps),
    !,
    assoc_to_keys(Steps, Tiles),
    length(Tiles, Len),
    Answer is Len / 2,
    maplist(mark_loop_tile, Tiles).

%%  mark_loop_tile(+TilePos)
%
%   Marks TilePos as part of the loop.
mark_loop_tile(X-Y) :-
    assertz(loop_tile(X, Y)).

%%  fix_start(+Start, +Begin, +End)
%
%   Converts the Start node into a regular pipe.
%   Begin and End are the beginning and ending positions of the loop.
%   Begin < End.
fix_start(X0-Y0, X1-Y1, X2-Y2) :-
    (   (   X1 =:= X0, Y1 =:= Y0 - 1,
            X2 =:= X0, Y2 =:= Y0 + 1
        )
    ->  retractall(tile(X0, Y0, 'S')),
        assertz(tile(X0, Y0, '|'))
    ;   (   X1 =:= X0 - 1, Y1 =:= Y0,
            X2 =:= X0 + 1, Y2 =:= Y0
        )
    ->  retractall(tile(X0, Y0, 'S')),
        assertz(tile(X0, Y0, '-'))
    ;   (   X1 =:= X0,     Y1 =:= Y0 - 1,
            X2 =:= X0 + 1, Y2 =:= Y0
        )
        ->  retractall(tile(X0, Y0, 'S')),
            assertz(tile(X0, Y0, 'L'))
    ;   (   X1 =:= X0 - 1, Y1 =:= Y0,
            X2 =:= X0,     Y2 =:= Y0 - 1
        )
        ->  retractall(tile(X0, Y0, 'S')),
            assertz(tile(X0, Y0, 'J'))
    ;   (   X1 =:= X0 - 1, Y1 =:= Y0,
            X2 =:= X0,     Y2 =:= Y0 + 1
        )
        ->  retractall(tile(X0, Y0, 'S')),
            assertz(tile(X0, Y0, '7'))
    ;   (   X1 =:= X0,     Y1 =:= Y0 + 1,
            X2 =:= X0 + 1, Y2 =:= Y0
        )
        ->  retractall(tile(X0, Y0, 'S')),
            assertz(tile(X0, Y0, 'F'))
    ).

%%  part2(-Answer)
%
%   Answer is the answer for part 2.
part2(X_max, Y_max, Answer) :-
    Y_max1 is Y_max - 1,
    numlist(0, Y_max1, Ys),
    maplist(count_inner_tiles(X_max), Ys, Counts),
    sum_list(Counts, Answer).

%%  count_inner_tiles(+X_max, +Y, -Count)
%
%   Count is the number of inner tiles in the row with the y-coordinate, Y.
%   X_max is the maximum value in the x-coordinate (plus one).
count_inner_tiles(X_max, Y, Count) :-
    count_inner_tiles(0, X_max, Y, outer, 0, Count).

count_inner_tiles(X, X_max, _, _, Acc, Acc) :-
    X >= X_max,
    !.
count_inner_tiles(X, X_max, Y, outer, Acc, Count) :-
    !,
    X1 is X + 1,
    (   loop_tile(X, Y),
        (   tile(X, Y, '|')
        ;   tile(X, Y, '7')
        ;   tile(X, Y, 'F')
        )
    ->  count_inner_tiles(X1, X_max, Y, inner, Acc, Count)
    ;   count_inner_tiles(X1, X_max, Y, outer, Acc, Count)
    ).
count_inner_tiles(X, X_max, Y, inner, Acc, Count) :-
    X1 is X + 1,
    (   loop_tile(X, Y),
        (   tile(X, Y, '|')
        ;   tile(X, Y, '7')
        ;   tile(X, Y, 'F')
        )
    ->  count_inner_tiles(X1, X_max, Y, outer, Acc, Count)
    ;   (   loop_tile(X, Y)
        ->  Acc1 is Acc
        ;   Acc1 is Acc + 1
        ),
        count_inner_tiles(X1, X_max, Y, inner, Acc1, Count)
    ).

%%  parse_lines(+Lines, +Y)
%
%   Parses the Lines starting from the row, Y.
parse_lines([], _) :- !.
parse_lines([Line|Lines], Y) :-
    parse_line(Line, Y),
    Y1 is Y + 1,
    parse_lines(Lines, Y1).

%%  parse_line(+Line, +Y)
%
%   Parses the Line at the row, Y.
parse_line(Line, Y) :-
    string_chars(Line, Chars),
    parse_tiles(Chars, 0, Y).

%%  parse_tiles(+Chars, +X, +Y)
%
%   Parses the tiles at the row, Y, starting from the column, X.
parse_tiles([], _, _) :- !.
parse_tiles([C|Cs], X, Y) :-
    assertz(tile(X, Y, C)),
    X1 is X + 1,
    parse_tiles(Cs, X1, Y).

%%  find_next(+Pos, -NextPos)
%
%   Finds a next position, NextPos, adjacent to Pos.
find_next(X-Y, X1-Y) :-
    X1 is X - 1,
    (   tile(X1, Y, '-')
    ;   tile(X1, Y, 'F')
    ;   tile(X1, Y, 'L')
    ).
find_next(X-Y, X1-Y) :-
    X1 is X + 1,
    (   tile(X1, Y, '-')
    ;   tile(X1, Y, '7')
    ;   tile(X1, Y, 'J')
    ).
find_next(X-Y, X-Y1) :-
    Y1 is Y - 1,
    (   tile(X, Y1, '|')
    ;   tile(X, Y1, 'F')
    ;   tile(X, Y1, '7')
    ).
find_next(X-Y, X-Y1) :-
    Y1 is Y + 1,
    (   tile(X, Y1, '|')
    ;   tile(X, Y1, 'L')
    ;   tile(X, Y1, 'J')
    ).

%%  follow_tiles(+Cur, +End, +Acc, -Steps)
%
%   Follows the tiles along the loop, starting with Cur.
%   End is the end of the path, Acc is the accumulator,
%   Steps is the assoc list storing the visited tiles.
follow_tiles(End, End, Acc, Steps) :-
    put_assoc(End, Acc, true, Steps),
    !.
follow_tiles(X-Y, X_e-Y_e, Acc, Steps) :-
    find_next(X-Y, X1-Y1),
    \+ get_assoc(X1-Y1, Acc, _),
    put_assoc(X-Y, Acc, true, Acc1),
    follow_tiles(X1-Y1, X_e-Y_e, Acc1, Steps).
