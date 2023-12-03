:- module(day03, [day03_solve/0]).

:- use_module(util).

:- dynamic found_number/4, symbol/3.

%%  day03_solve
%
%   Prints answers for day 3.
day03_solve :-
    retractall(found_number(_,_,_,_)),
    retractall(symbol(_,_,_)),

    read_file_to_string(input('day03.txt'), S, []),
    string_lines(S, Lines),
    parse_schematic(0, Lines),
    part1(Answer1),
    write(Answer1), nl,
    part2(Answer2),
    write(Answer2), nl.

%%  part1(-Answer)
%
%   Answer is the answer for part 1.
part1(Answer) :-
    findall(N, part_number(N), Nums),
    sum_list(Nums, Answer).

%%  part_number(-N)
%
%   N is a part number.
part_number(N) :-
    found_number(X, Y, Len, N),
    has_adjacent_symbols(X, Y, Len).

%%  has_adjacent_symbols(+X, +Y, +Len)
%
%   True if there are symbols adjacent to a number that starts at (X, Y) with
%   the length, Len.
has_adjacent_symbols(X, Y, Len) :-
    X0 is X - 1,
    X_max is X + Len,
    (   has_symbols(X0, Y - 1, X_max)
    ;   has_symbols(X0, Y, X_max)
    ;   has_symbols(X0, Y + 1, X_max)
    ).

%%  has_symbols(+X, +Y, +X_max)
%
%   True if there are symbols in the range [X, X_max] with the y-coordinate, Y.
has_symbols(X, Y, X_max) :-
    X =< X_max,
    (   symbol(X, Y, _)
    ;   X1 is X + 1,
        has_symbols(X1, Y, X_max)
    ).

%%  part2(-Answer)
%
%   Answer is the answer for part 2.
part2(Answer) :-
    findall(Ratio, gear_ratio(Ratio), Ratios),
    sum_list(Ratios, Answer).

%%  gear_ratio(-Ratio)
%
%   Ratio is a gear ratio.
gear_ratio(Ratio) :-
    symbol(X, Y, '*'),
    two_adjacent_numbers(X, Y, N1, N2),
    Ratio is N1 * N2.

%%  two_adjacent_numbers(+X, +Y, -N1, -N2)
%
%   N1 and N2 are the two adjacent numbers around (X, Y).
two_adjacent_numbers(X, Y, N1, N2) :-
    findall(N, number_touches(X, Y, N), [N1, N2]).

%%  number_touches(+X, +Y, -N)
%
%   Number N touches (X, Y).
number_touches(X, Y, N) :-
    found_number(X_part, Y_part, Len, N),
    X_part_max is X_part + Len - 1,
    X_part_max >= X - 1,
    X_part =< X + 1,
    Y_part >= Y - 1,
    Y_part =< Y + 1.

%%  parse_schematic(+Y, +Lines)
%
%   Parses the Lines starting from the y-coordinate, Y.
parse_schematic(_, []) :- !.
parse_schematic(Y, [Line|Lines]) :-
    string_chars(Line, Chars),
    parse_line(0, Y, [], Chars),
    Y1 is Y + 1,
    parse_schematic(Y1, Lines).

%%  parse_line(+X, +Y, +Acc, +Chars)
%
%   Parses a line consisting of Chars starting at the x-coordinate, X.
%   Y is the y-coordinate of the given line. Uses Acc as the accumulator.
parse_line(X, Y, Acc, []) :-
    !,
    record_found_number(X, Y, Acc).
parse_line(X, Y, Acc, ['.'|Cs]) :-
    !,
    record_found_number(X, Y, Acc),
    X1 is X + 1,
    parse_line(X1, Y, [], Cs).
parse_line(X, Y, Acc, [C|Cs]) :-
    char_type(C, digit),
    !,
    X1 is X + 1,
    parse_line(X1, Y, [C|Acc], Cs).
parse_line(X, Y, Acc, [C|Cs]) :-
    record_found_number(X, Y, Acc),
    assertz(symbol(X, Y, C)),
    X1 is X + 1,
    parse_line(X1, Y, [], Cs).

%%  record_found_number(+X, +Y, +Chars)
%
%   Records the found number converted from Chars at (X, Y) if Chars is not
%   empty.
record_found_number(X, Y, Chars) :-
    (   Chars = []
    ->  true
    ;   reverse(Chars, RevChars),
        number_chars(N, RevChars),
        length(Chars, Len),
        X1 is X - Len,
        assertz(found_number(X1, Y, Len, N))
    ).
