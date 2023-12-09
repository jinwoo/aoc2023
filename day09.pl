:- module(day09, [day09_solve/0]).

:- use_module(util).

%%  day09_solve
%
%   Prints answers for day 9.
day09_solve :-
    read_file_to_string(input('day09.txt'), S, []),
    string_lines(S, Lines),
    maplist(parse_line, Lines, Sequences),
    part1(Sequences, Answer1),
    write(Answer1), nl,
    part2(Sequences, Answer2),
    write(Answer2), nl.

%%  parse_line(+Line, -Sequence)
%
%   Sequence is the parsed number sequence from Line.
parse_line(Line, Sequence) :-
    split_string(Line, " ", "", Chunks),
    maplist([S,N]>>number_string(N,S), Chunks, Sequence).

%%  part1(+Sequences, -Answer)
%
%   Answer is the answer for part 1 from the given Sequences.
part1(Sequences, Answer) :-
    maplist(extrapolate, Sequences, Nums),
    sum_list(Nums, Answer).

%%  extrapolate(+Sequence, -Num)
%
%   Num is the extrapolated number that comes after the Sequence.
extrapolate(Sequence, 0) :-
    all_zeros(Sequence),
    !.
extrapolate(Sequence, Num) :-
    seq_diffs(Sequence, Diffs),
    extrapolate(Diffs, Num0),
    last(Sequence, Last),
    Num is Last + Num0.

%%  all_zeros(+Sequence)
%
%   True if all numbers in Sequence are zero.
all_zeros(Sequence) :-
    maplist([N]>>(N =:= 0), Sequence).

%%  seq_diffs(+Sequence, -Diffs)
%
%   Diffs is a list of differences between adjacent numbers in Sequence.
seq_diffs([N1,N2|T], [Diff|Num0]) :-
    !,
    seq_diffs([N2|T], Num0),
    Diff is N2 - N1.
seq_diffs([_], []).

%%  part2(+Sequences, -Answer)
%
%   Answer is the answer for part 2 from the given Sequences.
part2(Sequences, Answer) :-
    maplist(extrapolate2, Sequences, Nums),
    sum_list(Nums, Answer).

%%  extrapolate2(+Sequence, -Num)
%
%   Num is the extrapolated number that comes before the Sequence.
extrapolate2(Sequence, 0) :-
    all_zeros(Sequence),
    !.
extrapolate2(Sequence, Num) :-
    seq_diffs(Sequence, Diffs),
    extrapolate2(Diffs, Num0),
    Sequence = [H|_],
    Num is H - Num0.
