:- module(day06, [day06_solve/0]).

%%  input_time_distance1(+Time, +Distance)
%
%   Time and Distance are the input data for part 1.
input_time_distance1(59,  543).
input_time_distance1(68, 1020).
input_time_distance1(82, 1664).
input_time_distance1(74, 1022).

%%  input_time_distance2(+Time, +Distance)
%
%   Time and Distance are the input data for part 2.
input_time_distance2(59_68_82_74, 543_1020_1664_1022).

%%  day06_solve
%
%   Prints answers for day 6.
day06_solve :-
    part1(Answer1),
    write(Answer1), nl,
    part2(Answer2),
    write(Answer2), nl.

%%  part1(-Answer)
%
%   Answer is the answer for part 1.
part1(Answer) :-
    findall(Range, time_range(Range), Ranges),
    foldl([X,Acc0,Acc]>>(Acc is Acc0*X), Ranges, 1, Answer).

%%  part2(-Answer)
%
%   Answer is the answer for part 2.
part2(Answer) :-
    input_time_distance2(Time, Distance),
    range(Time, Distance, Answer).

%%  time_range(-Range)
%
%   Range is the time range for an input time and distance for part 1.
time_range(Range) :-
    input_time_distance1(Time, Distance),
    range(Time, Distance, Range).

%%  range(+Time, +Distance, -Range)
%
%   Range is the time range for Time and Distance.
range(T, D, Range) :-
    SqrtDiscr is sqrt(T*T - 4*D),
    X1 is floor((T - SqrtDiscr) / 2 + 1),
    X2 is ceiling((T + SqrtDiscr) / 2 - 1),
    Range is X2 - X1 + 1.
