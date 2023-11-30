:- module(day01, [day01_solve/0]).

:- use_module(util).

%%  day01_solve
%
%   Prints answers for day 1.
day01_solve :-
    read_file_to_string(input('day01.txt'), S, []),
    string_lines(S, Lines),
    part1(Lines, Answer1),
    write(Answer1), nl,
    part2(Lines, Answer2),
    write(Answer2), nl.

%%  part1(+Lines, -Answer)
%
%   Answer is the answer for part 1 with the input, Lines.
part1(Lines, Answer) :-
    maplist(calibration_value, Lines, Values),
    sum_list(Values, Answer).

%%  calibration_value(+Line, -Value)
%
%   Value is the calibration value of the given Line.
calibration_value(Line, Value) :-
    string_chars(Line, Chars),
    first_digit(Chars, F),
    last_digit(Chars, L),
    Value is F * 10 + L.

%%  first_digit(+Chars, -Result)
%
%   Result is the first digit in the characters, Chars.
first_digit([C|Cs], Result) :-
    (   char_type(C, digit(N))
    ->  Result = N
    ;   first_digit(Cs, Result)
    ).

%%  last_digit(+Chars, -Result)
%
%   Result is the last digit in the characters, Chars.
last_digit(Chars, Result) :-
    last_digit(Chars, none, Result).

%%  last_digit(+Chars, +Acc, -Result)
%
%   Result is the last digit in the characters, Chars, using Acc as accumulator.
last_digit([], Acc, Acc) :-
    Acc \= none.
last_digit([C|Cs], Acc, Result) :-
    (   char_type(C, digit(N))
    ->  last_digit(Cs, N, Result)
    ;   last_digit(Cs, Acc, Result)
    ).

%%  part2(+Lines, -Answer)
%
%   Answer is the answer for part 2 with the input, Lines.
part2(Lines, Answer) :-
    maplist(calibration_value2, Lines, Values),
    sum_list(Values, Answer).

%%  calibration_value2(+Line, -Value)
%
%   Value is the calibration value of the given Line.
calibration_value2(Line, Value) :-
    string_chars(Line, Chars),
    first_digit2(Chars, F),
    last_digit2(Chars, L),
    Value is F * 10 + L.

%%  first_digit2(+Chars, -Result)
%
%   Result is the first digit in the characters, Chars.
first_digit2(Chars, Result) :-
    Chars = [_|Rest],
    (   phrase(digit(N), Chars, _)
    ->  Result = N
    ;   first_digit2(Rest, Result)
    ).

%%  last_digit2(+Chars, -Result)
%
%   Result is the last digit in the characters, Chars.
last_digit2(Chars, Result) :-
    last_digit2(Chars, none, Result).

%%  last_digit2(+Chars, +Acc, -Result)
%
%   Result is the last digit in the characters, Chars, using Acc as accumulator.
last_digit2([], Acc, Acc) :-
    !,
    Acc \= none.
last_digit2(Chars, Acc, Result) :-
    Chars = [_|Rest],
    (   phrase(digit(N), Chars, _)
    ->  last_digit2(Rest, N, Result)
    ;   last_digit2(Rest, Acc, Result)
    ).

% Parsers.

digit(N) --> [C], { char_type(C, digit(N)) }.
digit(1) --> [o,n,e].
digit(2) --> [t,w,o].
digit(3) --> [t,h,r,e,e].
digit(4) --> [f,o,u,r].
digit(5) --> [f,i,v,e].
digit(6) --> [s,i,x].
digit(7) --> [s,e,v,e,n].
digit(8) --> [e,i,g,h,t].
digit(9) --> [n,i,n,e].

chars([]) --> [].
chars([C|Cs]) --> [C], chars(Cs).
