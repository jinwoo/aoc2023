:- module(day08, [day08_solve/0]).

:- use_module(util).

:- dynamic next_nodes/3, instructions/1.

%%  day08_solve
%
%   Prints answers for day 8.
day08_solve :-
    retractall(next_nodes(_,_,_)),
    retractall(instructions(_)),

    read_file_to_string(input('day08.txt'), S, []),
    string_lines(S, Lines),
    parse_input(Lines),
    part1(Answer1),
    write(Answer1), nl,
    part2(Answer2),
    write(Answer2), nl.

%%  parse_input(+Lines)
%
%   Parses the input data from Lines.
parse_input([Line,""|Lines]) :-
    string_chars(Line, Instructions),
    assertz(instructions(Instructions)),
    maplist(parse_next, Lines).

%%  parse_next(+Line)
%
%   Parses the next left/right nodes data from Line.
parse_next(Line) :-
    string_chars(Line, Chars),
    phrase(next(Cur, Left, Right), Chars),
    assertz(next_nodes(Cur, Left, Right)).

%%  part1(-Answer)
%
%   Answer is the answer for part 1.
part1(Answer) :-
    run(['A','A','A'], [], Answer),
    !.

%%  run(+Cur, +Instructions, -Steps)
%
%   Steps is the number of steps needed to move from the Cur node to the ending
%   node using the Instructions.
run(['Z','Z','Z'], _, 0) :-
    !.
run(Cur, [], Steps) :-
    !,
    instructions(Instructions),
    run(Cur, Instructions, Steps).
run(Cur, ['L'|T], Steps) :-
    !,
    next_nodes(Cur, Left, _),
    run(Left, T, Steps0),
    Steps is Steps0 + 1.
run(Cur, ['R'|T], Steps) :-
    next_nodes(Cur, _, Right),
    run(Right, T, Steps0),
    Steps is Steps0 + 1.

%%  part2(-Answer)
%
%   Answer is the answer for part 2.
part2(Answer) :-
    findall(Steps, run2(Steps), StepsList),
    foldl([X,Acc0,Acc]>>(Acc is lcm(X, Acc0)), StepsList, 1, Answer).

%%  run2(-Steps)
%
%   Steps is the number of steps needed to move from a starting node to the
%   ending node.
run2(Steps) :-
    next_nodes(Cur, _, _),
    Cur = [_,_,'A'],
    run2(Cur, [], 0, Steps).

%%  run2(+Cur, +Instructions, +Acc, -Steps)
%
%   Steps is the number of steps needed to move from the Cur node to the ending
%   node using the Instructions, with Acc as the accumulator.
run2([_,_,'Z'], _, Acc, Acc) :-
    !.
run2(Cur, [], Acc, Steps) :-
    !,
    instructions(Instructions),
    run2(Cur, Instructions, Acc, Steps).
run2(Cur, ['L'|T], Acc, Steps) :-
    !,
    next_nodes(Cur, Left, _),
    Acc1 is Acc + 1,
    run2(Left, T, Acc1, Steps).
run2(Cur, ['R'|T], Acc, Steps) :-
    next_nodes(Cur, _, Right),
    Acc1 is Acc + 1,
    run2(Right, T, Acc1, Steps).

% Parsers.

next(Cur, Left, Right) -->
    node(Cur), [' ',=,' ','('], node(Left), [',',' '], node(Right), [')'].

node([C1,C2,C3]) --> [C1,C2,C3].
