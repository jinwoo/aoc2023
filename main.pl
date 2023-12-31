#!/usr/bin/env swipl

/*  Advent of Code 2023.

    ./main.pl [days ...]

    If `days` are given, problems from those days are solved.
    If none are given, all are solved.

    Examples:
      ./main.pl
      ./main.pl 1
      ./main.pl 1 5 25
*/

:- use_module([day01, day02, day03, day04, day05]).
:- use_module([day06, day07, day08, day09, day10]).
:- use_module([day11]).

:- initialization(main, main).

main :-
    current_prolog_flag(argv, Argv),
    run_solvers(Argv).

%%  run_solvers(+Days)
%
%   Runs the solvers for the given Days.
%   If Days is empty, runs all solvers.
run_solvers([]) :-
    !,
    setof(Day, Solver^solver(Day, Solver), Days),
    maplist(solve, Days).
run_solvers(Days) :-
    Days = [_|_],
    maplist(atom_number, Days, DayNums),
    maplist(solve, DayNums).

%%  solve(+Day)
%
%   Solves the problem for the given Day.
solve(Day) :-
    solver(Day, Solver),
    format('~n<< Day ~w >>~n', [Day]),
    call(Solver).

%% Solvers.

solver(1, day01_solve).
solver(2, day02_solve).
solver(3, day03_solve).
solver(4, day04_solve).
solver(5, day05_solve).
solver(6, day06_solve).
solver(7, day07_solve).
solver(8, day08_solve).
solver(9, day09_solve).
solver(10, day10_solve).
solver(11, day11_solve).
