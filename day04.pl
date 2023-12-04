:- module(day04, [day04_solve/0]).

:- use_module(util).

:- dynamic card_info/3, card_count/2.

%%  day04_solve
%
%   Prints answers for day 4.
day04_solve :-
    retractall(card_info(_,_,_)),
    retractall(card_count(_,_)),

    read_file_to_string(input('day04.txt'), S, []),
    string_lines(S, Lines),
    maplist(parse_card, Lines),
    part1(Answer1),
    write(Answer1), nl,
    part2(Answer2),
    write(Answer2), nl.

%%  parse_card(+Line)
%
%   Parses a card from the given Line.
parse_card(Line) :-
    string_chars(Line, Chars),
    phrase(card(ID, WinningCards, MyCards), Chars),
    !,
    assertz(card_info(ID, WinningCards, MyCards)).

%%  part1(-Answer)
%
%   Answer is the answer for part 1.
part1(Answer) :-
    findall(Score, card_score(_, Score), Scores),
    sum_list(Scores, Answer).

%%  card_score(-ID, -Score)
%
%   Score is the score of the card of ID.
card_score(ID, Score) :-
    card_info(ID, WinningCards, MyCards),
    intersection(WinningCards, MyCards, Matches),
    (   Matches = []
    ->  Score = 0
    ;   length(Matches, Len),
        Score is 2 ** (Len - 1)
    ).

%%  part2(-Answer)
%
%   Answer is the answer for part 2.
part2(Answer) :-
    reset_card_counts,
    findall(ID, run_part2(ID), _),
    findall(Count, card_count(_, Count), Counts),
    sum_list(Counts, Answer).

%%  reset_card_counts
%
%   Resets the counts of cards to one.
reset_card_counts :-
    findall(ID, card_info(ID, _, _), IDs),
    maplist([ID]>>assertz(card_count(ID, 1)), IDs).

%%  run_part2(+ID)
%
%   Runs the game as the rule of part 2 for the card ID.
run_part2(ID) :-
    card_info(ID, WinningCards, MyCards),
    intersection(WinningCards, MyCards, Matches),
    (   Matches = [_|_]
    ->  length(Matches, Len),
        card_count(ID, Count),
        ID1 is ID + 1,
        ID_max is ID + Len,
        inc_cards(ID1, ID_max, Count)
    ).

%%  inc_cards(+ID, +ID_max, +Delta)
%
%   Increases the counts of cards with the ID in the range [ID, ID_max],
%   by Delta.
inc_cards(ID, ID_max, Delta) :-
    (   ID > ID_max
    ->  true
    ;   card_count(ID, Count),
        retractall(card_count(ID, _)),
        Count1 is Count + Delta,
        assertz(card_count(ID, Count1)),
        ID1 is ID + 1,
        inc_cards(ID1, ID_max, Delta)
    ).

% Parsers.

card(ID, WinningCards, MyCards) -->
    ['C',a,r,d], ws, number(ID), [:], ws,
    numbers(WinningCards), ws, ['|'], ws, numbers(MyCards).

numbers([N]) --> number(N).
numbers([N|Ns]) --> number(N), ws, numbers(Ns).

number(N) --> digits(Cs), { number_chars(N, Cs) }.

digits([C]) --> digit(C).
digits([C|Cs]) --> digit(C), digits(Cs).

digit(C) --> [C], { char_type(C, digit) }.

ws --> [' '].
ws --> [' '], ws.
