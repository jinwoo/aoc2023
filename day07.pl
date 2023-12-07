:- module(day07, [day07_solve/0]).

:- use_module(util).

day07_solve :-
    read_file_to_string(input('day07.txt'), S, []),
    string_lines(S, Lines),
    maplist(parse_input, Lines, HandBidPairs),
    part1(HandBidPairs, Answer1),
    write(Answer1), nl,
    part2(HandBidPairs, Answer2),
    write(Answer2), nl.

%%  parse_input(+Line, -HandBidPair)
parse_input(Line, Hand-Bid) :-
    split_string(Line, ' ', '', [Hand, BidStr]),
    number_string(Bid, BidStr).

%%  part1(+HandBidPairs, -Answer)
part1(HandBidPairs, Answer) :-
    maplist([HS-B, H-B]>>convert_hand(HS, H), HandBidPairs, HandBidPairs1),
    maplist(rank_hand, HandBidPairs1, RankHandBidList),
    msort(RankHandBidList, SortedList),
    maplist([_-H-B,H-B]>>true, SortedList, SortedHandBidPairs),
    total_score(SortedHandBidPairs, 1, 0, Answer).

%%  convert_hand(+HandStr, -Hand)
convert_hand(HandStr, Hand) :-
    string_chars(HandStr, HandCs),
    maplist(card_rank, HandCs, Hand).

%%  rank_hand(+HandBidPair, -RankHandBid)
rank_hand(Hand-Bid, Rank-Hand-Bid) :-
    hand_type(Hand, Type),
    type_rank(Type, Rank).

%%  part2(+HandBidPairs, -Answer)
part2(HandBidPairs, Answer) :-
    maplist([HS-B, H-B]>>convert_hand2(HS, H), HandBidPairs, HandBidPairs1),
    maplist([H-B, H1]>>exclude(=(1), H, H1), HandBidPairs1, NonJokerHands),
    maplist(rank_hand2, NonJokerHands, HandBidPairs1, RankHandBidList),
    msort(RankHandBidList, SortedList),
    maplist([_-H-B,H-B]>>true, SortedList, SortedHandBidPairs),
    total_score(SortedHandBidPairs, 1, 0, Answer).

%%  convert_hand2(+HandStr, -Hand)
convert_hand2(HandStr, Hand) :-
    string_chars(HandStr, HandCs),
    maplist(card_rank2, HandCs, Hand).

%%  rank_hand2(+NonJokerHand, +HandBidPair, -RankHandBid)
rank_hand2(NJHand, Hand-Bid, Rank-Hand-Bid) :-
    hand_type2(NJHand, Type),
    type_rank(Type, Rank).

%%  total_score(+HandBidPairs, +Rank, +Acc, -Score)
total_score([], _, Acc, Acc).
total_score([_-B|T], Rank, Acc, Score) :-
    Rank1 is Rank + 1,
    Acc1 is Acc + B*Rank,
    total_score(T, Rank1, Acc1, Score).

%%  type_rank(+Type, -Rank)
type_rank(five_of_a_kind,  7).
type_rank(four_of_a_kind,  6).
type_rank(full_house,      5).
type_rank(three_of_a_kind, 4).
type_rank(two_pair,        3).
type_rank(one_pair,        2).
type_rank(high_card,       1).

%%  card_rank(+Card, -Rank)
card_rank('A', 14).
card_rank('K', 13).
card_rank('Q', 12).
card_rank('J', 11).
card_rank('T', 10).
card_rank('9',  9).
card_rank('8',  8).
card_rank('7',  7).
card_rank('6',  6).
card_rank('5',  5).
card_rank('4',  4).
card_rank('3',  3).
card_rank('2',  2).

%%  card_rank2(+Card, -Rank)
card_rank2('A', 14).
card_rank2('K', 13).
card_rank2('Q', 12).
card_rank2('T', 10).
card_rank2('9',  9).
card_rank2('8',  8).
card_rank2('7',  7).
card_rank2('6',  6).
card_rank2('5',  5).
card_rank2('4',  4).
card_rank2('3',  3).
card_rank2('2',  2).
card_rank2('J',  1).

%%  hand_type(+Hand, -Type)
hand_type(Hand, Type) :-
    msort(Hand, Sorted),
    sorted_hand_type(Sorted, Type).

%%  sorted_hand_type(+Hand, -Type)
sorted_hand_type([X,X,X,X,X], five_of_a_kind) :- !.
sorted_hand_type([X,X,X,X,_], four_of_a_kind) :- !.
sorted_hand_type([_,X,X,X,X], four_of_a_kind) :- !.
sorted_hand_type([X,X,X,Y,Y], full_house) :- !.
sorted_hand_type([X,X,Y,Y,Y], full_house) :- !.
sorted_hand_type([X,X,X,_,_], three_of_a_kind) :- !.
sorted_hand_type([_,X,X,X,_], three_of_a_kind) :- !.
sorted_hand_type([_,_,X,X,X], three_of_a_kind) :- !.
sorted_hand_type([X,X,Y,Y,_], two_pair) :- !.
sorted_hand_type([X,X,_,Y,Y], two_pair) :- !.
sorted_hand_type([_,X,X,Y,Y], two_pair) :- !.
sorted_hand_type([X,X,_,_,_], one_pair) :- !.
sorted_hand_type([_,X,X,_,_], one_pair) :- !.
sorted_hand_type([_,_,X,X,_], one_pair) :- !.
sorted_hand_type([_,_,_,X,X], one_pair) :- !.
sorted_hand_type([_,_,_,_,_], high_card) :- !.

%%  hand_type2(+Hand, -Type)
hand_type2(Hand, Type) :-
    msort(Hand, Sorted),
    sorted_hand_type2(Sorted, Type).

%%  sorted_hand_type2(+Hand, -Type)
sorted_hand_type2(Hand, Type) :- sorted_hand_type(Hand, Type), !.
sorted_hand_type2([X,X,X,X], five_of_a_kind) :- !.
sorted_hand_type2([X,X,X,_], four_of_a_kind) :- !.
sorted_hand_type2([_,X,X,X], four_of_a_kind) :- !.
sorted_hand_type2([X,X,Y,Y], full_house) :- !.
sorted_hand_type2([X,X,_,_], three_of_a_kind) :- !.
sorted_hand_type2([_,X,X,_], three_of_a_kind) :- !.
sorted_hand_type2([_,_,X,X], three_of_a_kind) :- !.
sorted_hand_type2([_,_,_,_], one_pair) :- !.
sorted_hand_type2([X,X,X], five_of_a_kind) :- !.
sorted_hand_type2([X,X,_], four_of_a_kind) :- !.
sorted_hand_type2([_,X,X], four_of_a_kind) :- !.
sorted_hand_type2([_,_,_], three_of_a_kind) :- !.
sorted_hand_type2([X,X], five_of_a_kind) :- !.
sorted_hand_type2([_,_], four_of_a_kind) :- !.
sorted_hand_type2([_], five_of_a_kind) :- !.
sorted_hand_type2([], five_of_a_kind) :- !.
