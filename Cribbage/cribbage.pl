% File: cribbage.pl
% Author: Andrew Naughton <naughtona@student.unimelb.edu.au>
% Purpose: Project 1 Submission, COMP30020 S2 2020

% We provide functionality in two parts of Cribbage. The first
% is hand_value/3, which calculates the value of a given hand 
% according to the standard rules of Cribbage. The second is
% select_hand/3, which determines your preferred 4-card hand 
% and 1-2 cribcards from your initial 5-6 cards. We assume 
% hand_value/3 has valid input: a hand is 4 cards; a startcard
% is 1 card; input cards are unique and in the form
% card(Rank, Suit). We assume select_hand/3 has valid input: 
% 5-6 valid initial cards; input cards are unique and in the 
% form card(Rank, Suit).

% Our approach for solving hand_value/3 is to separately
% calculate scores for each rule, including score_pairs/2 for
% pair combinations, score_runs/2 for runs, score_flush/3 for 
% a flush, score_oneforhisnob/3 for one for his nob, and
% finally score_15s/3 for number of ways to sum to 15. Then, 
% we simply sum these scores together to arrive at a final 
% value for the provided hand.

% Our approach for solving select_hand/3 has the following 
% steps. First, find the different combinations of hands that 
% are possible from the provided 5-6 cards. Second, deduce the
% startcards that are possible given that the 5-6 cards faced
% up in front of you cannot appear as startcards. This should
% leave 46-47 possible startcards. Next, model the returns you
% get from combining a hand with each possible startcard. Take
% the sum of these returns; this will be the expected value
% for that hand. Do this for each possible hand. Finally, 
% select the hand that yields the highest expected value.

% Note, we have provided modes for which our predicates will 
% reliably work in. However, some predicates may indeed work
% in other modes as well.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% facts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% suit(?Suit)
%	Suits that cards can take

suit(hearts).
suit(diamonds).
suit(clubs).
suit(spades).


%% rank(?Rank, ?Index, ?Countvalue)
%	Rank is the position of a card relative to others in the same suit
%	Index is the relative position of a card in integer form
%	Countvalue is the counting value of a card, used in scoring 15s

% numerics

rank(2, 2, 2).
rank(3, 3, 3).
rank(4, 4, 4).
rank(5, 5, 5).
rank(6, 6, 6).
rank(7, 7, 7).
rank(8, 8, 8).
rank(9, 9, 9).
rank(10, 10, 10).

% royals

rank(jack, 11, 10).
rank(queen, 12, 10).
rank(king, 13, 10).
rank(ace, 1, 1).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% hand_value %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% hand_value(+Hand, +Startcard, ?Value)
%	Should hold when Hand is a valid 4-card hand, Startcard is a valid
%	card, and Value is the correct score awarded for the provided Hand
%	and Startcard.

hand_value(Hand, Startcard, Value) :-
	score_15s(Hand, Startcard, Fifteenscore),
	% score_pairs and score_runs benefit from sorted-ranks in index form
	maplist(to_index, [Startcard|Hand], Ranks),
	msort(Ranks, Sortedranks),
	score_pairs(Sortedranks, Pairscore),
	score_runs(Sortedranks, Runscore),
	% score_flush and score_oneforhisnob require startcard's suit
	to_suit(Startcard, SCsuit),
	score_flush(Hand, SCsuit, Flushscore),
	score_oneforhisnob(Hand, SCsuit, Nobscore),
	% add scores to get value of hand + startcard
	Value is Pairscore + Runscore + Nobscore + Flushscore + Fifteenscore.


%% score_15s(+Hand, +Startcard, ?Score)
%	Should hold when Hand is a valid hand, Startcard is a valid card,
%	and Score is two times the number of ways the elements in Hand with
%	Startcard can sum to 15. Note we do not care about order (i.e.
%	permutations).

score_15s(Hand, Startcard, Score) :-
	maplist(to_countvalue, [Startcard|Hand], Values),
	Target = 15, % represents the target sum per rule #1
	findall(Sol, subset_sum(Values, Target, Sol), Sols),
	length(Sols, Count),
	double(Count, Score).


%% score_pairs(+Ranks, ?Score)
%	Should hold when Ranks is a sorted number list, and Score is two
%	times the number of different pairs in Ranks. E.g. [1,2,3,3,3] has 3
%	threes and represent three different pair combinations, each worth 2
%	points.

score_pairs(Ranks, Score) :-
	% remove duplicates
	sort(Ranks, SRanks),
	score_pairs(SRanks, Ranks, 0, Score).

% score_pairs(+Sorted_Ranks, +Ranks, +Accumulator, ?Score)

score_pairs([], _, Acc, Acc).
score_pairs([Head|Tail], Ranks, Acc, Score) :-
	occurrences_of_term(Head, Ranks, Count),
	replicate(Head, Count, Front),
	% Back becomes Ranks excluding element Head
	append(Front, Back, Ranks),
	choose2(Count, Ways),
	% each pair is worth 2 points
	double(Ways, Value),
	NewAcc is Acc + Value,
	score_pairs(Tail, Back, NewAcc, Score).


%% score_runs(+Ranks, ?Score)
%	Should hold when Ranks is a sorted number list and Score is the
%	number of ways a run that be formed from Ranks, where the run length
%	must be at least 3 and the maximum run length is taken, multiplied
%	by the length of that run. E.g. [6,7,8,8,9] has two runs of 4.
%	Notice that we only count runs of max length (>2).

score_runs(Ranks, Score) :-
	findall(Runlen, consec_subset_gt2(Ranks, _, Runlen), Runlens),
	% maxlist will fail with empty list, so add 0 to list (work-around)
	max_list([0|Runlens], Maxlen),
	% Freq is number of different runs of Maxlen in Runlens list
	occurrences_of_term(Maxlen, Runlens, Freq),
	Score is Maxlen * Freq.


%% score_flush(+Hand, +SCsuit, ?Score)
%	Should hold when Hand is a valid hand, SCsuit is the suit of a valid
%	startcard, and Score is 5 for a five flush, 4 for a four flush,
%	otherwise 0.

score_flush(Hand, SCsuit, Score) :-
	maplist(to_suit, Hand, Suits),
	( five_flush([SCsuit|Suits], Len) ->
		Score = Len
	; four_flush(Suits, Len) ->
		Score = Len
	;	
		Score = 0
	).


%% score_oneforhisnob(+Hand, +SCsuit, ?Score)
%	Should hold when Hand is a valid hand, SCsuit is the suit of a valid
%	startcard, and Score is 1 when Hand contains the Jack of SCsuit,
%	otherwise 0.

score_oneforhisnob(Hand, SCsuit, Score) :-
	( contains_term(card(jack, SCsuit), Hand) ->
		Score = 1
	;
		Score = 0
	).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% select_hand %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% select_hand(+Cards, ?Hand, ?Cribcards)
%	Should hold when Cards is a list of 5-6 valid cards, Hand is a list
%	of 4 valid cards from Cards, and Cribcards is the remaining cards in
%	Cards after subtracting the cards in Hand. Hand must represent the 4
%	cards in Cards that generate the highest expected value.

select_hand(Cards, Hand, Cribcards) :-
	new_deck(Deck),
	subtract(Deck, Cards, Startcards),
	possible_hands(Cards, Phands),
	best_hand(Phands, Startcards, Hand),
	subtract(Cards, Hand, Cribcards).


%% best_hand(+Phands, +Startcards, ?Hand)
%	Should hold when Phands is a list of valid possible hands, Startcards
%	is a list of valid startcards, and Hand is the hand from Phands with
%	the highest expected value.

best_hand(Phands, Startcards, Hand) :-
	maplist(expected_value(Startcards), Phands, Sumreturns),
	max_list(Sumreturns, Maxreturn),
	first_index_of(Sumreturns, Maxreturn, Index),
	nth0(Index, Phands, Hand).


%% expected_value(+Startcards, +Hand, ?Sumreturn)
%	Should hold when Startcards is a list of valid startcards, Hand is
%	a valid hand, and Sumreturn is the summation of all hand_value results
%	from all combinations of Hand with Startcards.

expected_value(Startcards, Hand, Sumreturn) :-
	maplist(hand_value(Hand), Startcards, Returns),
	sumlist(Returns, Sumreturn).





%%%%%%%%%%%%%%%%%%%%%%%%%%% game-specific helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% card(?Card)
%	Should hold when Card is a card with a valid Rank and Suit.

card(card(Rank, Suit)) :-
	rank(Rank, _, _),
	suit(Suit).


%% new_deck(?Deck)
%	Should hold when Deck is a full deck of cards (52 cards).

new_deck(Deck) :-
	bagof(C, card(C), Deck).


%% to_index(+Card, ?Index)
%	Should hold when Index obtains the index associated with Card's Rank.

to_index(card(Rank, _), Index) :-
	rank(Rank, Index, _).


%% to_countvalue(+Card, ?Countvalue)
%	Should hold when Countvalue is the count value associated with
%	Card's Rank.

to_countvalue(card(Rank, _), Countvalue) :-
	rank(Rank, _, Countvalue).


%% to_suit(+Card, ?Suit)
%	Should hold when Suit is the suit in Card.

to_suit(card(_, Suit), Suit).


%% possible_hands(+Cards, ?Phands)
%	Should hold when Phands is all possible subsets of list Cards that
%	are of length K, where K is 4, representing all possible hands 
%	given Cards.

possible_hands(Cards, Phands) :-
	K = 4, % hand is 4 cards
	bagof(Phand, subset_k(Cards, K, Phand), Phands).


%% five_flush(+Suits, ?Len)
%	Assumes the first element in the list Suits is Startcard's suit.
%	Should hold when all suits in Suits share the same suit, and Suits'
%	length is Len.

five_flush([Suit|Ss], Len) :-
	append([[Suit,Suit,Suit,Suit]], Ss),
	length([Suit|Ss], Len).


%% four_flush(+Suits, ?Len)
%	Should hold when all suits excluding Startcard share the same suit, and
%	Suits' length is Len.

four_flush([Suit|Ss], Len) :-
	append([[Suit,Suit,Suit]], Ss),
	length([Suit|Ss], Len).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%% general-case helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% subset(+List, ?Subset)
%	Should hold when Subset is subset of List.

subset([], []).
subset([Elem|List], [Elem|Subset]) :-
	subset(List, Subset).
subset([_|List], Subset) :-
	subset(List, Subset).


%% subset_k(+List, ?K, ?Subset)
%	Should hold when Subset is a subset of List of length K.

subset_k(List, K, Subset) :-
	subset(List, Subset),
	length(Subset, K).


%% subset_sum(+List, ?Target, ?Subset)
%	Should hold when Subset is a subset of List whose elements sum to
%	integer Target.

subset_sum(List, Target, Subset) :- 
	subset(List, Subset),
	sumlist(Subset, Target).


%% consec_subset_gt2(+List, ?Subset, ?K)
%	Should hold when List is a sorted number list, and Subset is a
%	subset of List of length K, where K is greater than 2, and whose
%	elements are consecutive.
%	Makes sure that Subset's elements are consecutive by checking
%	that Subset contains no duplicates and its last element equates
%	to the head element plus the length of the subset K minus 1.
%	e.g.	Subset = [3,4,5,6] Last = 6, Head = 3, K = 4
%		-> Last = 6 == Head + K - 1 = 3 + 4 - 1 = 6

consec_subset_gt2(List, Subset, K) :-
	subset_k(List, K, [Head|Tail]),
	K > 2,
	% remove duplicates
	sort([Head|Tail], [Head|Tail]),
	last([Head|Tail], Last),
	Last is Head + K - 1,
	Subset = [Head|Tail].


%% replicate(+Elem, +Ntimes, ?List)
%	Should hold when List is a list of element Elem Ntimes times.

replicate(Elem, Ntimes, List) :-
	findall(Elem, between(1, Ntimes, _), List).


%% first_index_of(+List, +Elem, ?Index)
%	Should hold when Index is the first occurrence of element Elem
%	in List.

first_index_of(List, Elem, Index) :-
	first_index_of(List, Elem, 0, Index).

% first_index_of(+List, +Elem, +CurrIndex, ?Index)

first_index_of([Head|Tail], Elem, CurrIndex, Index) :-
	( Head = Elem ->
		Index = CurrIndex
	;	
		NewCurrIndex is CurrIndex + 1,
		first_index_of(Tail, Elem, NewCurrIndex, Index)
	).


%% choose2(+Number, ?Result)
%	Should hold when Result is the number of ways to choose 2 from
%	Number numbers. Ie. 4 choose 2 = 6 == (4 - 1) * 4 / 2 = 6.

choose2(N, Result) :-
	Result is (N - 1) * N / 2.


%% double(+Number, ?Result)
%	Should hold when Result is double the value of Number.

double(N, Result) :-
	Result is N + N.
