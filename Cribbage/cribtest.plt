:- begin_tests(cribbage).
:- include(cribbage).

test(x) :- hand_value([card(7,clubs),card(queen,hearts),card(2,clubs),card(jack,clubs)], card(9,hearts), Value),
Value = 0.

test(x) :- hand_value([card(ace,spades),card(3,hearts),card(king,hearts),card(7,hearts)], card(king,spades), Value),
Value = 2.

test(x) :- hand_value([card(ace,spades),card(3,hearts),card(king,hearts),card(7,hearts)], card(2,diamonds), Value),
Value = 5.

test(x) :- hand_value([card(6,clubs),card(7,clubs),card(8,clubs),card(9,clubs)], card(8,spades), Value),
Value = 20.

test(x) :- hand_value([card(7,hearts),card(9,spades),card(8,clubs),card(7,clubs)], card(8,hearts), Value),
Value = 24.

test(x) :- hand_value([card(5,hearts),card(5,spades),card(5,clubs),card(jack,diamonds)], card(5,diamonds), Value),
Value = 29.

test(x) :- hand_value([card(king,spades),card(5,diamonds),card(3,hearts),card(3,diamonds)],card(jack,diamonds),Value),
Value = 6.

test(x) :- hand_value([card(3,hearts),card(queen,diamonds),card(6,clubs),card(ace,clubs)],card(ace,hearts),Value),
Value = 4.

test(x) :- hand_value([card(9,hearts),card(3,hearts),card(10,spades),card(9,clubs)],card(7,clubs),Value),
Value = 2.

test(x) :- hand_value([card(6,spades),card(6,clubs),card(3,spades),card(4,hearts)],card(2,spades),Value),
Value = 11.

test(x) :- hand_value([card(jack,clubs),card(king,diamonds),card(9,spades),card(2,hearts)],card(7,diamonds),Value),
Value = 0.

test(x) :- select_hand([card(10,spades),card(jack,diamonds),card(jack,spades),card(2,hearts),card(7,diamonds)],Hand,Cribcards),
Hand = [card(10, spades), card(jack, diamonds), card(jack, spades), card(2, hearts)],
Cribcards = [card(7, diamonds)].

test(x) :- select_hand([card(jack,hearts),card(5,hearts),card(5,diamonds),card(2,diamonds),card(3,clubs)],Hand,Cribcards),
Hand = [card(jack, hearts), card(5, hearts), card(5, diamonds), card(3, clubs)],
Cribcards = [card(2, diamonds)].

test(x) :- select_hand([card(king,diamonds),card(4,diamonds),card(6,spades),card(10,spades),card(7,spades)],Hand,Cribcards),
Hand = [card(king, diamonds), card(4, diamonds), card(6, spades), card(7, spades)],
Cribcards = [card(10, spades)].

test(x) :- select_hand([card(4,diamonds),card(5,diamonds),card(3,diamonds),card(6,diamonds),card(10,spades)],Hand,Cribcards),
Hand = [card(4, diamonds), card(5, diamonds), card(3, diamonds), card(6, diamonds)],
Cribcards = [card(10, spades)].

test(x) :- select_hand([card(king,clubs),card(7,clubs),card(jack,clubs),card(2,hearts),card(8,spades)],Hand,Cribcards),
Hand = [card(7, clubs), card(jack, clubs), card(2, hearts), card(8, spades)],
Cribcards = [card(king, clubs)].

test(x) :- select_hand([card(2,clubs),card(4,hearts),card(6,diamonds),card(8,hearts),card(2,spades),card(jack,clubs)], Hand, Cribcards),
Hand = [card(2, clubs), card(4, hearts), card(6, diamonds), card(2, spades)],
Cribcards = [card(8, hearts), card(jack, clubs)].

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(6, clubs), card(6, diamonds)], card(queen, clubs), Value),
Value = 4.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(queen, clubs), card(6, diamonds)], card(6, clubs), Value),
Value = 4.

test(x) :- hand_value([card(ace, clubs), card(6, clubs), card(queen, clubs), card(6, diamonds)], card(2, clubs), Value),
Value = 4.

test(x) :- hand_value([card(2, clubs), card(6, clubs), card(queen, clubs), card(6, diamonds)], card(ace, clubs), Value),
Value = 4.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(6, clubs), card(queen, clubs)], card(7, diamonds), Value),
Value = 6.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(6, clubs), card(7, diamonds)], card(queen, clubs), Value),
Value = 2.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(queen, clubs), card(7, diamonds)], card(6, clubs), Value),
Value = 2.

test(x) :- hand_value([card(ace, clubs), card(6, clubs), card(queen, clubs), card(7, diamonds)], card(2, clubs), Value),
Value = 2.

test(x) :- hand_value([card(2, clubs), card(6, clubs), card(queen, clubs), card(7, diamonds)], card(ace, clubs), Value),
Value = 2.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(6, clubs), card(queen, clubs)], card(8, diamonds), Value),
Value = 6.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(6, clubs), card(8, diamonds)], card(queen, clubs), Value),
Value = 2.

test(x) :- hand_value([card(ace, clubs), card(2, clubs), card(queen, clubs), card(8, diamonds)], card(6, clubs), Value),
Value = 2.

test(x) :- hand_value([card(ace, clubs), card(6, clubs), card(queen, clubs), card(8, diamonds)], card(2, clubs), Value),
Value = 2.

test(x) :- hand_value([card(2, clubs), card(6, clubs), card(queen, clubs), card(8, diamonds)], card(ace, clubs), Value),
Value = 2.

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(4, clubs), card(king, diamonds)], _, Cribcards),
Cribcards = [card(king, diamonds)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(6, clubs)], _, Cribcards),
Cribcards = [card(6, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(7, clubs)], _, Cribcards),
Cribcards = [card(7, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(8, clubs)], _, Cribcards),
Cribcards = [card(8, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(9, clubs)], _, Cribcards),
Cribcards = [card(5, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(10, clubs)], _, Cribcards),
Cribcards = [card(5, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(jack, clubs)], _, Cribcards),
Cribcards = [card(5, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(queen, clubs)], _, Cribcards),
Cribcards = [card(5, clubs)].

test(x) :- select_hand([card(ace, clubs), card(2, clubs), card(3, clubs), card(5, clubs), card(king, clubs)], _, Cribcards),
Cribcards = [card(5, clubs)].

:- end_tests(cribbage).

% ?- ["cribtest.plt"].
% true.

% ?- run_tests.
% % PL-Unit: cribbage ........................................ done
% % All 40 tests passed
% true.