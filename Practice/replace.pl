% replace(E1, L1, E2, L2)
% Should hold when list L1 is the same as L2, except that in one place where
% L1 has the value E1, L2 has E2.
% Note that only one occurrence of E1 is replaced. 
% This must work in any mode in which at least one of L1 or L2 is a proper list.
replace(E1, [E1 | T], E2, [E2 | T]).
replace(E1, [H | TX], E2, [H | TY]) :- replace(E1, TX, E2, TY).

% ~/uni/COMP30020/A1 >>> swipl -q -l q1.pl
% ?- replace(2,[1,2,3,4],5,X).
% X = [1, 5, 3, 4] ;
% false.

% ?- replace(2,[1,2,3,2,1],5,X).
% X = [1, 5, 3, 2, 1] ;
% X = [1, 2, 3, 5, 1] ;
% false.

% ?- replace(2,X,5,[1,5,3,5,1]).
% X = [1, 2, 3, 5, 1] ;
% X = [1, 5, 3, 2, 1] ;
% false.

% ?- replace(X,[a,b,c,d],Y,[a,e,c,d]).
% X = b,
% Y = e ;
% false.

% ?- replace(X,[1,2,3,2,1],Y,[1,5,3,5,1]).
% false.