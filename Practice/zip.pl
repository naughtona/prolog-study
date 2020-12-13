% zip(As, Bs, ABs)
% Should hold when As, Bs, and ABs are lists of the same length,
% and each element of ABs is a term of the form A-B where A is the
% corresponding element of As and B is the corresponding element of Bs.
% This should work whenever at least one of the arguments is a proper list.
zip([A | []], [B | []], [A-B | []]).
zip([A | As], [B | Bs], [A-B | ABs]) :- zip(As, Bs, ABs). 

% ~/uni/COMP30020/A1 >>> swipl -q -l q2.pl
% ?- zip([1,2,3,4],[a,b,c,d],L).
% L = [1-a, 2-b, 3-c, 4-d] ;
% false.

% ?- zip(X,Y,[1-a,2-b,3-c,4-d]).
% X = [1, 2, 3, 4],
% Y = [a, b, c, d] ;
% false.

% ?- zip([1,2,3,4],Y,[1-a,2-b,3-c,4-d]).
% Y = [a, b, c, d] ;
% false.

% ?- zip(X,[a,b,c,d],[1-P,2-Q,3-R,4-S]).
% X = [1, 2, 3, 4],
% P = a,
% Q = b,
% R = c,
% S = d ;
% false.

% ?- zip([1,2,3],[a,b,c,d],L).
% false.