% Implement the following predicate:
% sublist(Xs, Ys)

% this holds when Xs is a list containing some of the elements of Ys, in the same order they appear in the list Ys. 
% This should work whenever Ys is a proper list. 
% For example:

% sublist([a,c,e],[a,b,c,d,e]) should succeed.

% sublist([a,e,c],[a,b,c,d,e]) should fail.

% sublist([a,X,d],[a,b,c,d,e]) should have the two solutions X=b and X=c.

% sublist(X,[a,b,c]) should have the eight solutions X=[]; X=[c]; X=[b]; X=[b,c]; X=[a]; X=[a,c]; X=[a,b]; and X=[a,b,c].

sublist([], _).
sublist([X | Xs], [X | Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_ | Ys]) :- sublist(Xs, Ys).

% ~/uni/COMP30020/A1 >>> swipl -q -l q3.pl
% ?- sublist([a,c,e],[a,b,c,d,e]).
% true ;
% false.

% ?- sublist([a,e,c],[a,b,c,d,e]).
% false.

% ?- sublist([a,X,d],[a,b,c,d,e]).
% X = b ;
% X = b ;
% X = c ;
% X = c ;
% false.

% ?- sublist(X,[a,b,c]).
% X = [] ;
% X = [a] ;
% X = [a, b] ;
% X = [a, b, c] ;
% X = [a, b] ;
% X = [a] ;
% X = [a, c] ;
% X = [a] ;
% X = [] ;
% X = [b] ;
% X = [b, c] ;
% X = [b] ;
% X = [] ;
% X = [c] ;
% X = [] ;
% false.

% ?- halt.
