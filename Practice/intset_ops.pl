% intset_member(+N, ?Set)
% 	  Should hold when N is a member of integer set Set (BSearchTree)
intset_member(N, tree(_,N,_)).
intset_member(N, tree(L,X,_)) :-
    N<X,
    intset_member(N, L),
    !.
intset_member(N, tree(_,X,R)) :-
    N>X,
    intset_member(N, R).


% intset_insert(+N, ?Set0, ?Set)
% 	  Should hold when Set is the same as Set0, except that Set has N as a
%     member. It doesn't matter whether Set0 already has N in it, but Set
%     must not have multiple  occurrences of N.
intset_insert(N, empty, tree(empty, N, empty)).
intset_insert(N, tree(L, N, R), tree(L, N, R)).
intset_insert(N, tree(L0, X, R), tree(L1, X, R)) :-
    N<X,
    intset_insert(N, L0, L1),
    !.
intset_insert(N, tree(L, X, R0), tree(L, X, R1)) :-
    N>X,
    intset_insert(N, R0, R1).


% ?- [intset_ops].
% true.

% ?- intset_member(1, tree(tree(empty,1,empty),2,empty)).
% true.

% ?- intset_insert(3, tree(tree(empty,1,empty),2,empty),tree(tree(empty,1,empty),2,tree(empty,3,empty))).
% true ;
% false.