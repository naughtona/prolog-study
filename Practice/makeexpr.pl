% make_expr(+List,?Expression,?Value)
%   Should hold when Expression contains all elements in List in L-R order,
%   each integer separated by any of [+-*] and Value is what Expression 
%   evaluates to.
make_expr([X],X,X).
make_expr(L,E,V) :-
    append(L1,L2,L),
    L1 \= [], L2 \= [],
    make_expr(L1,E1,V1),
    make_expr(L2,E2,V2),
    ( E = E1+E2, V is V1+V2
    ; E = E1-E2, V is V1-V2
    ; E = E1*E2, V is V1*V2
    ).

% ?- make_expr([1,3,2],E,V).
% E = 1+(3+2),
% V = 6 ;
% E = 1-(3+2),
% V = -4 ;
% E = 1*(3+2),
% V = 5 ;
% E = 1+(3-2),
% V = 2 ;
% E = 1-(3-2),
% V = 0 ;
% E = 1*(3-2),
% V = 1 ;
% E = 1+3*2,
% V = 7 ;
% E = 1-3*2,
% V = -5 ;
% E = 1*(3*2),
% V = 6 ;
% E = 1+3+2,
% V = 6 ;
% E = 1+3-2,
% V = 2 ;
% E =  (1+3)*2,
% V = 8 ;
% E = 1-3+2,
% V = 0 ;
% E = 1-3-2,
% V = -4 ;
% E =  (1-3)*2,
% V = -4 ;
% E = 1*3+2,
% V = 5 ;
% E = 1*3-2,
% V = 1 ;
% E = 1*3*2,
% V = 6 ;
% false.

% ?- make_expr([1,3,2],1*(3+2),V).
% V = 5 ;
% false.

% ?- make_expr([1,3,2],E,5).
% E = 1*(3+2) ;
% E = 1*3+2 ;
% false.