% You have two containers, one able to hold 5 litres and the other able to
% hold 3 litres. Your goal is to get exactly 4 litres of water into the 
% 5 litre container.

% You have a well with an unlimited supply of water. For each turn, you
% are permitted to do one of the following:
%   - completely empty one container, putting the water back in the well
%   - completely fill one container from the well
%   - pour water from one container to the other just until the source
%     container is empty or the receiving container is full.

% In the last case, the original container is left with what it originally
% had less whatever unfilled space the receiving container originally had.

% All containers begin empty.

:- use_module(library(arithmetic)).

% Should hold when Moves is a list of actions to take in order to obtain
% the desired state. Each action on the list is either:
%   - fill(To), where To is the capacity of the container to fill from
%     the well
%   - empty(From), where From is the capacity of the container to empty
%   - pour(From, To), where From is the capacity of the container to pour
%     from and To is the capacity of container to pour into
containers(Moves) :- path([0,0], [[0,0]], Moves).

path([_,4], _, []).
path(State, History, [Action|Moves]) :-
    move(State, Action, NewState),
    \+ member(NewState, History),
    path(NewState,[NewState|History], Moves).

move([_,B0],fill(3),[3,B0]).
move([S0,_],fill(5),[S0,5]).

move([_,B0],empty(3),[0,B0]).
move([S0,_],empty(5),[S0,0]).

move([S0,B0],pour(3,5),[S,B]) :-
    B1 is B0 + S0,
    B is min(5,B1),
    S1 is S0 - (B - B0),
    S is max(0,S1).
move([S0,B0],pour(5,3),[S,B]) :-
    S1 is S0 + B0,
    S is min(3,S1),
    B1 is B0 - (S - S0),
    B is max(0,B1).

% ?- containers([fill(3), fill(5), empty(3), pour(5, 3), empty(3), pour(5, 3), fill(5), pour(5, 3)]).
% true.

% ?- setof(Sol, containers(Sol), Sols), length(Sols, N).
% Sols = [...] ,
% N = 31.