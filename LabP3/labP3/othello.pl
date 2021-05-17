/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */

opponent(1, 2).
opponent(2, 1). 





% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

%[ [.,.,.,.,.,.], [.,.,.,.,.,.], [.,.,1,2,.,.], [.,.,2,1,.,.], [.,.,.,.,.,.], [.,.,.,.,.,.] ]
initBoard([ [.,.,.,.,.,.], 
			[.,.,.,.,.,.], 
			[.,.,1,2,.,.], 
			[.,.,2,1,.,.], 
			[.,.,.,.,.,.], 
			[.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(initBoard, 2).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
count([], 0, 0).
count([[]|Rows], X, O) :- count(Rows, X, O).
count([[S|Tail]|Rows], X, O) :-
	(S==1, count([Tail|Rows], X1, O), X is X1 + 1 );
	(S==2, count([Tail|Rows], X, O1), O is O1 + 1 );
	(S=='.',count([Tail|Rows], X, O)).

winner(State, Plyr):- terminal(State), count(State, X, O), X>O, Plyr is 2, writeln('Player 2 wins!').
winner(State, Plyr):- terminal(State), count(State, X, O), X<O, Plyr is 1, writeln('Player 1 wins!').

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State):- terminal(State), count(State, X, O), X==O, writeln('Game ended in a tie!').



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State):- moves(0, State, MvList0), moves(1, State, MvList1), MvList0 == [], MvList1 == [].



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%
moves(Plyr, State, MvList):- findall(Move, validmove(Plyr, State, Move), Moves), my_sort(Moves,MvList).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, Move, State, NewState, NextPlyr):- opponent(Plyr, Opponent), setMove(Plyr, Opponent, State, NewState), (moves(Opponent, NewState, MvList), MvList \= [], NextPlyr is Opponent); NextPlyr is Plyr.

%setMove(Plyr, Opponent, State, NewState):- 
setAll([], State, State, _).
setAll([Disk|Disks], State, NewState, Plyr):- set(State, NS, Disk, Plyr), setAll(Disks, NS, NewState, Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State, Proposed):- testEast(Proposed, Plyr, State, _);
	 							testWest(Proposed, Plyr, State, _);
								testSouth(Proposed, Plyr, State, _); 
								testNorth(Proposed, Plyr, State, _);
								testSouthEast(Proposed, Plyr, State, _);
								testSouthWest(Proposed, Plyr, State, _);
								testNorthEast(Proposed, Plyr, State, _);
								testNorthWest(Proposed, Plyr, State, _).

testEast([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X + 1, get(State, [X1,Y], O), O == Opponent, sequenceWithEnd(State, [X1,Y], 1, 0, Opponent, Plyr, Disks). %East
testWest([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X - 1, get(State, [X1,Y], O), O == Opponent, sequenceWithEnd(State, [X1,Y], -1, 0, Opponent, Plyr, Disks). %West
testSouth([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), Y1 is Y + 1, get(State, [X,Y1], O), O == Opponent, sequenceWithEnd(State, [X,Y1], 0, 1, Opponent, Plyr, Disks). %South
testNorth([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), Y1 is Y - 1, get(State, [X,Y1], O), O == Opponent, sequenceWithEnd(State, [X,Y1], 0, -1, Opponent, Plyr, Disks). %North
testSouthEast([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X + 1, Y1 is Y + 1, get(State, [X1,Y1], O), O == Opponent, sequenceWithEnd(State, [X1,Y1], 1, 1, Opponent, Plyr, Disks). %SouthEast
testNorthEast([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X + 1, Y1 is Y - 1, get(State, [X1,Y1], O), O == Opponent, sequenceWithEnd(State, [X1,Y1], 1, -1, Opponent, Plyr, Disks). %NorthEast
testSouthWest([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X - 1, Y1 is Y + 1, get(State, [X1,Y1], O), O == Opponent,sequenceWithEnd(State, [X1,Y1], -1, 1, Opponent, Plyr, Disks). %SouthWest
testNorthWest([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V == '.', opponent(Plyr, Opponent), X1 is X - 1, Y1 is Y - 1, get(State, [X1,Y1], O), O == Opponent,sequenceWithEnd(State, [X1,Y1], -1, -1, Opponent, Plyr, Disks).	%NorthWest


sequenceWithEnd(State, [X,Y], _, _, _, StopN, []):- get(State, [X,Y],V), V==StopN.
sequenceWithEnd(State, [X,Y], XS, YS, SeqN, StopN, [[X,Y]|Disks]):- get(State, [X,Y],V), V==SeqN, X1 is X + XS, Y1 is Y + YS, sequenceWithEnd(State, [X1,Y1], XS,YS, SeqN, StopN, Disks).
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 

my_sort(List,Sorted):- i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([[A,B]|T],Acc,Sorted):-insert([A,B],Acc,NAcc),i_sort(T,NAcc,Sorted).

insert([A1,A2],[[B1,B2]|T],[[B1,B2]|NT]):- (A1>B1;(A1==B1, A2>B2)),insert([A1,A2],T,NT).
insert([A1,A2],[[B1,B2]|T],[[A1,A2],[B1,B2]|T]):- B1>A1;(B1==A1, B2>A2).
insert(X,[],[X]).