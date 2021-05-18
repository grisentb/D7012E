/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */

%Tom Brander (tombra-7)

%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
:- ensure_loaded('stupid.pl').
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

initialize(InitialState, 1) :- initBoard(InitialState), writeln('Initializing').



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

winner(State, Plyr):- terminal(State), count(State, X, O), X>O, Plyr is 2.
winner(State, Plyr):- terminal(State), count(State, X, O), X<O, Plyr is 1.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State):- terminal(State), count(State, X, O), X==O.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State):- moves(1, State, MvList0), moves(2, State, MvList1), MvList0 == [], MvList1 == [].



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
moves(Plyr, State, MvList):- findall(Move, validmove(Plyr, State, Move), Moves), sort(Moves,MvList), !.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
nextState(Plyr, Move, State, NewState, NextPlyr):-
	findall(Disks, testmoves(Move, Plyr, State, [Disks]), Pieces), 
	setAll([Move|Pieces], State, NewState, Plyr),
	opponent(Plyr, Opponent),
	moves(Opponent, NewState, MvList),
	MvList \= [],
	NextPlyr is Opponent.

nextState(Plyr, Move, State, NewState, NextPlyr):-
	findall(Disks, testmoves(Move, Plyr, State, [Disks]), Pieces),
	setAll([Move|Pieces], State, NewState, Plyr), 
	opponent(Plyr, Opponent), 
	moves(Opponent, NewState, MvList), 
	MvList == [],
	NextPlyr is Plyr.
	
setAll([], State, State, _).
setAll([Disk|Disks], State, NewState, Plyr):- set(State, NS, Disk, Plyr), setAll(Disks, NS, NewState, Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

%[[., ., ., ., ., .],[1, ., 1, ., 1, .],[2, 2, 1, 1, ., .],[., ., 1, 1, ., .],[., ., 1, ., ., .],[., ., 2, ., ., .]]
validmove(Plyr, State, Proposed):- testmoves(Proposed, Plyr, State,_).

testmoves([X,Y], Plyr, State, Disks):- get(State, [X,Y], V), V = '.', opponent(Plyr, Opponent), (
	E is X + 1, get(State, [E,Y], O), O = Opponent, sequenceWithEnd(State, [E,Y], 1, 0, Opponent, Plyr, Disks); %East
	W is X - 1, get(State, [W,Y], O), O = Opponent, sequenceWithEnd(State, [W,Y], -1, 0, Opponent, Plyr, Disks); %West
	S is Y + 1, get(State, [X,S], O), O = Opponent, sequenceWithEnd(State, [X,S], 0, 1, Opponent, Plyr, Disks); %South
	N is Y - 1, get(State, [X,N], O), O = Opponent, sequenceWithEnd(State, [X,N], 0, -1, Opponent, Plyr, Disks); %North
	SE1 is X + 1, SE2 is Y + 1, get(State, [SE1,SE2], O), O = Opponent, sequenceWithEnd(State, [SE1,SE2], 1, 1, Opponent, Plyr, Disks); %SouthEast
	NE1 is X + 1, NE2 is Y - 1, get(State, [NE1,NE2], O), O = Opponent, sequenceWithEnd(State, [NE1,NE2], 1, -1, Opponent, Plyr, Disks); %NorthEast
	SW1 is X - 1, SW2 is Y + 1, get(State, [SW1,SW2], O), O = Opponent,sequenceWithEnd(State, [SW1,SW2], -1, 1, Opponent, Plyr, Disks); %SouthWest
	NW1 is X - 1, NW2 is Y - 1, get(State, [NW1,NW2], O), O = Opponent,sequenceWithEnd(State, [NW1,NW2], -1, -1, Opponent, Plyr, Disks)).	%NorthWest

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

h(State,100) :- winner(State,1), !.
h(State,-100) :- winner(State,2), !.
h(State,0) :- tie(State), !.
h(_,0).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-150).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(150).



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