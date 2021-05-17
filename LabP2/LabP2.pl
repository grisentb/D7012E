%Tom Brander (tombra-7)
:- set_prolog_flag(stack_limit, 10 000 000 000).
use_module(library(lists)).
%listSum
listSum([], 0).
listSum([L], L).
listSum([L|Tail],N) :- listSum(Tail,N1), N is N1+L.

%POWERSET
powerset(S,I,_,[]) :- length(S,N), I>=N.
powerset(S,I,J,R) :- partset(S,I,J,A), IN is I + 1, JN is J + 1, length(S,N), I < N, powerset(S,IN,JN, Tail), append(A,Tail,R),!.

partset(S, I,J,[[A,I,J]|Tail]) :- takeK(J,S,TK), dropK(I,TK,DK), listSum(DK,A), JN is J+1, length(S,N), J =< N, partset(S,I,JN,Tail).
partset(S,_,J,[]):- length(S,N), J>N.

%powerSetSort
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([[S,I,J]|T],Acc,Sorted):-insert([S,I,J],Acc,NAcc),i_sort(T,NAcc,Sorted).

%kSmallestSet(Input, K, Result_sums,Result)
kSmallestSet(Input,K,Result,Result_Sums) :- powerset(Input,0,1,PS), insert_sort(PS,S), getSublist(Input,S, R, RS), takeK(K, R,Result), takeK(K, RS,Result_Sums),!.

printKSmallestSet(Input,K) :- kSmallestSet(Input,K,R,RS),print(Input,K,R,RS).

print(Input,K, [R|RTail],[RS|RSTail]) :-  write('Sums: '), write(RS), write('\t\t'), write('Sublist: '), write(R), write('\n'), print(Input,K,RTail,RSTail).
%HELPERS-------------------------------------------------------------------------------------------------------------------------------------------------
ordered([A,B|L]) :- listSum(A,AS), listSum(B,BS), AS=<BS, ordered([B|L]).
ordered([[_]]).
ordered([[]]).

insert([X,XI,XJ],[[Y,YI,YJ]|T],[[Y,YI,YJ]|NT]):-X>Y,insert([X,XI,XJ],T,NT).
insert([X,XI,XJ],[[Y,YI,YJ]|T],[[X,XI,XJ],[Y,YI,YJ]|T]):- X=<Y.
insert(X,[],[X]).

takeK(1, [X|_],[X|[]]).
takeK(K,[X|Tail],[X|R_Tail]) :- takeK(K1,Tail,R_Tail), K is K1 + 1.

dropK(0,X,X).
dropK(1, [_|Tail],Tail).
dropK(K,[_|Tail],R):- K1 is K-1, dropK(K1,Tail,R).

conc([],L,L).
conc([X| L1], L2, [X|L3]) :-  conc(L1,L2,L3).

getSublist(_,[],[],[]).
getSublist(S,[[Sum,I,J]|Tail], [R|Tail1], [Sum|Tail2]) :- takeK(J,S,TK), dropK(I,TK,R), getSublist(S,Tail, Tail1, Tail2),!.
%---------------TESTS------------------------
func(X,Y):- XS is -1^X, Y is XS*X.
createTestList(_,1,[]).
createTestList(X, N, [Y|Tail]) :- N1 is N-1,func(X,Y), XS is X+1,  createTestList(XS, N1, Tail), !.
%createTestList(1,100,X), printKSmallestSet( X,15).
%printKSmallestSet( [24,-11,-34,42,-24,7,-19,21],6).
%printKSmallestSet( [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],8).

lengthOfPowerset(L, N) :- powerset(L,PS), length(PS,N).
