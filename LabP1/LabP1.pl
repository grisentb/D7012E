%Tom brander (tombra-7)
:-set_prolog_flag(answer_write_options,[max_depth(0)]).
%state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package)

%---------------------Move Robot-----------------------------
% se till att det den har i den andra handen också följer med i nästa rum!
action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):- 
    (Robot == r2,Move = [move, r1],Hand1 == steel_key, Hand2 == nothing, State = state(r1, Hand1/Hand2, r1, Brass_Key, Package));
    (Robot == r2,Move = [move, r1],Hand1 == steel_key, Hand2 == brass_key, State = state(r1, Hand1/Hand2, r1, r1, Package));
    (Robot == r2,Move = [move, r1],Hand1 == steel_key, Hand2 == package, State = state(r1, Hand1/Hand2, r1, Brass_Key, r1));
    (Robot == r2,Move = [move, r1],Hand2 == steel_key, Hand1 == nothing, State = state(r1, Hand1/Hand2, r1, Brass_Key, Package));
    (Robot == r2,Move = [move, r1],Hand2 == steel_key, Hand1 == brass_key, State = state(r1, Hand1/Hand2, r1, r1, Package));
    (Robot == r2,Move = [move, r1],Hand2 == steel_key, Hand1 == package, State = state(r1, Hand1/Hand2, r1, Brass_Key, r1)).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):- 
    (Robot == r1,Move = [move, r3],Hand1 == brass_key, Hand2 == nothing, State = state(r3, Hand1/Hand2, Steel_Key, r3, Package));
    (Robot == r1,Move = [move, r3],Hand1 == brass_key, Hand2 == steel_key, State = state(r3, Hand1/Hand2, r3, r3, Package));
    (Robot == r1,Move = [move, r3],Hand1 == brass_key, Hand2 == package, State = state(r3, Hand1/Hand2, Steel_Key, r3, r3));
    (Robot == r1,Move = [move, r3],Hand2 == brass_key, Hand1 == nothing, State = state(r3, Hand1/Hand2, Steel_Key, r3, Package));
    (Robot == r1,Move = [move, r3],Hand2 == brass_key, Hand1 == steel_key, State = state(r3, Hand1/Hand2, r3, r3, Package));
    (Robot == r1,Move = [move, r3],Hand2 == brass_key, Hand1 == package, State = state(r3, Hand1/Hand2, Steel_Key, r3, r3)).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):- 
    (Robot == r1,Move = [move, r2],Hand1 == steel_key, Hand2 == nothing, State = state(r2, Hand1/Hand2, r2, Brass_Key, Package));
    (Robot == r1,Move = [move, r2],Hand1 == steel_key, Hand2 == brass_key, State = state(r2, Hand1/Hand2, r2, r2, Package));
    (Robot == r1,Move = [move, r2],Hand1 == steel_key, Hand2 == package, State = state(r2, Hand1/Hand2, r2, Brass_Key, r2));
    (Robot == r1,Move = [move, r2],Hand2 == steel_key, Hand1 == nothing, State = state(r2, Hand1/Hand2, r2, Brass_Key, Package));
    (Robot == r1,Move = [move, r2],Hand2 == steel_key, Hand1 == brass_key, State = state(r2, Hand1/Hand2, r2, r2, Package));
    (Robot == r1,Move = [move, r2],Hand2 == steel_key, Hand1 == package, State = state(r2, Hand1/Hand2, r2, Brass_Key, r2)).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):- 
    (Robot == r3,Move = [move, r1],Hand1 == brass_key, Hand2 == nothing, State = state(r1, Hand1/Hand2, Steel_Key, r1, Package)); 
    (Robot == r3,Move = [move, r1],Hand1 == brass_key, Hand2 == steel_key, State = state(r1, Hand1/Hand2, r1, r1, Package));
    (Robot == r3,Move = [move, r1],Hand1 == brass_key, Hand2 == package, State = state(r1, Hand1/Hand2, Steel_Key, r1, r1));
    (Robot == r3,Move = [move, r1],Hand2 == brass_key, Hand1 == nothing, State = state(r1, Hand1/Hand2, Steel_Key, r1, Package));
    (Robot == r3,Move = [move, r1],Hand2 == brass_key, Hand1 == steel_key, State = state(r1, Hand1/Hand2, r1, r1, Package));
    (Robot == r3,Move = [move, r1],Hand2 == brass_key, Hand1 == package, State = state(r1, Hand1/Hand2, Steel_Key, r1, r1)).

%-----------------------Take item----------------------
action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Robot == Steel_Key,Hand1 == nothing, Hand2 \= steel_key, State = state(Robot, steel_key/Hand2, Steel_Key, Brass_Key, Package), Move = [take, steel_key]);
    (Robot == Steel_Key,Hand2 == nothing, Hand1 \= steel_key, State = state(Robot,Hand1/steel_key, Steel_Key, Brass_Key, Package), Move = [take, steel_key]).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Robot == Brass_Key,Hand1 == nothing, Hand2 \= brass_key, State = state(Robot, brass_key/Hand2, Steel_Key, Brass_Key, Package), Move = [take, brass_key]);
    (Robot == Brass_Key,Hand2 == nothing, Hand1 \= brass_key, State = state(Robot,Hand1/brass_key, Steel_Key, Brass_Key, Package), Move = [take, brass_key]).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Robot == Package,Hand1 == nothing, Hand2 \= package, State = state(Robot, package/Hand2, Steel_Key, Brass_Key, Package),Move = [take, package]);
    (Robot == Package,Hand2 == nothing, Hand1 \= package, State = state(Robot,Hand1/package, Steel_Key, Brass_Key, Package),Move = [take, package]).

%----------------------Drop item----------------------------
action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Hand1 == steel_key, State = state(Robot, nothing/Hand2, Steel_Key, Brass_Key, Package),Move = [drop, steel_key]);
    (Hand2 == steel_key, State = state(Robot, Hand1/nothing, Steel_Key, Brass_Key, Package),Move = [drop, steel_key]).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Hand1 == brass_key, State = state(Robot, nothing/Hand2, Steel_Key, Brass_Key, Package),Move = [drop, brass_key]);
    (Hand2 == brass_key, State = state(Robot, Hand1/nothing, Steel_Key, Brass_Key, Package),Move = [drop, brass_key]).

action(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Package), State, Move):-
    (Hand1 == package, State = state(Robot, nothing/Hand2, Steel_Key, Brass_Key, Package),Move = [drop, package]);
    (Hand2 == package, State = state(Robot, Hand1/nothing, Steel_Key, Brass_Key, Package),Move = [drop, package]).

%-----------------------------------SOVLVE--------------------------------------
solve(state(Robot, Hand1/Hand2, Steel_Key, Brass_Key, Dest), Dest, _, []).
solve(State, Dest, N, Trace):-
    N>0,
    action(State, NewState, [Action, Item]),
    solve(NewState, Dest, N-1, TraceCo),
    Trace = [action(Action , Item) | TraceCo].

temp(state(r1,nothing/nothing, r1, r2 ,r3)).
start(T, N) :- temp(X), solve(X,r2, N, T).