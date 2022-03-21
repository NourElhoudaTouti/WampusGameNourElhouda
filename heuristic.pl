:- dynamic([
  stench_at/2,
  breeze_at/2
]).

no_pit_around(X, Y) :- neighbors([X, Y],[A, B]), has_pit(A, B, no).
no_wumpus_arround(X, Y) :- neighbors([X, Y],[A, B]), has_wumpus(A, B, no).
all_adjacent_visited(X, Y) :- neighbors([X, Y],[A, B]), visited(A, B).

neighbors([X, Y],[A, B]) :- A is X+1, in_bounds(A, Y), B is Y.
neighbors([X, Y],[A, B]) :- B is Y+1, in_bounds(X, B), A is X.
neighbors([X, Y],[A, B]) :- A is X-1, in_bounds(A, Y), B is Y.
neighbors([X, Y],[A, B]) :- B is Y-1, in_bounds(X, B), A is X.

neighbors(N) :- hunter(X, Y), findall([A, B], neighbors([X, Y], [A, B]), N).

feels_danger(yes) :- perceptions([yes, _, _, _]), !.
feels_danger(yes) :- perceptions([_, yes, _, _]), !.
feels_danger(no).

%---pit and wumpus---

has_pit(X, Y, yes) :- breeze_at(A, B), isAdjacent([X, Y],[A, B]), no_pit_around(A, B), asserta(pit_at(X,Y)).
has_pit(X, Y, yes) :- breeze_at(A, B), isAdjacent([X, Y],[A, B]), all_adjacent_visited(A, B), asserta(pit_at(X,Y)).

has_pit(X, Y, no):-
  (A is X+1, B is Y,   \+breeze_at(A, B));
  (A is X  , B is Y+1, \+breeze_at(A, B));
  (A is X-1, B is Y,   \+breeze_at(A, B));
  (A is X  , B is Y-1, \+breeze_at(A, B)).
has_pit(X, Y, no):- has_wumpus(X, Y, yes).

has_pit(X, Y, maybe) :-
  (A is X + 1, B is Y + 1, breeze_at(A, Y), breeze_at(X, B), !);
  (A is X + 1, B is Y - 1, breeze_at(A, Y), breeze_at(X, B), !);
  (A is X - 1, B is Y + 1, breeze_at(A, Y), breeze_at(X, B), !);
  (A is X - 1, B is Y - 1, breeze_at(A, Y), breeze_at(X, B), !).


has_wumpus(X, Y, yes) :- stench_at(A, B), isAdjacent([X, Y],[A, B]), no_wumpus_around(A, B), asserta(wumpus(X, Y)).
has_wumpus(X, Y, yes) :- stench_at(A, B), isAdjacent([X, Y], [A, B]), all_adjacent_visited(A, B), asserta(wumpus(X, Y)).
has_wumpus(X, Y, yes) :-
  (
    (A is X + 1, B is Y + 1, C is Y - 1, stench_at(A, Y), stench_at(X, B), stench_at(X, C), !);
    (A is X - 1, B is Y + 1, C is Y - 1, stench_at(A, Y), stench_at(X, B), stench_at(X, C), !);
    (A is X - 1, B is Y - 1, C is X + 1, stench_at(A, Y), stench_at(X, B), stench_at(C, Y), !);
    (A is X + 1, B is Y - 1, C is X - 1, stench_at(A, Y), stench_at(X, B), stench_at(C, Y), !)
  ),
  asserta(wumpus(X, Y)).

has_wumpus(X, Y, no):-
  (A is X + 1, B is Y, \+stench_at(A, B));
  (A is X,   B is Y + 1,\+stench_at(A, B));
  (A is X - 1, B is Y,\+stench_at(A, B));
  (A is X,   B is Y - 1,\+stench_at(A, B)).
has_wumpus(X, Y, no):- has_pit(X, Y, yes).

has_wumpus(X, Y, maybe) :-
  (A is X + 1, B is Y + 1, stench_at(A, Y), stench_at(X, B), !);
  (A is X + 1, B is Y - 1, stench_at(A, Y), stench_at(X, B), !);
  (A is X - 1, B is Y + 1, stench_at(A, Y), stench_at(X, B), !);
  (A is X - 1, B is Y - 1, stench_at(A, Y), stench_at(X, B), !).


%---heuristic---

heuristic([_, _, _, yes], exit) :- write('Wumpus is Killed!'), !.

heuristic([no, no, no, no], random) :- write('Nothing is perceived, Hunter will move to a random adjacent room...'), nl, !.

heuristic([_, _, yes, _], grab) :- write('Hunter perceives Glitter! '), !.

heuristic([yes, _, _, _], [shoot, X, Y]) :-
  write('The Wumpus is near.'),nl,
  hunter(A, B),
  assertz(stench_at(A, B)),
  neighbors([A, B], [C, D]),
  has_wumpus(C, D, yes),
  X is C, Y is D,
  !.

heuristic([yes, _, _, _], [move, X, Y]) :-
  write('The Wumpus is near.'),nl,
  hunter(A,B),
  assertz(stench_at(A,B)),
  safest_room(X, Y),
  !.

heuristic([yes, _, _, _], [shoot, X, Y]) :-
  write('The Wumpus is near.'),nl,
  hunter(A, B),
  assertz(stench_at(A, B)),
  neighbors([A, B], [C, D]),
  has_wumpus(C, D, maybe),
  X is C, Y is D,
  !.

heuristic([_, yes, _, _], [move, X, Y]) :-
  write('A pit is near.'),nl,
  hunter(A,B),
  assertz(breeze_at(A,B)),
  safest_room(X, Y),
  !.

safest_room(X, Y) :-
  findall(C, neighbors_cost(C), L),
  min_list(L, Min),
  index_of(L, Min, I),
  neighbors(N), nth0(I, N, [X, Y]),
  format("~n> Costs ~p for ~p selected ~p~n", [L, N, Min]).

neighbors_cost(C) :- hunter(X, Y), neighbors([X, Y], [A, B]), sum_cost(A, B, C).


sum_cost(X, Y, C) :- findall(Ci, cost(X, Y, Ci), Cs), sum_list(Cs, C).

cost(X, Y, C) :- \+visited(X, Y), feels_danger(no), C is 0.
cost(X, Y, C) :- visited(X, Y), feels_danger(yes), C is 10.
cost(X, Y, C) :- \+visited(X, Y), feels_danger(yes), C is 20.
cost(X, Y, C) :- has_pit(X, Y, maybe), C is 50.
cost(X, Y, C) :- has_wumpus(X, Y, maybe), C is 50.
cost(X, Y, C) :- \+visited(X, Y), feels_danger(yes), has_pit(X, Y, maybe), C is 80.
cost(X, Y, C) :- \+visited(X, Y), feels_danger(yes), has_wumpus(X, Y, maybe), C is 80.
cost(X, Y, C) :- has_pit(X, Y, yes), C is 100.
cost(X, Y, C) :- has_wumpus(X, Y, yes), C is 100.



index_of([H|_], H, 0):- !.
index_of([_|T], H, Index):- index_of(T, H, OldIndex), !, Index is OldIndex + 1.