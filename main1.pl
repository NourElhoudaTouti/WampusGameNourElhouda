:- abolish(hunter/2).
:- abolish(pit_at/2).
:- abolish(gold/2).
:- abolish(grab/2).
:- abolish(visited/2).
:- abolish(shooted/2).
:- abolish(score/1).
:- abolish(steps/1).
:- abolish(wumpus/2).

:- dynamic([
  hunter/2,
  wumpus/2,
  pit_at/2,
  gold/2,
  grab/2,
  shooted/2,
  visited/2,
  score/1,
  steps/1
]).

in_bounds(X,Y):-
  world(W,H),
  ((X > 0, X < W); (X > 0, X == W)),
  ((Y > 0, Y < H); (Y > 0, Y == H)),
  !.

isAdjacent((X,Y),(XT,YT)) :-
    (X =:= XT, Y =:= YT+1);
    (X =:= XT, Y =:= YT-1);
    (X =:= XT+1, Y =:= YT);
    (X =:= XT-1, Y =:= YT).

%---initilization---
world(4, 4).
hunter(1, 1).
visited(1, 1).
score(50).
steps(0).

%---what the hunter currently holds---
has_arrow(no) :- shooted(_, _), !.
has_arrow(yes).
has_gold(yes) :- gold(X,Y), grab(X,Y), !.
has_gold(no).

%---states of the players---

hunter_is(dead):-
	(hunter(X,Y), wumpus(X,Y), !; hunter(X,Y), pit_at(X,Y), !),
	score(S), N is S-1000,
  retractall(score(_)), asserta(score(N)).
hunter_is(alive).

wumpus_is(dead):-
  wumpus(X,Y),
	shooted(X,Y),
	score(S), N is S+1000,
  retractall(score(_)), asserta(score(N)).
wumpus_is(alive).


%----Perceptions---

has_glitter(yes) :- hunter(X,Y), gold(X,Y), !.
has_glitter(no).

has_stench(yes) :- hunter(X,Y), wumpus(A,B), isAdjacent((X,Y),(A,B)).
has_stench(no).

has_breeze(yes) :- hunter(X,Y), pit_at(A,B), isAdjacent((X,Y),(A,B)).
has_breeze(no).

has_scream(yes):- wumpus_is(dead), !.
has_scream(no).

perceptions([Stench, Breeze, Glitter, Scream]) :-
  has_stench(Stench),
  has_breeze(Breeze),
  has_glitter(Glitter),
  has_scream(Scream),
  !.


%---Moves---

action([move, X, Y]) :- move(X, Y).
move(X, Y) :-
  hunter(A,B),
  isAdjacent((X,Y),(A,B)),
  retractall(hunter(_, _)),
  asserta(hunter(X,Y)),
  assertz(visited(X,Y)),
  score(Sc),
  N is Sc-1,
  retractall(score(_)),
  assert(score(N)),
  steps(St),
  M is St+1,
  retractall(steps(_)),
  asserta(steps(M)),
  format("Hunter is Moving to room (~dx~d)~n", [X, Y]),
  !.
move(X,Y) :- format("Cannot move to room (~dx~d)~n !", [X, Y]).

action([shoot, X, Y]) :- shoot(X, Y).
shoot(X, Y) :-
  hunter(A, B),
  isAdjacent((A, B),(X, Y)),
  has_arrow(yes),
  assertz(shooted(X, Y)),
  score(S),
  N is S-10,
  retractall(score(_)),
  asserta(score(N)),
  format("Hunter Shot an arrow at room (~dx~d)~n !", [X, Y]),
  !.
shoot(_, _) :- has_arrow(no), write('I do not have an arrow anymore.'), !.

action(grab) :-
  hunter(X, Y),
  assertz(grab(X, Y)),
  retractall(gold(_,_)),
  score(S),
  N is S+49,
  retractall(score(_)),
  asserta(score(N)),
  format("Hunter Found Gold in room (~dx~d)~n !", [X, Y]),
  !.

action(random):-
  neighbors(N), length(N, L), random_between(1, L, R), nth1(R, N, [X, Y]),
  move(X, Y).


%---loop---
run :- runloop(0).
runloop(500) :- write('Reached max allowed moves!'), nl, action(exit), !.

runloop(T) :-
  perceptions(P),
  hunter(X,Y),
  format("At time ~d: hunter is in room (~dx~d), and senses ~p.",[T, X, Y, P]),nl,
  heuristic(P, A),
  action(A),
  hunter_is(dead) -> (write('You Lost, Wumpus is sill Alive...'), nl, action(exit), !);
  wumpus_is(dead) -> (write('You Won! You killed the Wumpus!'), nl, action(exit), !);
  (has_arrow(no), wumpus_is(alive)) -> (
    score(S), N is S-1000,
    retractall(score(_)), asserta(score(N)),
    write('You Lost, you have no weapon and Wumpus is sill Alive...'), nl, action(exit), !
  );
  Ti is T + 1,
  runloop(Ti).


%---end of the game---

action(exit) :- write('----Game has Finished---'), nl, print_result, nl.
print_result :-
  format("~n~tResult~t~40|~n"),
  score(S), steps(T),
  format("Steps: ~`.t ~d~40|", [T]), nl,
  format('Score: ~`.t ~d~40|', [S]), nl.