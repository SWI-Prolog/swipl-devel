/* Demo program for the command line tracer

This program is a small route planner  for   a  cyclic map. It is meant
to be *traced* rather than merely run: it  is small enough to follow by
hand, yet it exercises all the ports and  most of the commands of the
SWI-Prolog command line tracer.  Use `?- help(debug).` for background.

The map below is undirected (see connected/3), so  the search runs into
cycles, dead ends and plenty of choice points.

  - Exercise 1 -- the four ports

	?- trace, route(amsterdam, arnhem, Route, Km).

    Type RET (or SPACE) to _creep_ from port to port.  Watch

      - `Call` on the way in and `Exit` on the way out.  Km is only
	filled in at the `Exit` ports: walk/5 sums the distance while
	the recursion *unwinds*.
      - `Fail` and `Redo` when not_seen/2 rejects a city we already
	visited and connected/3 offers the next road.
      - The depth counter between the parentheses.  It goes up with
	every nested call and is the level you can hand to `r` (retry)
	and `g` (backtrace).

    Useful here: `s` (skip over a not_seen/2 call you do not care
    about), `u` (up, finish the enclosing goal), `A` (alternatives),
    `g` (backtrace), `L` (list the current predicate).

  - Exercise 2 -- backtracking and search

	?- trace, route(amsterdam, groningen, Route, Km).

    groningen is on the map but has no roads, so this query fails
    after searching the whole map: about 1,300 ports.  Creeping is
    hopeless; this is what the search commands are for:

      - `/f` stops at the next `Fail` port.
      - `/c connected(arnhem, _, _)` stops when that goal is called.
      - `.` repeats the last search.
      - `+` sets a spy point on the goal at hand and `l` leaps from
	one spy point to the next.

  - Exercise 3 -- the exception port

	?- trace, route(nijmegen, eindhoven, Route, Km).

    The distance of the last road below is the *atom* '35' rather
    than a number, so the summation throws a type error on the way
    out.  Creep to the end and watch the `Exception` port travel up
    through every walk/5 frame.  At an `Exception` port, `m` shows
    the details of the exception, `r` retries the goal and `i`
    ignores it (pretends it succeeded).

    Note that `l` (leap) does *not* stop at an `Exception` port; use
    creep or a search as in exercise 2 to get there.

    You can also try these examples using the the __graphical tracer__,
    ?- gtrace.

Copyright: Public domain
*/

%!  route(+From, +To, -Route, -Distance) is nondet.
%
%   True when Route is the list of cities  visited when travelling From
%   To without passing the same city twice.   Distance is the length of
%   Route in kilometres.

route(From, To, [From|Route], Distance) :-
    walk(From, To, [From], Route, Distance).

%!  walk(+From, +To, +Seen, -Route, -Distance) is nondet.
%
%   Worker for route/4.  Seen holds the cities  visited so far, in
%   reverse order.  Note that Distance is computed while the recursion
%   unwinds, i.e. at the `Exit` ports.

walk(To, To, _Seen, [], 0).
walk(From, To, Seen, [Next|Route], Distance) :-
    connected(From, Next, Step),
    not_seen(Next, Seen),
    walk(Next, To, [Next|Seen], Route, Rest),
    Distance is Step+Rest.

%!  not_seen(+City, +Seen) is semidet.
%
%   True when City does not appear in Seen.  Deliberately written as a
%   recursive predicate rather than using memberchk/2, so that the
%   whole trace stays inside this file.

not_seen(_City, []).
not_seen(City, [Seen|Rest]) :-
    City \== Seen,
    not_seen(City, Rest).

%!  connected(?From, ?To, ?Km) is nondet.
%
%   The roads of road/3, usable in both directions.

connected(From, To, Km) :-
    road(From, To, Km).
connected(From, To, Km) :-
    road(To, From, Km).

%!  road(?From, ?To, ?Km) is nondet.
%
%   The map.  Note that zaandam is a dead end, that groningen has no
%   roads at all and that the distance of the last road is an atom
%   rather than a number.  All three are intentional; see the module
%   documentation above.

road(amsterdam,  zaandam,     15).   % a dead end
road(amsterdam,  utrecht,     40)./* Copyright: public domain
*/

road(amsterdam,  haarlem,     20).
road(haarlem,    leiden,      30).
road(leiden,     denhaag,     20).
road(denhaag,    rotterdam,   25).
road(rotterdam,  utrecht,     60).
road(utrecht,    amersfoort,  20).
road(utrecht,    arnhem,      65).
road(amersfoort, arnhem,      50).
road(arnhem,     nijmegen,    20).
road(nijmegen,   denbosch,    40).
road(denbosch,   eindhoven,  '35').   % typo: see exercise 3

city(groningen).                      % on the map, but unreachable
