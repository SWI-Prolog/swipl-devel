/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(date,
	  [ date_time_value/3,		% ?Field, ?DaTime, ?Value
	    parse_time/2		% +Date, -Stamp
	  ]).

%%	date_time_value(?Field:atom, +Struct:datime, -Value) is nondet.
%
%	Extract values from a date-time structure.  Provided fields are
%	
%		| year | integer | |
%		| month | 1..12 | |
%		| day | 1..31 | |
%		| hour | 0..23 | |
%		| minute | 0..59 | |
%		| second | 0.0..60.0 | |
%		| utc_offset | integer | Offset to UTC in seconds (positive is west) |
%		| daylight_saving | bool | Name of timezone; fails if unknown |
%		| date | date(Y,M,D) | |
%		| time | time(H,M,S) | |

date_time_value(year,		 date(Y,_,_,_,_,_,_,_,_), Y).
date_time_value(month,		 date(_,M,_,_,_,_,_,_,_), M).
date_time_value(day,		 date(_,_,D,_,_,_,_,_,_), D).
date_time_value(hour,		 date(_,_,_,H,_,_,_,_,_), H).
date_time_value(minute,		 date(_,_,_,_,M,_,_,_,_), M).
date_time_value(second,		 date(_,_,_,_,_,S,_,_,_), S).
date_time_value(utc_offset,	 date(_,_,_,_,_,_,O,_,_), O).
date_time_value(time_zone,	 date(_,_,_,_,_,_,_,Z,_), Z) :- Z \== (-).
date_time_value(daylight_saving, date(_,_,_,_,_,_,_,_,D), D) :- D \== (-).

date_time_value(date,		 date(Y,M,D,_,_,_,_,_,_), date(Y,M,D)).
date_time_value(time,		 date(_,_,_,H,M,S,_,_,_), time(H,M,S)).

%%	parse_time(+Text, -Stamp) is det.
%
%	Stamp is a timestamp created from   parsing Text. Currently only
%	deals with RFC1123 from the HTTP protocol.

parse_time(Text, Stamp) :-
	atom_codes(Text, Codes),
	phrase(date(Y,Mon,D,H,Min,S,UTCOffset), Codes),
	date_time_stamp(date(Y,Mon,D,H,Min,S,UTCOffset,-,-), Stamp).

% RFC 1123: "Fri, 08 Dec 2006 15:29:44 GMT"
date(Y,Mon,D,H,Min,S,0) -->
	day_name(_), ",", ws,
	day_of_the_month(D), ws,
	month_name(Mon), ws,
	year(Y), ws,
	hour(H), ":", minute(Min), ":", second(S), ws,
	(   "GMT"
	->  []
	;   []
	).

day_name(1) --> "Sun".
day_name(2) --> "Mon".
day_name(3) --> "Tue".
day_name(4) --> "Wed".
day_name(5) --> "Thu".
day_name(6) --> "Fri".
day_name(7) --> "Sat".

month_name(1) --> "Jan".
month_name(2) --> "Feb".
month_name(3) --> "Mar".
month_name(4) --> "Apr".
month_name(5) --> "May".
month_name(6) --> "Jun".
month_name(7) --> "Jul".
month_name(8) --> "Aug".
month_name(9) --> "Sep".
month_name(10) --> "Oct".
month_name(11) --> "Nov".
month_name(12) --> "Dec".

day_of_the_month(N) --> int2digit(N), { between(1, 31, N) }.
hour(N)             --> int2digit(N), { between(0, 23, N) }.
minute(N)	    --> int2digit(N), { between(0, 59, N) }.
second(N)           --> int2digit(N), { between(0, 60, N) }. % leap second

int2digit(N) -->
	digit(D0),
	digit(D1),
	{ N is D0*10+D1 }.
	  
year(Y) -->
	digit(D0),
	digit(D1),
	digit(D2),
	digit(D3),
	{ Y is D0*1000+D1*100+D2*10+D3 }.

digit(D) -->
	[C],
	{ code_type(C, digit(D)) }.

ws -->
	" ", !,
	ws.
ws -->
	[].
