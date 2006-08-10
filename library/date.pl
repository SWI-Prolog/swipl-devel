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
	  [ date_time_value/3		% Extract values from a date/time struct
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
