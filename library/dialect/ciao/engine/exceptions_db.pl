/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%% Migrated from Ciao to SWI-Prolog

:- module(exceptions_db, [asserta_catching/3,
			  retract_catching/3,
			  asserta_disabled/1,
			  retract_disabled/1,
			  catching/3,
			  disabled/1], []).

:- data catching/3, disabled/1.


asserta_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)).
asserta_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)), fail.

retract_catching(Ch, Er, Ha) :- retract_fact_nb(catching(Ch, Er, Ha)).
retract_catching(Ch, Er, Ha) :- asserta_fact(catching(Ch, Er, Ha)), fail.

asserta_disabled(Ref) :- asserta_fact(disabled(Ref)).
asserta_disabled(Ref) :- retract_fact_nb(disabled(Ref)), fail.

retract_disabled(Ref) :- retract_fact_nb(disabled(Ref)).
retract_disabled(Ref) :- asserta_fact(disabled(Ref)), fail.
