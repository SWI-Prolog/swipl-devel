/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(backward_compatibility,
	  [ '$arch'/2,
	    '$version'/1,
	    '$home'/1,
	    '$argv'/1,
	    '$strip_module'/3,
	    displayq/1,
	    displayq/2,
	    sformat/2,			% -String, +Fmt
	    sformat/3,			% -String, +Fmt, +Args
	    concat/3,
	    read_variables/2,
	    read_variables/3,
	    feature/2,
	    set_feature/2,
	    substring/4,
	    flush/0,
	    write_ln/1,
	    proper_list/1,
	    free_variables/2,		% +Term, -Variables
	    checklist/2,		% :Goal, +List
	    sublist/3,			% :Goal, +List, -Sublist
	    convert_time/2,		% +Stamp, -String
	    convert_time/8,		% +String, -YMDmhs.ms
	    'C'/3,			% +List, -Head, -Tail
	    current_thread/2,		% ?Thread, ?Status
	    current_mutex/3,		% ?Mutex, ?Owner, ?Count
	    message_queue_size/2	% +Queue, -TermsWaiting
	  ]).

/** <module> Backward compatibility

This library defines predicates that used to exist in older version of
SWI-Prolog, but are considered obsolete as there functionality is neatly
covered by new features. Most often, these constructs are superceeded by
ISO-standard compliant predicates.

Please also note the existence of   quintus.pl and edinburgh.pl for more
compatibility predicates.
*/

'$arch'(Arch, unknown) :-
	current_prolog_flag(arch, Arch).
'$version'(Version) :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
	Version is 10000*Major+100*Minor+Patch.
'$home'(Home) :-
	current_prolog_flag(home, Home).
'$argv'(Argv) :-
	current_prolog_flag(argv, Argv).

%%	displayq(@Term) is det.
%%	displayq(+Stream, @Term) is det.
%	
%	Write term ignoring operators and quote atoms.
%	
%	@deprecated Use write_term/3 or write_canonical/2.

displayq(Term) :-
	write_term(Term, [ignore_ops(true),quoted(true)]).
displayq(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true),quoted(true)]).


%%	sformat(-String, +Format, +Args) is det.
%%	sformat(-String, +Format) is det.
%
%	@deprecated Use format/3 as =|format(string(String), ...)|=

:- module_transparent sformat/2, sformat/3.

sformat(String, Format) :-
	format(string(String), Format, []).
sformat(String, Format, Arguments) :-
	format(string(String), Format, Arguments).

%%	concat(+Atom1, +Atom2, -Atom) is det.
%
%	@deprecated Use ISO atom_concat/3

concat(A, B, C) :-
	atom_concat(A, B, C).

%%	read_variables(-Term, -Bindings) is det.
%%	read_variables(+In:stream, -Term, -Bindings) is det.
%
%	@deprecated Use ISO read_term/[2,3].

read_variables(Term, Vars) :-
	read_term(Term, [variable_names(Vars)]).

read_variables(Stream, Term, Vars) :-
	read_term(Stream, Term, [variable_names(Vars)]).

%%	feature(?Key, ?Value) is nondet.
%%	set_feature(+Key, @Term) is det.
%
%	Control Prolog flags.
%
%	@deprecated Use ISO current_prolog_flag/2 and set_prolog_flag/2.

feature(Key, Value) :-
	current_prolog_flag(Key, Value).

set_feature(Key, Value) :-
	set_prolog_flag(Key, Value).

%%	substring(+String, +Offset, +Length, -Sub)
%
%	Predecessor of sub_string using 1-based Offset.
%	
%	@deprecated Use sub_string/5.

substring(String, Offset, Length, Sub) :-
	Offset0 is Offset - 1,
	sub_string(String, Offset0, Length, _After, Sub).

%%	flush is det.
%
%	@deprecated use ISO flush_output/0.

flush :-
	flush_output.

%%	write_ln(X) is det
%
%	@deprecated Use writeln(X).

write_ln(X) :-
	write(X), nl.

%%	proper_list(+List)
%
%	Old SWI-Prolog predicate to check for a list that really ends
%	in a [].  There is not much use for the quick is_list, as in
%	most cases you want to process the list element-by-element anyway.
%	
%	@deprecated Use ISO is_list/1.

proper_list(List) :-
	is_list(List).

%%	free_variables(+Term, -Variables)
%	
%	Return  a  list  of  unbound  variables    in   Term.  The  name
%	term_variables/2 is more widely used.
%	
%	@deprecated Use term_variables/2.

free_variables(Term, Variables) :-
	term_variables(Term, Variables).

%%	checklist(:Goal, +List)
%	
%	@deprecated Use maplist/2

:- module_transparent
	checklist/2,
	sublist/3.

checklist(Goal, List) :-
	maplist(Goal, List).

%%	sublist(:Goal, +List1, ?List2)
%	
%	Succeeds if List2 unifies with a list holding those terms for wich
%	call(Goal, Elem) succeeds.
%	
%	@deprecated Use include/3 from library(apply)
%	@compat	DEC10 library

sublist(_, [], []) :- !.
sublist(Goal, [H|T], Sub) :-
	call(Goal, H), !, 
	Sub = [H|R], 
	sublist(Goal, T, R).
sublist(Goal, [_|T], R) :-
	sublist(Goal, T, R).


%%	strip_module(+Term, -Module, -Plain)
%	
%	This used to be an internal predicate.  It was added to the XPCE
%	compatibility library without $ and  since   then  used  at many
%	places. From 5.4.1 onwards strip_module/3 is  built-in and the $
%	variation is added here for compatibility.
%	
%	@deprecated Use strip_module/3.

:- module_transparent
	'$strip_module'/3.

'$strip_module'(Term, Module, Plain) :-
	strip_module(Term, Module, Plain).

%%	convert_time(+Stamp, -String)
%
%	Convert  a time-stamp as  obtained though get_time/1 into a  textual
%	representation  using the C-library function ctime().  The  value is
%	returned  as a  SWI-Prolog string object  (see section  4.23).   See
%	also convert_time/8.
%	
%	@deprecated Use format_time/3.


convert_time(Stamp, String) :-
	format_time(string(String), '%+', Stamp).

%%	convert_time(+Stamp, -Y, -Mon, -Day, -Hour, -Min, -Sec, -MilliSec)
%
%	Convert   a  time  stamp,   provided  by   get_time/1,   time_file/2,
%	etc.   Year is  unified with the year,  Month with the month  number
%	(January  is 1), Day  with the day of  the month (starting with  1),
%	Hour  with  the hour  of the  day (0--23),  Minute  with the  minute
%	(0--59).   Second with the  second (0--59) and MilliSecond with  the
%	milliseconds  (0--999).  Note that the latter might not  be accurate
%	or  might always be 0, depending  on the timing capabilities of  the
%	system.  See also convert_time/2.
%	
%	@deprecated Use stamp_date_time/3.

convert_time(Stamp, Y, Mon, Day, Hour, Min, Sec, MilliSec) :-
	stamp_date_time(Stamp,
			date(Y, Mon, Day,
			     Hour, Min, FSec,
			     _, _, _),
			local),
	Sec is integer(float_integer_part(FSec)),
	MilliSec is integer(float_fractional_part(FSec)*1000).

%%	'C'(?List, ?Head, ?Tail) is det.
%
%	Used to be generated by DCG.  Some people appear to be using in
%	in normal code too.
%	
%	@deprecated Do not use in normal code; DCG no longer generates it.

'C'([H|T], H, T).


%%	current_thread(?Thread, ?Status) is nondet.
%
%	@deprecated Replaced by thread_property/2

current_thread(Thread, Status) :-
	nonvar(Thread), !,
	catch(thread_property(Thread, status(Status)),
	      error(existence_error(thread, _), _),
	      fail).
current_thread(Thread, Status) :-
	thread_property(Thread, status(Status)).

%%	current_mutex(?Mutex, ?Owner, ?Count) is nondet.
%
%	@deprecated Replaced by mutex_property/2

current_mutex(Mutex, Owner, Count) :-
	nonvar(Mutex), !,
	catch(mutex_property(Mutex, status(Status)),
	      error(existence_error(mutex, _), _),
	      fail),
	map_mutex_status(Status, Owner, Count).
current_mutex(Mutex, Owner, Count) :-
	mutex_property(Mutex, status(Status)),
	map_mutex_status(Status, Owner, Count).

map_mutex_status(unlocked, [], 0).
map_mutex_status(locked(Owner, Count), Owner, Count).


%%	message_queue_size(+Queue, -Size) is det.
%
%	True if Queue holds Size terms.
%	
%	@deprecated Please use message_queue_property(Queue, Size)

message_queue_size(Queue, Size) :-
	message_queue_property(Queue, size(Size)).
