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

:- module(file_utils, [file_terms/2, copy_stdout/1, 
	  file_to_string/2,
	  file_to_string/3,
	  string_to_file/2,
	  stream_to_string/2,
	  stream_to_string/3,
	  output_to_file/2],
        [assertions,isomodes]).

% :- use_module(library(read), [read/1]).
:- use_module(library(streams)).
:- use_module(library(strings)).

% TODO: Integrate with other stream module?
:- doc(title,"File/Stream Utilities").

:- doc(author,"The CLIP Group").

:- doc(module,"This module implements a collection of predicates to
   read/write files (or streams) from/to several sources (lists of
   terms, strings, predicate output, etc.), in a compact way.").

% (pp) probably redundant
%:- pred file_terms(@File, ?Terms) : sourcename(File) =>  list(Terms) 
%   # "Transform a file @var{File} to/from a list of terms @var{Terms}.".


:- pred file_terms(File, Terms) : (sourcename(File), var(Terms)) => list(Terms) 
   # "Unifies @var{Terms} with the list of all terms in @var{File}.".

:- pred file_terms(File, Terms) : sourcename * list
   # "Writes the terms in list @var{Terms} (including the ending '.')
      onto file @var{File}.".

file_terms(File, Terms) :- var(Terms), !,
        open_input(File, IO),
        read(T),
        read_terms(T, Terms),
        close_input(IO).
file_terms(File, Terms) :-
        open_output(File, IO),
        display_term_list(Terms),
        close_output(IO).        

read_terms(end_of_file, []) :- !.
read_terms(T, [T|Ts]) :-
        read(T1),
        read_terms(T1, Ts).

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

:- pred copy_stdout(+File): sourcename 
   # "Copies file @var{File} to standard output.".

copy_stdout(File) :-
 	open_input(File, IO),
	repeat,
	  get_code(Code),
	  ( Code = -1
	  ; put_code(Code),
	    fail
	  ),
	!,
	close_input(IO).


:- pred file_to_string(+FileName, -String) : sourcename(FileName) => string(String)
   # "Reads all the characters from the file @var{FileName}
      and returns them in @var{String}.".


file_to_string(File, String) :-
	file_to_string(File, String, []).


:- pred file_to_string(+FileName, -String, ?Tail) : sourcename(FileName) =>
   string(String) # "Reads all the characters from the file
   @var{FileName} and returns them in @var{String}.  @var{Tail} is the
   end of @var{String}.".


file_to_string(File, String, Tail) :-
        open(File, read, Stream),
        stream_to_string(Stream, String, Tail).

:- pred string_to_file(+String, +FileName): (string(String), sourcename(FileName))
   # "Reads all the characters from the string @var{String} and writes
    them to file @var{FileName}.".

string_to_file(String, File) :-
	open(File, write, Stream),
	write_string(Stream, String),
	close(Stream).


:- pred stream_to_string(+Stream, -String): stream(Stream) =>
   string(String) # "Reads all the characters from @var{Stream},
   returns them in @var{String}, and closes @var{Stream}.".

stream_to_string(Stream, String) :-
	stream_to_string(Stream, String, []).


:- pred stream_to_string(+Stream, -String, ?Tail): stream(Stream) #
   "Reads all the characters from @var{Stream}, returns them in
   @var{String}, and closes @var{Stream}.  @var{Tail} is the end of
   @var{String}".

stream_to_string(Stream, String, Tail) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_close(String, Tail),
        set_input(OldIn),
        close(Stream).

read_to_close(L, T) :-
        get_code(C),
        read_to_close1(C, L, T).

read_to_close1(-1, T, T) :- !.
read_to_close1(C, [C|L], T) :-
        get_code(C1),
        read_to_close1(C1, L, T).

:- meta_predicate output_to_file(goal, ?).
output_to_file(Goal, File) :-
	open(File, write, OS),
	current_output(CO),
	set_output(OS),
	call(Goal),
	set_output(CO),
	close(OS).
