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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(files,
	[ can_open_file/2,		% +Path, +Mode
	  chdir/1			% +Dir
	]).

%%	can_open_file(+Path, +Mode)
%
%	Succeeds if the user has access to `File' in mode `Mode'.  Fails
%	silently if this is not the  case.   `Mode'  is  one  of  {read,
%	write, both}.  This used to be difficult.  Since we have
%	access_file/2 it is merely a Quintus compatibility predicate
%	and should be in quintus.pl.  We will leave it here for compatibility
%	reasons.
%
%	@deprecated Use access_file/2.

can_open_file(File, read) :- !,
	access_file(File, read).
can_open_file(File, write) :- !,
	(   exists_file(File)
	->  access_file(File, write)
        ;   path_dir_name(File, Dir),
	    access_file(Dir, write)
	).
can_open_file(File, both) :-
	access_file(File, read),
	access_file(File, write).

path_dir_name(File, Dir) :-
	file_base_name(File, Base),
	atom_concat(RawDir, Base, File),
	(   RawDir == ''
	->  Dir = '.'
	;   Dir = RawDir
	).

%%	chdir(+Dir) is det.
%
%	Change Working Directory.
%
%	@deprecated	Use using working_directory/2.

chdir(Dir) :-
	working_directory(_Old, Dir).
