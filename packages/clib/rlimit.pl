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

:- module(clib_rlimit,
	  [ rlimit/3			% +Limit, -Old, +New
	  ]).

%	rlimit(+Limit, -Old, +New)
%
%	Query and set POSIX resource limits.  Provided resources are:
%	
%		cpu		CPU time (seconds)
%		fsize		file-size (bytes)
%		data		size of data-segment (bytes)
%		stack		size of C-stack (bytes)
%		core		size of a core-dump (bytes)
%		rss		resident set size
%		memlock		locked-in-memory address space
%		nproc		number of processes
%		nofile		number of open files

:- initialization
	load_foreign_library(foreign(rlimit)).

