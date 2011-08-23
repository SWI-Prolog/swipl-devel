/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

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

:- module(ciao_system,
	  [ pause/1,
	    time/1,
%	    datime/1,
%	    datime/9,
%	    getenvstr/2,
%	    setenvstr/2,
%	    extract_paths/2,
%	    get_pid/1,
	    current_host/1,
%	    current_executable/1,
%	    umask/2,
%	    make_directory/2,
%	    make_directory/1,
%	    make_dirpath/2,
%	    make_dirpath/1,
	    working_directory/2,
%	    cd/1,
	    shell/0,
	    shell/1,
	    shell/2,
%	    system/1,
%	    system/2,
	    popen/3,
%	    exec/4,
%	    exec/3,
%	    exec/8,
%	    wait/3,
%	    directory_files/2,
%	    mktemp/2,
%	    file_exists/1,
%	    file_exists/2,
%	    file_property/2,
%	    file_properties/6,
%	    modif_time/2,
%	    modif_time0/2,
%	    fmode/2,
%	    chmod/2,
%	    chmod/3,
	    delete_file/1,
	    delete_directory/1,
	    rename_file/2
%	    cyg2win/3
	  ]).
:- use_module('../../socket').

/** <module> Ciao system utlities


@tbd	Complete.  Most of this is easily emulated.
*/

%%	pause(+Seconds)
%
%	Make this thread sleep for some Seconds.

pause(Seconds) :-
	sleep(Seconds).

%%	time(-Time)
%
%	Time is unified  with  the  number   of  seconds  elapsed  since
%	January, 1, 1970 (UTC).

time(Time) :-
	get_time(Time).


%%	current_host(-Host) is det.
%
%	Hostname is unified with the fully qualified name of the host.

current_host(Host) :-
	gethostname(Host).

%%	popen(+Command, +Mode, +Stream) is det.
%
%	Open a pipe to process Command in a  new shell with a given Mode
%	and return a communication Stream (as in UNIX popen(3)). If Mode
%	is read the output from the process   is sent to Stream. If Mode
%	is write, Stream is sent as input  to the process. Stream may be
%	read  from  or  written  into  using  the  ordinary  stream  I/O
%	predicates. Stream must be  closed   explicitly  using  close/1,
%	i.e., it is not closed automatically when the process dies. Note
%	that popen/2 is defined in ***x  as using /bin/sh, which usually
%	does not exist in Windows systems.  In   this  case,  a sh shell
%	which comes with Windows is used.

popen(Command, Mode, Stream) :-
	open(pipe(Command), Mode, Stream).
