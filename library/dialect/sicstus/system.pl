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

:- module(sicstus_system,
	  [ environ/2,			% ?Name, ?Value

	    exec/3,			% +Command, -Streams, -PID
	    wait/2,			% +PID, -Status
	    pid/1,			% -PID

	    now/1,			% -TimeStamp
	    datime/1,			% -DaTime
	    datime/2,			% +TimeStamp, -DaTime
	    sleep/1,			% +Seconds

	    shell/0,
	    shell/1,			% +Command
	    shell/2,			% +Command, -Status

	    system/0,
	    system/1,			% +Command
	    system/2,			% +Command, -Status

	    popen/3,			% +Command, +Mode, -Stream

	    host_name/1,		% -HostName

	    working_directory/2,	% -Old, +New
	    make_directory/1,		% +DirName
	    file_exists/1,		% +FileName
	    delete_file/1,		% +FileName
	    rename_file/2,		% +Old, +New
	    mktemp/2,			% +Template, -FileName
	    tmpnam/1			% -FileName
	  ]).
:- use_module(library(process)).
:- use_module(library(socket)).

:- multifile sicstus:rename_module/2.

sicstus:rename_module(system, sicstus_system).

/** <module> SICStus-3 library system



@tbd	This library is incomplete
*/

%%	environ(?Name, ?Value) is nondet.
%
%	True if Value an atom associated   with the environment variable
%	Name.
%
%	@tbd	Mode -Name is not supported

environ(Name, Value) :-
	getenv(Name, Value).


		 /*******************************
		 *	      PROCESSES		*
		 *******************************/

%%	exec(+Command, +Streams, -PID)
%
%	SICStus 3 compatible implementation of  exec/3   on  top  of the
%	SICStus 4 compatible process_create/3.
%
%	@bug	The SICStus version for Windows seems to hand Command
%		directly to CreateProcess(). We hand it to
%
%		  ==
%		  %COMSPEC% /s /c "Command"
%		  ==
%
%		In case of conflict, it is adviced to use
%		process_create/3

exec(Command, Streams, PID) :-
	Streams = [In, Out, Error],
	shell(Shell, Command, Argv),
	process_create(Shell, Argv,
		       [ stdin(In),
			 stdout(Out),
			 stderr(Error),
			 process(PID)
		       ]).

shell(Shell, Command, ['/s', '/c', Command]) :-
	current_prolog_flag(windows, true), !,
	getenv('COMSPEC', Shell).
shell('/bin/sh', Command, ['-c', Command]).

%%	wait(+PID, -Status)
%
%	Wait for processes created using exec/3.
%
%	@see exec/3

wait(PID, Status) :-
	process_wait(PID, Status).

%%	pid(-PID)
%
%	Process ID of the current process.
%
%	@compat sicstus.

pid(PID) :-
	current_prolog_flag(pid, PID).

%%	now(-When) is det.
%
%	Unify when with the current time-stamp
%
%	@compat sicstus

now(When) :-
	get_time(Now),
	When is integer(Now).

%%	datime(+When, -Datime) is det.
%
%	True when Datime is a  datime/6   record  that reflects the time
%	stamp When.
%
%	@compat sicstus

datime(When, datime(Year,Month,Day,Hour,Min,Sec)) :-
	stamp_date_time(When, date(Year,Month,Day,Hour,Min,SecF,_,_,_), local),
	Sec is integer(SecF).

%%	datime(-Datime) is det.
%
%	Unifies Datime with the current  date   and  time  as a datime/6
%	record  of  the  form  datime(Year,Month,Day,Hour,Min,Sec).  All
%	fields are integers.
%
%	@compat sicstus

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
	get_time(Now),
	stamp_date_time(Now, date(Year,Month,Day,Hour,Min,SecF,_,_,_), local),
	Sec is integer(SecF).


%%	system.
%%	system(+Command).
%%	system(+Command, -Status).
%
%	Synomyms for shell/0, shell/1 and shell/2.
%
%	@compat sicstus.

system :- shell.
system(Command) :- shell(Command).
system(Command, Status) :- shell(Command, Status).

%%	popen(+Command, +Mode, ?Stream)
%
%	@compat sicstus

popen(Command, Mode, Stream) :-
	open(pipe(Command), Mode, Stream).

%%	host_name(-HostName)
%
%	@compat sicstus
%	@see gethostname/1

host_name(HostName) :-
	gethostname(HostName).


		 /*******************************
		 *	 FILE OPERATIONS	*
		 *******************************/

%%	mktemp(+Template, -File) is det.
%
%	Interface to the Unix function.  This emulation uses
%	tmp_file/2 and ignores Template.
%
%	@compat sicstus
%	@deprecated This interface is a security-risc.  Use
%	tmp_file_stream/3.

mktemp(_Template, File) :-
	tmp_file(mkstemp, File).

%%	tmpnam(-FileName)
%
%	Interface to tmpnam(). This emulation uses tmp_file/2.
%
%	@compat sicstus
%	@deprecated This interface is a security-risc.  Use
%	tmp_file_stream/3.

tmpnam(File) :-
	tmp_file(tmpnam, File).

%%	file_exists(+FileName) is semidet.
%
%	True if a file named FileName exists.
%
%	@compat sicstus

file_exists(FileName) :-
	exists_file(FileName).
