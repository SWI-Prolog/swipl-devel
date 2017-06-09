/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2016, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
:- use_module(library(shell), [shell/0]).

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
