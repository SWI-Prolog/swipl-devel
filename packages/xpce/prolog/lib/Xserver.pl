/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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


:- module('Xserver',
	  [ ensure_x_server/2		% +Display, +Depth
	  ]).
:- use_module(library(pce)).


%	ensure_x_server(+Display, +Depth)
%	
%	Ensure the existence of a graphics environment for XPCE.  This
%	library uses the `head-less' server Xvfb if there is no X-server
%	in the environment.
%	
%	Currently this library deals with Windows (where no explicit
%	server is required) and Xfree using the Xfree socket naming
%	conventions.  Please send platform-specific code to
%	info@swi-prolog.org

ensure_x_server(_Display, _) :-
	current_prolog_flag(windows, true), !. % we have a display
ensure_x_server(_Display, _) :-
        getenv('DISPLAY', _), !.	       % Existing X11 display
ensure_x_server(Display, _) :-			
        atom_concat('/tmp/.X11-unix/X', Display, Socket),
	send(file(Socket), exists, @off), !,
        export_environment(Display).
ensure_x_server(Display, Depth) :-
        xauthority(Display, Auth),
        xlog(Display, Log),
        mcookie(Cookie),
        ignore(catch(delete_file(Auth), _, true)),
        sformat(Cmd1, '/usr/bin/X11/xauth -f ~w add :~w . ~s > ~w 2>&1',
                [ Auth, Display, Cookie, Log ]),
        shell(Cmd1),
        sformat(Cmd2, 'nohup /usr/bin/X11/Xvfb :~w -auth ~w -screen 0 640x480x~w >> ~w &',
                [ Display, Auth, Depth, Log ]),
        shell(Cmd2),
        sleep(5),
        export_environment(Display).

xauthority(Display, Auth) :-
        concat_atom(['/tmp/.X', Display, 'Authority'], Auth).
xlog(Display, Log) :-
        concat_atom(['/tmp/.X', Display, 'Log'], Log).

mcookie(Cookie) :-
        open(pipe(mcookie), read, Stream),
        read_line_to_codes(Stream, Cookie).

export_environment(Display) :-
        xauthority(Display, Auth),
        atom_concat(':', Display, Address),
        setenv('DISPLAY', Address),
        setenv('XAUTHORITY', Auth).
