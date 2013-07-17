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

:- module(streams, [ open_null_stream/1, % ?Stream
		     open_input/2,	 % +FileName, -InputStreams
		     close_input/1,	 % +InputStreams
		     open_output/2,	 % +FileName, -OutputStreams
		     close_output/1	 % +OutputStreams
		   ], [assertions]).

:- use_module(engine(internals)).
% :- use_module(engine(streams_basic), [stream/1]).

:- doc(title,"Structured stream handling").

:- pred open_input(FileName,InputStreams)
         : sourcename(FileName)
        => input_handler(InputStreams).

open_input(FileName, i(OldInput, NewInput)) :-
	current_input(OldInput),
	open(FileName, read, NewInput),
	set_input(NewInput).

:- pred close_input(InputStreams)
         : input_handler(InputStreams)
        => input_handler(InputStreams).

close_input(i(OldInput, NewInput)) :- !,
	set_input(OldInput),
	close(NewInput).
close_input(X) :-
	throw(error(domain_error(open_input_handler, X), close_input/1-1)).

:- pred open_output(FileName,OutputStreams)
         : sourcename(FileName)
        => output_handler(OutputStreams).

open_output(FileName, o(OldOutput, NewOutput)) :-
	current_output(OldOutput),
	open(FileName, write, NewOutput),
	set_output(NewOutput).

:- pred close_output(OutputStreams)
         : output_handler(OutputStreams)
        => output_handler(OutputStreams).

close_output(o(OldOutput, NewOutput)) :- !,
	set_output(OldOutput),
	close(NewOutput).
close_output(X) :-
	throw(error(domain_error(open_output_handler, X), close_output/1-1)).

:- prop input_handler/1 + regtype.

input_handler(i(Old,New)):-
	stream(Old),
	stream(New).

:- prop output_handler/1 + regtype.

output_handler(o(Old,New)):-
	stream(Old),
	stream(New).
