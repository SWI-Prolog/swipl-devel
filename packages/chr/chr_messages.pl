/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Jan Wielemaker and Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

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

:- module(chr_messages,
	  [ chr_message/3		% +CHR Message, Out, Rest
	  ]).
:- use_module(chr(chr_runtime)).

:- discontiguous
	chr_message/3.

%	compiler messages

chr_message(compilation_failed(From)) -->
	[ 'CHR Failed to compile ~w'-[From] ].

%	debug messages

chr_message(prompt) -->
	[ at_same_line, ' ? ', flush ].
chr_message(command(Command)) -->
	[ at_same_line, '[~w]'-[Command] ].
chr_message(invalid_command) -->
	[ nl, 'CHR: Not a valid debug option.  Use ? for help.' ].
chr_message(debug_options) -->
	{ bagof(Ls-Cmd,
		bagof(L, 'chr debug command'(L, Cmd), Ls),
		Lines)
	},
	[ 'CHR Debugger commands:', nl, nl ],
	debug_commands(Lines),
	[ nl ].

debug_commands([]) -->
	[].
debug_commands([Ls-Cmd|T]) -->
	[ '\t' ], chars(Ls), [ '~t~28|~w'-[Cmd], nl ],
	debug_commands(T).
	
chars([C]) --> !,
	char(C).
chars([C|T]) -->
	char(C), [', '],
	chars(T).

char(' ') --> !, ['<space>'].
char('\r') --> !, ['<cr>'].
char(end_of_file) --> !, ['EOF'].
char(C) --> [C].


chr_message(ancestors(History, Depth)) -->
	[ 'CHR Ancestors:', nl ],
	ancestors(History, Depth).

ancestors([], _) -->
	[].
ancestors([Event|Events], Depth) -->
	[ '\t' ], event(Event, Depth), [ nl ],
	{ NDepth is Depth - 1
	},
	ancestors(Events, NDepth).


%	debugging ports

chr_message(event(Port, Depth)) -->
	[ 'CHR: ' ],
	event(Port, Depth),
	[ flush ].			% do not emit a newline

event(Port, Depth) -->
	depth(Depth),
	port(Port).
event(apply(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Apply: ' ],
	rule(H1,H2,G,B).
event(try(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Try: ' ],
	rule(H1,H2,G,B).
event(insert(#(_,Susp)), Depth) -->
	depth(Depth),
	[ 'Insert: ' ],
	head(Susp).

port(call(Susp)) -->
	[ 'Call: ' ],
	head(Susp).
port(wake(Susp)) -->
	[ 'Wake: ' ],
	head(Susp).
port(exit(Susp)) -->
	[ 'Exit: ' ],
	head(Susp).
port(fail(Susp)) -->
	[ 'Fail: ' ],
	head(Susp).
port(redo(Susp)) -->
	[ 'Redo: ' ],
	head(Susp).
port(remove(Susp)) -->
	[ 'Remove: ' ],
	head(Susp).


depth(Depth) -->
	[ '~t(~D)~10| '-[Depth] ].

head(Susp) -->
	{ Susp =.. [_,ID,_,_,_,_,Goal|_Args]
	},
	[ '~w # <~w>'-[Goal, ID] ].

heads([H]) --> !,
	head(H).
heads([H|T]) -->
	head(H),
	[ ', ' ],
	heads(T).


%	rule(H1, H2, G, B)
%	
%	Produce text for the CHR rule "H1 \ H2 [<=]=> G | B"

rule(H1, H2, G, B) -->
	rule_head(H1, H2),
	rule_body(G, B).

rule_head([], H2) --> !,
	heads(H2),
	[ ' ==> ' ].
rule_head(H1, []) --> !,
	heads(H1),
	[ ' <=> ' ].
rule_head(H1, H2) -->
	heads(H1), [ ' \\ ' ], heads(H2).


rule_body(true, B) --> !,
	[ '~w.'-[B] ].
rule_body(G, B) -->
	[ '~w | ~w.'-[G, B] ].
