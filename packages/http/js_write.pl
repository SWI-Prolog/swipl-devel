/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Michiel Hildebrand
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(javascript,
	  [ js_call//1,			% +Function(Arg..)
	    js_new//2,			% +Id, +Function(+Args)
	    js_args//1			% +Args
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library(error)).
:- use_module(library(debug)).

/** <module> Utilities for including javascript

This library is a supplement   to library(http/html_write) for producing
JavaScript fragments. Its main role is  to   be  able to call JavaScript
functions with valid  arguments  constructed   from  Prolog  data.  E.g.
suppose you want to call a  JavaScript   functions  to process a list of
names represented as Prolog atoms.  This  can   be  done  using the call
below, while without this library  you  would   have  to  be  careful to
properly escape special characters.

    ==
    numbers_script(Names) -->
	html(script(type('text/javascript'),
	     [ \js_call('ProcessNumbers'(Names)
	     ]),
    ==

The accepted arguments are described with js_args//1.
*/

%%	js_call(+Term)// is det.
%
%	Emit a call to a Javascript function.  The Prolog functor is the
%	name of the function. The arguments are converted from Prolog to
%	JavaScript using js_args//1. Please not that Prolog functors can
%	be quoted atom and thus the following is legal:
%
%	    ==
%	    	...
%	    	html(script(type('text/javascript'),
%	    	     [ \js_call('x.y.z'(hello, 42)
%	    	     ]),
%	    ==

js_call(Term) -->
	{ Term =.. [Function|Args] },
	html([Function, '(']),
	js_args(Args),
	html(');\n').


%%	js_new(+Id, +Term)// is det.
%
%	Emit a call to a Javascript object declaration. This is the same
%	as:
%
%	    ==
%	    ['var ', Id, ' = new ', \js_call(Term)]
%	    ==


js_new(Id, Term) -->
	{ Term =.. [Function|Args] },
	html(['var ', Id, ' = new ', Function, '(']),
	js_args(Args),
	html(');\n').

%%	js_args(+Args:list)// is det.
%
%	Write javascript function arguments. Each  argument is separated
%	by a comma. Elements  of  the   list  may  contain the following
%	terms:
%
%	    $ Variable :
%	    Emitted as Javascript =null=
%	    $ List :
%	    Produces a Javascript list, where each element is processed
%	    by this library.
%	    $ object(Attributes) :
%	    Where Attributes is a Key-Value list where each pair can be
%	    written as Key-Value, Key=Value or Key(Value), accomodating
%	    all common constructs for this used in Prolog.
%	    $ json(Term) :
%	    Emits a term using json_write/3.
%	    $ @(true), @(false), @(null) :
%	    Emits these constants without quotes.
%	    $ Number :
%	    Emited literally
%	    $ symbol(Atom) :
%	    Emitted without quotes.  Can be used for JavaScript symbols
%	    (e.i., function and variable-names)
%	    $ Atom or String :
%	    Emitted as quoted JavaScript string.

js_args([]) -->
	[].
js_args([H|T]) -->
	(   js_arg(H)
	->  (   { T == [] }
	    ->  []
	    ;   html(', '),
		js_args(T)
	    )
	;   { type_error(javascript_argument, H) }
	).

js_arg(H) -->
	{ var(H) }, !,
	[null].
js_arg(object(H)) -->
	{ is_list(H) }, !,
	html([ '{', \js_kv_list(H), '}' ]).
js_arg(@(true)) -->  [true].
js_arg(@(false)) --> [false].
js_arg(@(null)) -->  [null].
js_arg(symbol(H)) -->
	[H].
js_arg(json(Term)) -->
	{ json_to_string(json(Term), String),
	  debug(json_arg, '~w~n', String)
	},
	[ String ].
js_arg(H) -->
	{ is_list(H) }, !,
	html([ '[', \js_args(H), ']' ]).
js_arg(H) -->
	{ number(H) }, !,
	[H].
js_arg(H) -->
	{ atomic(H), !,
	  js_quoted_string(H, Q)
	},
	[ '\'', Q, '\''
	].

js_kv_list([]) --> [].
js_kv_list([H|T]) -->
	(   js_kv(H)
	->  (   { T == [] }
	    ->	[]
	    ;	html(', '),
		js_kv_list(T)
	    )
	;   { type_error(javascript_key_value, H) }
	).

js_kv(Key-Value) -->
	html(['\'',Key,'\':',\js_arg(Value)]).
js_kv(Key=Value) -->
	html(['\'',Key,'\':',\js_arg(Value)]).
js_kv(Term) -->
	{ compound(Term),
	  Term =.. [Key,Value]
	}, !,
	html(['\'',Key,'\':',\js_arg(Value)]).

%%	js_quoted_string(+Raw, -Quoted)
%
%	Quote text for use in JavaScript. Quoted does _not_ include the
%	leading and trailing quotes.
%
%	@tbd	Join with json stuff.

js_quoted_string(Raw, Quoted) :-
	atom_codes(Raw, Codes),
	phrase(js_quote_codes(Codes), QuotedCodes),
	atom_codes(Quoted, QuotedCodes).

js_quote_codes([]) -->
	[].
js_quote_codes([0'\r,0'\n|T]) --> !,
	"\\n",
	js_quote_codes(T).
js_quote_codes([H|T]) -->
	js_quote_code(H),
	js_quote_codes(T).

js_quote_code(0'') --> !,
	"\\'".
js_quote_code(34) --> !,
	[92,34].
js_quote_code(0'\\) --> !,
	"\\\\".
js_quote_code(0'\n) --> !,
	"\\n".
js_quote_code(0'\r) --> !,
	"\\r".
js_quote_code(0'\t) --> !,
	"\\t".
js_quote_code(C) -->
	[C].

%%	json_to_string(+JSONTerm, -String)
%
%	Write JSONTerm to String.

json_to_string(JSON, String) :-
	with_output_to(string(String),
		       json_write(current_output,JSON,[width(0)])).
