/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

:- module(quasi_quotations,
	  [ with_quasi_quotation_input/3,	% +Content, -Stream, :Goal
	    phrase_from_quasi_quotation/2,	% :Grammar, +Content
	    quasi_quotation_syntax_error/1,	% +Error
	    quasi_quotation_syntax/1		% :Type
	  ]).
:- use_module(library(error)).
:- use_module(library(pure_input)).

/** <module> Define Quasi Quotation syntax

Inspired                                                              by
[Haskell](http://www.haskell.org/haskellwiki/Quasiquotation), SWI-Prolog
support _quasi quotation_. Quasi quotation   allows for embedding (long)
strings using the syntax of an external   language  (e.g., HTML, SQL) in
Prolog text and syntax-aware  embedding  of   Prolog  variables  in this
syntax. At the same time,  quasi   quotation  provides an alternative to
represent long strings and atoms in Prolog.

The basic form of a quasi quotation is   inspired  by XML and defined as
below. Here, `Type` is an arbitrary Prolog   term that must parse into a
_callable_ (atom or  compound)  term  and   Quotation  is  an  arbitrary
sequence of characters, not including  the   sequence  =|]]>|=.  If this
sequence needs to be embedded, it must be escaped according to the rules
of the target language.

    ==
    <![Type][Quotation]]>
    ==

While reading a Prolog term, and if   the  Prolog flag =quasi_quotes= is
set to =true= (which is the case if  this library is loaded), the parser
collects quasi quotations. After reading the final full stop, the parser
makes the call below. Here, `TypeName`  is   the  functor name of `Type`
above and `TypeArgs` is a list holding   the  arguments, i.e., `Type =..
[TypeName|TypeArgs]`. Splitting the type into its  name and arguments is
done to make the quasi quotation parser   a  predicate with a consistent
arity 4, regardless of the number of additional arguments.

    ==
    call(+TypeName, +Content, +TypeArgs, +VariableNames, -Result)
    ==

The arguments are defined as

  - `TypeName` is the principal functor of the quasi quotation type.
  This must be declared using quasi_quote_type/1 and there must be a
  predicate TypeName/4.

  - `Content` is an opaque term that carries the content of the quasi
  quoted material and position information about the source code. It is
  passed to with_quasi_quote_input/3.

  - `TypeArgs` carries the additional arguments of the `Type`. These are
  commonly used to make the parameter passing between the clause and the
  quasi quotation explicit. For example:

    ==
	...,
	<![html(Name, Address)
	   [ <tr><td>##Name<td>##Address</tr>
	   ]]>
    ==

  - `VariableNames` is the complete variable dictionary of the clause as
  it is made available throug read_term/3 with the option
  =variable_names=. It is a list of terms `Name = Var`.

  - `Result` is a variable that must be unified to resulting term.
  Typically, this term is structured Prolog tree that carries a
  (partial) representation of the abstract syntax tree with embedded
  variables that pass the Prolog parameters. This term is normally
  either passed to a predicate that serializes the abstract syntax tree,
  or a predicate that processes the result in Prolog. For example, HTML
  is commonly embedded for writing HTML documents (see
  library(http/html_write)). Examples of languages that may be embedded
  for processing in Prolog are SPARQL, RuleML or regular expressions.

Below is a simple but functional example that defines the =html= type.

  ==
  :- module(qq_html,
	    [ html/4
	    ]).
  :- use_module(library(sgml)).
  :- use_module(library(apply)).
  :- use_module(library(lists)).
  :- use_module(library(quasi_quotations)).

  :- quasi_quotation_syntax(html).

  html(Content, Vars, Dict, DOM) :-
	  include(qq_var(Vars), Dict, QQDict),
	  with_quasi_quotation_input(
	      Content, In,
	      load_html(In, DOM0,
			[ max_errors(0)
			])),
	  dom_vars(QQDict, DOM0, DOM).

  qq_var(Vars, _=Var) :-
	  member(V, Vars),
	  V == Var, !.

  dom_vars(Dict, Ref, Var) :-
	  atom(Ref),
	  atom_concat('##', VarName, Ref),
	  memberchk(VarName=Var, Dict), !.
  dom_vars(Dict, DOM0, DOM) :-
	  compound(DOM0), !,
	  DOM0 =.. [Name|Args0],
	  maplist(dom_vars(Dict), Args0, Args),
	  DOM =.. [Name|Args].
  dom_vars(_, DOM, DOM).
  ==

@author Jan Wielemaker.  Introduction of Quasi Quotation was suggested
	by Michael Hendricks.
@see    [Why it's nice to be quoted: quasiquoting for
	haskell](http://www.eecs.harvard.edu/~mainland/ghc-quasiquoting/mainland07quasiquoting.pdf)
*/


:- meta_predicate
	with_quasi_quotation_input(+, -, 0),
	quasi_quotation_syntax(4),
	phrase_from_quasi_quotation(//, +).

:- set_prolog_flag(quasi_quotations, true).

%%	with_quasi_quotation_input(+Content, -Stream, :Goal) is det.
%
%	Process the quasi-quoted Content using   Stream  parsed by Goal.
%	Stream is a temporary stream with the following properties:
%
%	    - Its initial _position_ represents the position of the
%	      start of the quoted material.
%	    - It is a text stream, using =utf8= _encoding_.
%	    - It allows for repositioning
%	    - It will be closed after Goal completes.
%
%	@arg Goal is executed as once(Goal).  Goal must succeed.
%	     Failure or exceptions from Goal are interpreted as
%	     syntax errors.
%	@see phrase_from_quasi_quotation/2 can be used to process a
%	     quatation using a grammar.

with_quasi_quotation_input(Content, Stream, Goal) :-
	functor(Content, '$quasi_quotation', 3), !,
	setup_call_cleanup(
	    '$qq_open'(Content, Stream),
	    (	call(Goal)
	    ->	true
	    ;	quasi_quotation_syntax_error(
		    quasi_quotation_parser_failed,
		    Stream)
	    ),
	    close(Stream)).

%%	phrase_from_quasi_quotation(:Grammar, +Content) is det.
%
%	Process the quasi quotation using the   DCG  Grammar. Failure of
%	the grammer is interpreted as a syntax error.
%
%	@see	with_quasi_quotation_input/3 for processing quotations from
%		stream.

phrase_from_quasi_quotation(Grammar, Content) :-
	functor(Content, '$quasi_quotation', 3), !,
	setup_call_cleanup(
	    '$qq_open'(Content, Stream),
	    phrase_quasi_quotation(Grammar, Stream),
	    close(Stream)).

phrase_quasi_quotation(Grammar, Stream) :-
	set_stream(Stream, buffer_size(512)),
	stream_to_lazy_list(Stream, List),
	phrase(Grammar, List), !.
phrase_quasi_quotation(_, Stream) :-
	quasi_quotation_syntax_error(
	    quasi_quotation_parser_failed,
	    Stream).

%%	quasi_quotation_syntax(:TypeName) is det.
%
%	Declare the predicate TypeName/4  to   implement  the  the quasi
%	quote type TypeName.  Normally used as a directive.

quasi_quotation_syntax(M:Type) :-
	must_be(atom, Type),
	'$set_predicate_attribute'(M:Type/4, quasi_quotation_syntax, 1).

%%	quasi_quotation_syntax_error(+Error)
%
%	Report syntax_error(Error) using the  current   location  in the
%	quasi quoted input parser.
%
%	@throws error(syntax_error(Error), Position)

quasi_quotation_syntax_error(Error) :-
	quasi_quotation_input(Stream),
	quasi_quotation_syntax_error(Error, Stream).

quasi_quotation_syntax_error(Error, Stream) :-
	stream_syntax_error_context(Stream, Context),
	throw(error(syntax_error(Error), Context)).

quasi_quotation_input(Stream) :-
	'$input_context'(Stack),
	memberchk(input(quasi_quoted, _File, _Line, StreamVar), Stack),
	Stream = StreamVar.


%%	stream_syntax_error_context(+Stream, -Position) is det.
%
%	Provide syntax error  location  for   the  current  position  of
%	Stream.

stream_syntax_error_context(Stream, file(File, LineNo, LinePos, CharNo)) :-
	stream_property(Stream, file_name(File)),
	position_context(Stream, LineNo, LinePos, CharNo), !.
stream_syntax_error_context(Stream, stream(Stream, LineNo, LinePos, CharNo)) :-
	position_context(Stream, LineNo, LinePos, CharNo), !.
stream_syntax_error_context(_, _).

position_context(Stream, LineNo, LinePos, CharNo) :-
	stream_property(Stream, position(Pos)), !,
	stream_position_data(line_count,    Pos, LineNo),
	stream_position_data(line_position, Pos, LinePos),
	stream_position_data(char_count,    Pos, CharNo).


		 /*******************************
		 *	   SYSTEM HOOK		*
		 *******************************/

%	system:'$parse_quasi_quotations'(+Quotations:list, +Module) is
%	det.
%
%	@arg	Quotations is a list of terms
%
%		    quasi_quotation(Type, Quotation, VarNames, Result)

:- public
	system:'$parse_quasi_quotes'/2.

system:'$parse_quasi_quotations'([], _).
system:'$parse_quasi_quotations'([H|T], M) :-
	qq_call(H, M),
	system:'$parse_quasi_quotations'(T, M).

qq_call(quasi_quotation(Type, Content, VariableNames, Result), M) :-
	Type =.. [TypeName|TypeArgs],
	setup_call_cleanup(
	    '$push_input_context'(quasi_quoted),
	    call(M:TypeName, Content, TypeArgs, VariableNames, Result),
	    '$pop_input_context'), !.
qq_call(quasi_quotation(_Type, Content, _VariableNames, _Result), _M) :-
	setup_call_cleanup(
	    '$push_input_context'(quasi_quoted),
	    with_quasi_quotation_input(
		Content, Stream,
		quasi_quotation_syntax_error(quasi_quote_parser_failed, Stream)),
	    '$pop_input_context'), !.


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:error_message//1.

prolog:error_message(syntax_error(unknown_quasi_quotation_syntax(Syntax, M))) -->
	{ functor(Syntax, Name, _) },
	[ 'Quasi quotation syntax ~q:~q is not defined'-[M, Name] ].
prolog:error_message(syntax_error(invalid_quasi_quotation_syntax(Syntax))) -->
	[ 'Quasi quotation syntax must be a callable term.  Found ~q'-[Syntax] ].
