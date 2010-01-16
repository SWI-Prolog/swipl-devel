/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University, Amsterdam

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


:- module(snowball,
	  [ snowball/3,			 % +Algorithm, +In, -Out
	    snowball_current_algorithm/1 % ?algorithm
	  ]).

/** <module> The Snowball multi-lingual stemmer library

This module encapsulates "The C version  of the libstemmer library" from
the Snowball project. This library  provides   stemmers  in a variety of
languages.  The interface to this library is very simple:

    * snowball/3 stems a word with a given algorithm
    * snowball_current_algorithm/1 enumerates the provided algorithms.

Here is an example:

    ==
    ?- snowball(english, walking, S).
    S = walk.
    ==

@see http://snowball.tartarus.org/
*/

:- use_foreign_library(foreign(snowball)).

%%	snowball(+Algorithm, +Input, -Stem) is det.
%
%	Apply the Snowball Algorithm on Input and unify the result
%	(an atom) with Stem.
%
%	The implementation maintains a cache of stemmers for each thread
%	that  accesses  snowball/3,   providing    high-perfomance   and
%	thread-safety without locking.
%
%	@param	Algorithm is the (english) name for desired algorithm
%		or an 2 or 3 letter ISO 639 language code.
%	@param	Input is the word to be stemmed.  It is either an
%		atom, string or list of chars/codes.  The library
%		accepts Unicode characters.  Input must be
%		_lowercase_.  See downcase_atom/2.
%	@error domain_error(snowball_algorithm, Algorithm)
%	@error type_error(atom, Algorithm)
%	@error type_error(text, Input)

%%	snowball_current_algorithm(?Algorithm) is nondet.
%
%	True if Algorithm is the official  name of an algorithm suported
%	by snowball/3. The predicate is =semidet= if Algorithm is given.

term_expansion(snowball_current_algorithm(dummy), Clauses) :-
	snowball_algorithms(Algos),
	maplist(wrap, Algos, Clauses).

wrap(X, snowball_current_algorithm(X)).

snowball_current_algorithm(dummy).
