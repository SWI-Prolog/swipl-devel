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

:- module(http_xref, []).

:- dynamic
	prolog:called_by/2.
:- multifile
	prolog:called_by/2.

					% HTML-WRITE Library
prolog:called_by(html(L, _, _), Called) :-
	html_called_by(L, Called).
prolog:called_by(page(L1, L2, _, _), Called) :-
	html_called_by(L1, C1),
	html_called_by(L2, C2),
	append(C1, C2, Called).
prolog:called_by(page(L, _, _), Called) :-
	html_called_by(L, Called).
					% HTTPD
prolog:called_by(send(_, reply_html(_:Term, _)), Called) :-
	prolog:called_by(send(_, reply_html(_:Term)), Called).
prolog:called_by(send(_, reply_html(_:Term, _, _)), Called) :-
	prolog:called_by(send(_, reply_html(_:Term)), Called).
prolog:called_by(send(_, reply_html(_:Term)), [Called]) :-
	catch(Term =.. L, _, fail),
	append(L, [_,_], L2),
	Called =.. L2.

html_called_by(Term, Called) :-
	findall(C, html_called(Term, C), Called).

html_called(Term, Called) :-
	term_member(\Call, Term),
	catch(Call=..L, _, fail),
	append(L, [_,_], L2),
	Called =.. L2.

term_member(X, X).
term_member(X, T) :-
	compound(T),
	arg(_, T, A),
	term_member(X, A).
