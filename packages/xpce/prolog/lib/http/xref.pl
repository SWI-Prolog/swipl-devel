/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(http_xref, []).

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
