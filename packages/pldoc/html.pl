/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pldoc_html,
	  [ doc_write_html/2		% +Stream, +Term
	  ]).
:- use_module(library('http/html_write')).

/** ---+ PlDoc HTML backend

This module translates the Herbrand term from the documentation
extractiong <module wiki.pl> into HTML+CSS.
*/

doc_write_html(Out, Doc) :-
	phrase(html(Doc), Tokens),
	print_html(Out, Tokens).

%% ---++ Rendering rules
%
% These rules translate \-terms produced by the <module wiki.pl>

tags(Tags) -->
	html(dl(Tags)).

tag(Name, Value) -->
	html([dt(Name), dd(Value)]).

params(Params) -->
	html(dl(Params)).

param(Name, Value) -->
	html([dt(Name), dd(Value)]).
