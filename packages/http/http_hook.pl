/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(http_hook,
	  []).

/** <module> HTTP library hooks

Get the declarations of the HTTP package using

    ==
    :- use_module(library(http/http_hook)).
    ==

@tbd	This should be using include, but then it cannot be a module
	and this would cause more overhead in SWI-Prolog
@tbd	Complete this and document the hooks.
*/

		 /*******************************
		 *	     HTTP-PATH		*
		 *******************************/

:- multifile http:location/3.
:- dynamic   http:location/3.


		 /*******************************
		 *	     HTML-WRITE		*
		 *******************************/

:- multifile
	html_write:expand//1,
	html_write:expand_attribute_value//1,
	html_write:html_head_expansion/2,
	html_write:layout/3.


		 /*******************************
		 *	   HTTP-DISPATCH	*
		 *******************************/

:- multifile
	http:authenticate/3.


		 /*******************************
		 *	 HTTP-PARAMETERS	*
		 *******************************/

:- multifile
	http:convert_parameter/3.

%%	http:convert_parameter(+Type, +ValueIn, -ValueOut) is semidet.
%
%	Hook to execute a step in the HTTP parameter conversion process.
%
%	@see http_parameters:check_type3/3.
