/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_class_template,
	  [ use_class_template/1,
	    use_class_template/2
	  ]).
:- use_module(library(pce)).
:- require([ pce_error/1
	   ]).

:- pce_begin_class(template, object, "use_class_template/1 super-class").
:- pce_end_class.

%	use_class_template(+TemplateClassName)
%
%	Handled by the XPCE class compiler.  This version just prints
%	an error message.

use_class_template(Template) :-
	pce_error(context_error(use_class_template(Template),
				nodirective,
				goal)).
use_class_template(Class, Template) :-
	pce_error(context_error(use_class_template(Class, Template),
				nodirective,
				goal)).
