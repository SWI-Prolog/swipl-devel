/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce,
	  [ new/2, free/1

	  , send/2, send/3, send/4, send/5, send/6, send/7
	  , send/8, send/9, send/10, send/11, send/12

	  , get/3, get/4, get/5, get/6, get/7, get/8
	  , get/9, get/10, get/11, get/12, get/13

	  , object/1, object/2

	  , pce_open/3

	  , pce_global/2
	  , pce_autoload/2
	  , pce_autoload_all/0

	  , pce_predicate_reference/2
	  , pce_term_expansion/2
	  , pce_compiling/1
	  , pce_begin_recording/1	% +- source|documentation
	  , pce_end_recording/0

	  , pce_register_class/1
	  , pce_extended_class/1
	  , pce_prolog_class/1
	  , pce_prolog_class/2
	  , pce_bind_send_method/8
	  , pce_bind_get_method/9
	  , pce_send_method_message/2
	  , pce_get_method_message/2

	  , pce_catch_error/2

	  , require/1
	  ]).


:- meta_predicate
	send(+, :),
	send(+, :, +),
	send(+, :, +, +),
	send(+, :, +, +, +),
	send(+, :, +, +, +, +),
	send(+, :, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +),
	send(+, :, +, +, +, +, +, +, +, +, +, +),

	get(+, :, -),
	get(+, :, +, -),
	get(+, :, +, +, -),
	get(+, :, +, +, +, -),
	get(+, :, +, +, +, +, -),
	get(+, :, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, -),
	get(+, :, +, +, +, +, +, +, +, +, +, +, -),

	new(?, :),
	pce_global(+, :),
	require(:),

	pce_predicate_reference(:, ?),
	pce_extended_class(:),
	pce_register_class(:).

:- use_module(library(qp_pce)).
