/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE public predicates
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(qp_pce,
	  [ new/2, free/1

	  , send/2, send/3, send/4, send/5, send/6, send/7
	  , send/8, send/9, send/10, send/11, send/12

	  , get/3, get/4, get/5, get/6, get/7, get/8
	  , get/9, get/10, get/11, get/12, get/13

	  , object/1, object/2

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
	  , pce_bind_send_method/8
	  , pce_bind_get_method/9
	  , pce_send_method_message/2
	  , pce_get_method_message/2

	  , pce_catch_error/2

	  , require/1
	  , auto_call/1
	  ]).


pce_ifhostproperty(repeat_meta_declaraction,
(:- meta_predicate
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

	pce_predicate_reference(:, ?),
	pce_extended_class(:),
	pce_register_class(:))).


		/********************************
		*          PROLOG PART		*
		********************************/

:- use_module(pce_principal).
:- use_module(pce_error).
:- use_module(pce_autoload).
:- use_module(pce_global).
:- use_module(pce_editor).
:- use_module(pce_expansion).
:- use_module(pce_realise).
:- use_module(require).
