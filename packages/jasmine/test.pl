/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- use_module(jasmine).

:- dynamic
	jasmine/1.

open :-
	odb_ses_start(H, 'einstein::jasmine/jasmine', _, _, _),
	asserta(jasmine(H)),
	exec('Transaction.start();').	% ensure a transaction

close :-
	retract(jasmine(H)), !,
	odb_ses_end(H).

exec(Cmd) :-
	jasmine(H),
	odb_exec_odql(H, Cmd).

get_var(Name, Value) :-
	ensure_transaction,
	jasmine(H),
	odb_get_var(H, Name, Value).

set_var(Name, Value) :-
	ensure_transaction,
	jasmine(H),
	odb_set_var(H, Name, Value).

ensure_transaction :-
	exec('if (Transaction.isWithinTransaction() != TRUE) { Transaction.start(); };').

		 /*******************************
		 *     CLASSES AND FAMILIES	*
		 *******************************/

%	Get all Jasmine class families

families(List) :-
	jasmine(SH),
	ensure_transaction,
	odql(SH,
	     [ ss:'Bag<String>',
	       pcount:'Integer'
	     ],
	     [ 'ss = FamilyManager.getAllFamilies();',
	       'pcount = ss.count();',
	       get(pcount, C),
	       { format('Found ~w families~n', [C])
	       },
	       get_list(ss, List)
	     ]).

family_classes(Family, Classes) :-
	jasmine(SH),
	ensure_transaction,
	odql(SH, 
	     [ cBag:'Bag<Composite class>',
	       cc:'Composite class',
	       cname:'String'
	     ],
	     [ 'defaultCF ~w;'-[Family],
	       'cBag = FamilyManager.getAllClasses("~w");'-[Family],
	       { odb_collection_to_list(SH, cBag, ClassObjects),
		 maplist(object_name(SH), ClassObjects, Classes)
	       }
	     ]).

object_name(SH, Class, Name) :-
	odql(SH, [],
	     [ set(cc, Class),
	       'cname = cc.getClassName();',
	       get(cname, Name)
	     ]).
	
:- style_check(-atom).

class_properties(Class, Properties) :-
	jasmine(SH),
	odql(SH,
	     [ 'PhysBagTup':'Bag<T1[Integer propType,
				    String name,
				    String CFName,
				    String className,
				    Boolean isClass,
				    Boolean isSet,
				    String propDescription,
				    Integer precision,
				    Integer scale,
				    Boolean isMandatory,
				    Boolean isUnique]>'
	     ],
	     [ 'PhysBagTup = (T1)~w.getPropInfo(TRUE);'-[Class],
	       get_list('PhysBagTup', Properties)
	     ]).
