/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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
