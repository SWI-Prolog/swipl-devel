/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module($rc,
	  [ open_resource/3,		% +Name, ?Class, -Stream
	    open_resource/4,		% +Name, ?Class, +RW, -Stream
	    current_resource/3		% :Name, ?Class, ?File
	  ]).

:- dynamic
	user:resource/3.
:- multifile
	user:resource/3.
	
:- module_transparent
	open_resource/3,
	open_resource/4.

%	open_resource(:Name, ?Class, -Handle)

open_resource(Name, Class, Handle) :-
	open_resource(Name, Class, read, Handle).

open_resource(Name, Class, RW, Handle) :-
	$strip_module(Name, Module, RcName),
	(   catch(Module:resource(RcName, Class, FileSpec),
		  error(existence_error(procedure, Module:resource/3), _),
		  user:resource(RcName, Class, FileSpec))
	->  absolute_file_name(FileSpec, File),
	    open(File, RW, Handle, [type(binary)])
	;   $rc_handle(RC),
	    tag_rc_name(Module, RcName, TaggedName),
	    $rc_open(RC, TaggedName, Class, RW, Handle)
	).

tag_rc_name(user, RcName, RcName) :- !.
tag_rc_name(Module, RcName, TaggedName) :-
	concat_atom([Module, ':', RcName], TaggedName).
tag_rc_name(_, RcName, RcName).

%	current_resource(Name, Class, File)
%
%	List all currently defined resources.  Should eventually deal with
%	resources that are already part of the state.

:- module_transparent
	current_resource/3.

current_resource(Name, Class, File) :-
	(   atom(Name)
	;   var(Name)
	), !,
	catch(resource(Name, Class, File), _, fail).
current_resource(M:Name, Class, File) :-
	current_module(M),
	catch(M:resource(Name, Class, File), _, fail).
