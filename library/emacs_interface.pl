/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Quintus editor interface support
*/

:- module(emacs_interface,
	  [ '$editor_load_code'/2
	  , find_predicate1/2
	  , emacs_consult/1
	  , emacs_dabbrev_atom/1
	  , emacs_complete_atom/1
	  , emacs_previous_command/0
	  , emacs_next_command/0
	  , call_emacs/1
	  , call_emacs/2
	  ]).


		/********************************
		*              UTIL		*
		********************************/

running_under_emacs_interface :-
	emacs_tmp_file(_).

emacs_tmp_file(File) :-
	'$argv'(Argv),
	tmp_file(Argv, File).

tmp_file(['+C', Raw|_], File) :- !,
	concat('Emacs:', File, Raw).
tmp_file([_|T], File) :-
	tmp_file(T, File).


		/********************************
		*            SETUP		*
		********************************/

:- (   running_under_emacs_interface
   ->  '$set_prompt'('a%m%l%! ?- ')
   ;   true
   ).


		/********************************
		*           CONSULT		*
		********************************/

%	'$editor_load_code'(+Kind, +File)
%	Load code from EMACS.  `Kind' is {procedure,region,buffer}.  
%	`File' is the name of the file from which the code comes.  It
%	is an absolute filename.
%	
%	To be implemented.  There is a start for portions of a file
%	(region, procedure), but this is hard:  What is the starting
%	line of the region (for error-messages).  There is also a
%	problem with path-names: `File' is emacs notion of the absolute
%	filename.  SWI-Prologs notion may be different due to symbolic
%	links.  Finally: the region might be the entire file, in which
%	case we need to know about the module info ...

'$editor_load_code'(_buffer, File) :- !,
	format('Kind = ~w; File = ~w~n', [buffer, File]),
	emacs_tmp_file(TmpFile),
	concat('ls -l ', TmpFile, Cmd),
	shell(Cmd).
'$editor_load_code'(_Kind, File) :-
	trace,
	emacs_tmp_file(TmpFile),
	'$load_context_module'(File, Module),
	'$set_source_module'(OldModule, Module),
	'$start_consult'(File),
	'$style_check'(OldStyle, OldStyle),
	seeing(Old), see(TmpFile),
	repeat,
	    '$read_clause'(Clause),
	    '$consult_clause'(Clause, File), !,
	seen, see(Old),
	'$style_check'(_, OldStyle),
	'$set_source_module'(_, OldModule).


		/********************************
		*    TELL EMACS ABOUT ERRORS	*
		********************************/

%	Redefine [] to clear the compilation-buffer first

:- (   running_under_emacs_interface
   ->  user:abolish('.', 2),
       user:abolish(make, 0),
       user:(module_transparent '.'/2),
       user:assert(([H|T] :- emacs_consult([H|T]))),
       user:assert((make :- emacs_interface:make)),
       user:assert(exception(A,B,C) :- emacs_interface:exception(A,B,C))
   ;   true
   ).


:- dynamic
	compilation_base_dir/1.

:- module_transparent
	emacs_consult/1.

emacs_consult(Files) :-
	emacs_start_compilation,
	consult(Files),
	emacs_finish_compilation.


make :-
	emacs_start_compilation,
	system:make,
	emacs_finish_compilation.
	

exception(warning, warning(Path, Line, Message), _) :-
	emacs_warning_file(Path, File),
	call_emacs('(prolog-compilation-warning "~w" "~d" "~w")',
		   [File, Line, Message]),
	fail.					  % give normal message too


emacs_start_compilation :-
	absolute_file_name('', Pwd),
	asserta(compilation_base_dir(Pwd)),
	call_emacs('(prolog-compilation-start "~w")', [Pwd]).

	
emacs_finish_compilation :-
	retractall(emacs_compilation_base_dir(_)),
	call_emacs('(prolog-compilation-finish)').


emacs_warning_file(user, _) :- !,
	fail.					  % donot give warnings here
emacs_warning_file(Path, File) :-
	compilation_base_dir(Cwd),
	concat(Cwd, File, Path), !.
emacs_warning_file(Path, Path).
	


		/********************************
		*         FIND PREDICATE	*
		********************************/

%	find_predicate1(Name, Arity)
%

find_predicate1(Name, Arity) :-
	find_predicate(Name, Arity, Preds),
	(   Preds == []
	->  call_emacs('(@find "undefined" "nodebug")')
	;   forall(member(Head, Preds),
		   (source_file(Head, File),
		    call_emacs('(@fd-in "\"~w\" ~w ~w")', [Name, Arity, File])
		   ))
	->  call_emacs('(@find "ok" "nodebug")')
	;   call_emacs('(@find "none" "nodebug")')
	).
	

find_predicate(Name, Arity, Preds) :-
	(   integer(Arity)
	->  functor(Head, Name, Arity)
	;   true
	),
	findall(Pred, find_predicate_(Head, Pred), Preds).

find_predicate_(Head, Module:Head) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, imported_from(_)).
	

		/********************************
		*          ATOM DABREV		*
		********************************/

emacs_dabbrev_atom(Sofar) :-
	'$complete_atom'(Sofar, Extended, Unique), !,
	map_unique_to_lisp(Unique, LispBool),
	call_emacs('(prolog-complete-atom-with "~s" ~w)',
		   [Extended, LispBool]).
emacs_dabbrev_atom(Sofar) :-
	call_emacs('(prolog-completion-error-message (concat "No completions for: " "~s"))', [Sofar]).

map_unique_to_lisp(unique, t).
map_unique_to_lisp(not_unique, nil).


		/********************************
		*         ATOM COMPLETION	*
		********************************/

emacs_complete_atom(Sofar) :-
	'$atom_completions'(Sofar, List), List \== [], !,
	call_emacs('(prolog-completions-start-collect)'),
	emacs_transfer_completions(List, 1),
	call_emacs('(prolog-completions-run "~s")', [Sofar]).
emacs_complete_atom(Sofar) :-
	call_emacs('(prolog-completion-error-message (concat "No completions for: " "~s"))', [Sofar]).

emacs_transfer_completions([], _).
emacs_transfer_completions([Atom|T], N) :-
	call_emacs('(prolog-transfer-completion "~w" ~d)', [Atom, N]),
	NN is N + 1,
	emacs_transfer_completions(T, NN).


		/********************************
		*             HISTORY		*
		********************************/

emacs_insert_command(Nr) :-
	recorded('$history_list', Nr/Command), !,
	flag(emacs_shown_command, _, Nr),
	call_emacs('(prolog-insert-history-command "~w")', Command).
emacs_insert_command(_) :-
	call_emacs('(prolog-completion-error-message "No more commands")').

emacs_previous_command :-
	flag('$last_event', Last, Last),
	(   flag(emacs_last_command, Last, Last)
	->  flag(emacs_shown_command, Shown, Shown),
	    This is Shown - 1,
	    emacs_insert_command(This)
	;   flag(emacs_last_command, _, Last),
	    emacs_insert_command(Last)
	).
	    

emacs_next_command :-
	flag('$last_event', Last, Last),
	(   flag(emacs_last_command, Last, Last)
	->  flag(emacs_shown_command, Shown, Shown),
	    This is Shown + 1,
	    emacs_insert_command(This)
	;   flag(emacs_last_command, _, Last),
	    emacs_insert_command(Last)
	).


		/********************************
		*           CALL EMACS		*
		********************************/

call_emacs(Fmt) :-
	call_emacs(Fmt, []).
call_emacs(Fmt, Args) :-
	concat_atom(['', Fmt, ''], F1),
	format(F1, Args),
	flush.

