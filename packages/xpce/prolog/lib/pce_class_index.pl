/*  File:    pce_index.pl
    Author:  Jan Wielemaker
    Created: Mar 14 2003
    Purpose: 
*/

:- module(pce_class_index,
	  [ pce_make_library_index/1,	% +Dir
	    pce_update_library_index/0
	  ]).
:- use_module(library('pce_prolog_xref')).
:- use_module(library(lists)).

index_file('CLASSINDEX.pl').

%	pce_make_library_index(+Dir)
%	
%	Create a file CLASSINDEX.pl in Dir holding facts of the format
%	
%		class(Name, Super, Summary, File)
%	
%	This file can be used for auto-loading as well as supporting
%	cross-referencing and syntax-highlighting.

pce_make_library_index(Dir) :-
	absolute_file_name(Dir,
			   [ file_type(directory),
			     access(exist)
			   ],
			   Path),
	working_directory(Old, Path),
	call_cleanup(make_library_index,
		     working_directory(_, Old)).

%	pce_update_library_index/0
%	
%	Update out-of-date CLASSINDEX.pl files in defined library
%	directories.

pce_update_library_index :-
	index_file(File),
	(   absolute_file_name(library(File),
			       [ access(read),
				 access(write),
				 solutions(all),
				 file_errors(fail)
			       ], 
			       File),
	    file_directory_name(File, Dir),
	    pce_update_library_index(Dir),
	    fail
	;   true
	).

pce_update_library_index(Dir) :-
	working_directory(Old, Dir),
	call_cleanup(update_library_index,
		     working_directory(_, Old)).

update_library_index :-
	expand_file_name('*.pl', Files),
	index_file(Index),
	(   library_index_out_of_date(Index, Files),
	    catch(open(Index, write, Out), _, fail)
	->  header(Out),
	    call_cleanup(index_file_list(Files, Out),
			 close(Out))
	;   true
	).

make_library_index :-
	expand_file_name('*.pl', Files),
	index_file(Index),
	open(Index, write, Out),
	header(Out),
	call_cleanup(index_file_list(Files, Out),
		     close(Out)).

index_file_list([], _).
index_file_list([H|T], Out) :-
	index_file(H, Out),
	index_file_list(T, Out).

index_file('INDEX.pl', _) :- !.
index_file('MKINDEX.pl', _) :- !.
index_file('CLASSINDEX.pl', _) :- !.
index_file(File, Out) :-
	xref_source(File),
	(   xref_defined_class(File, Class, local(_Line, Super, Summary)),
	    format(Out,
		   'class(~q, ~q, ~q, ~q).~n',
		   [Class, Super, Summary, File]),
	    fail
	;   true
	),
	xref_clean(File).
	
header(Out) :-
	format(Out,
	       '/*  $Id\
	       $\n\n    \
	       Creator: pce_make_library_index/1\n    \
	       Purpose: Provide index of XPCE classes in directory\n\
	       */\n\n',
	       []).

library_index_out_of_date(Index, _Files) :-
	\+ exists_file(Index), !.
library_index_out_of_date(Index, Files) :-
	time_file(Index, IndexTime),
	(   time_file('.', DotTime),
	    DotTime @> IndexTime
	;   member(File, Files),
	    time_file(File, FileTime),
	    FileTime @> IndexTime
	), !.
