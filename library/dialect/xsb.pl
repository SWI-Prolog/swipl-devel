/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
			 CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(xsb,
          [ add_lib_dir/1,			% +Directories
	    add_lib_dir/2,			% +Root, +Directories

            compile/2,                          % +File, +Options

            xsb_import/2,                       % +Preds, From

            op(1050,  fy, import),
            op(1040, xfx, from),
            op(1100,  fy, index)
          ]).
:- use_module(library(error)).
:- use_module(library(dialect/xsb/source)).

/** <module> XSB Prolog compatibility layer

This  module  provides  partial  compatibility   with  the  [XSB  Prolog
system](http://xsb.sourceforge.net/)
*/

:- meta_predicate
    xsb_import(:, +),
    compile(:).

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_xsb_library
%
%	Pushes searching for  dialect/xsb  in   front  of  every library
%	directory that contains such as sub-directory.

push_xsb_library :-
    (   absolute_file_name(library(dialect/xsb), Dir,
			   [ file_type(directory),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ]),
	asserta((user:file_search_path(library, Dir) :-
		prolog_load_context(dialect, xsb))),
	fail
    ;   true
    ).

:- push_xsb_library.


% Register XSB specific term-expansion to rename conflicting directives.

user:term_expansion(In, Out) :-
    prolog_load_context(dialect, xsb),
    xsb_term_expansion(In, Out).

xsb_term_expansion((:- import Preds from From),
                   (:- xsb_import(Preds, From))).
xsb_term_expansion((:- index(_PI, _How)), []).
xsb_term_expansion((:- index(_PI)), []).

%!  xsb_import(:Predicates, +From)
%
%   Make Predicates visible in From. As the XSB library structructure is
%   rather different from SWI-Prolog's, this is a heuristic process.

xsb_import(Into:Preds, From) :-
    xsb_import(Preds, Into, From).

xsb_import(Var, _Into, _From) :-
    var(Var),
    !,
    instantiation_error(Var).
xsb_import((A,B), Into, From) :-
    !,
    xsb_import(A, Into, From),
    xsb_import(B, Into, From).
xsb_import(Name/Arity, Into, _From) :-
    functor(Head, Name, Arity),
    predicate_property(Into:Head, visible).
xsb_import(Name/Arity, Into, From) :-
    current_predicate(From:Name/Arity),
    !,
    @(import(From:Name/Arity), Into).
xsb_import(Name/Arity, Into, From) :-
    absolute_file_name(library(From), Path,
                       [ extensions(['P', pl, prolog]),
                         access(read)
                       ]),
    use_module(Into:Path, [Name/Arity]).


		 /*******************************
		 *      BUILT-IN PREDICATES	*
		 *******************************/

%!  add_lib_dir(+Directories) is det.
%!  add_lib_dir(+Root, +Directories) is det.
%
%   Add    members    of    the    comma      list     Directories    to
%   user:library_directory/1.  If  Root  is  given,    all   members  of
%   Directories are interpreted relative to Root.

add_lib_dir(Directories) :-
    add_lib_dir('.', Directories).

add_lib_dir(_, Var) :-
    var(Var),
    !,
    instantiation_error(Var).
add_lib_dir(Root, (A,B)) :-
    !,
    add_lib_dir(Root, A),
    add_lib_dir(Root, B).
add_lib_dir(Root, a(Dir)) :-
    !,
    add_to_library_directory(Root, Dir, asserta).
add_lib_dir(Root, Dir) :-
    add_to_library_directory(Root, Dir, assertz).

add_to_library_directory(Root, Dir, How) :-
    (   expand_file_name(Dir, [Dir1])
    ->  true
    ;   Dir1 = Dir
    ),
    relative_file_name(TheDir, Root, Dir1),
    exists_directory(TheDir),
    !,
    (   user:library_directory(TheDir)
    ->  true
    ;   call(How, user:library_directory(TheDir))
    ).
add_to_library_directory(_, _, _).

%!  compile(File, Options)
%
%   The XSB version compiles a file into .xwam without loading it. We do
%   not have that. Calling qcompile/1 seems the best start.

compile(File, _Options) :-
    qcompile(File).
