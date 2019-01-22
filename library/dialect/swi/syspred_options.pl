/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2018, VU University Amsterdam
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

:- module(prolog_system_predicate_options, []).

/** <module> Provide declarations for options to built-in predicates
*/

:- predicate_options(system:open/4, 4,
		     [ type(oneof([text,binary])),
		       alias(atom),
		       encoding(encoding),
		       bom(boolean),
		       create(list(atom)),
		       eof_action(oneof([eof_code,error,reset])),
		       buffer(oneof([full,line,false])),
		       close_on_abort(boolean),
		       lock(oneof([none,read,shared,write,exclusive])),
		       wait(boolean),
		       locale(any)		% no type-check yet
		     ]).
:- predicate_options(system:write_term/3, 3,
		     [ attributes(oneof([ignore,dots,write,portray])),
		       backquoted_string(boolean),
		       blobs(oneof([portray])),
		       character_escapes(boolean),
		       cycles(boolean),
		       fullstop(boolean),
		       nl(boolean),
		       ignore_ops(boolean),
		       max_depth(nonneg),
		       module(atom),
		       numbervars(boolean),
		       partial(boolean),
		       portray(boolean),
		       portray_goal(callable+2), % as in meta_predicate
		       priority(between(0,1200)),
		       quoted(boolean),
		       spacing(oneof([standard,next_argument])),
		       variable_names(list)
		     ]).
:- predicate_options(system:write_term/2, 2,
		     [ pass_to(system:write_term/3, 3)
		     ]).
:- predicate_options(system:write_length/3, 3,
		     [ max_length(nonneg),
		       pass_to(system:write_term/3, 3)
		     ]).
:- predicate_options(system:read_clause/3, 3,
		     [ syntax_errors(oneof([error,fail,quiet,dec10])),
		       process_comment(boolean),
		       term_position(-any),
		       variable_names(-list),
		       subterm_positions(-any),
		       comments(-list)
		     ]).
:- predicate_options(system:read_term/3, 3,
		     [ backquoted_string(boolean),
		       character_escapes(boolean),
		       comments(-any),
		       cycles(boolean),
		       double_quotes(boolean),
		       module(atom),
		       singletons(-list),
		       syntax_errors(oneof([error,fail,quiet,dec10])),
		       subterm_positions(-any),
		       term_position(-any),
		       variables(-list),
		       variable_names(-list)
		     ]).
:- predicate_options(system:read_term/2, 2,
		     [ pass_to(system:read_term/3, 3)
		     ]).
:- predicate_options(system:numbervars/4, 4,
		     [ functor_name(atom),
		       attvar(oneof([skip,bind,error])),
		       singletons(boolean)
		     ]).
:- predicate_options(system:absolute_file_name/3, 3,
		     [ extensions(list(atom)),
		       relative_to(atom),
		       access(oneof([read,write,append,execute,exist,none])),
		       file_type(oneof([txt,prolog,executable,directory])),
		       file_errors(oneof([fail,error])),
		       solutions(oneof([first,all])),
		       expand(boolean)
		     ]).
:- predicate_options(system:load_files/2, 2,
		     [ autoload(boolean),
		       derived_from(atom),
                       dialect(atom),
		       encoding(encoding),
		       expand(boolean),
		       format(oneof([source,qlf])),
		       if(oneof([true,changed,not_loaded])),
		       imports(any),
		       modified(float),
		       module(atom),
                       check_script(boolean),
		       must_be_module(boolean),
		       qcompile(oneof([never,auto,large,part])),
                       optimise(boolean),
		       redefine_module(oneof([false,true,ask])),
		       reexport(boolean),
		       sandboxed(boolean),
		       scope_settings(boolean),
		       silent(boolean),
		       stream(any)
		     ]).
:- predicate_options(system:qcompile/2, 2,
		     [ pass_to(system:load_files/2, 2)
		     ]).
:- predicate_options(system:close/2, 2,
		     [ force(boolean)
		     ]).
:- predicate_options(system:create_prolog_flag/3, 3,
		     [ access(oneof([read_write,read_only])),
		       type(oneof([boolean,atom,integer,float,term])),
		       keep(boolean)
		     ]).
:- predicate_options(system:qsave_program/2, 2,
		     [ local(nonneg),
		       global(nonneg),
		       trail(nonneg),
		       argument(nonneg),
		       goal(callable),
		       toplevel(callable),
		       init_file(atom),
		       class(oneof([runtime,kernel,development])),
		       autoload(boolean),
		       map(atom),
		       op(oneof([save,standard])),
		       stand_alone(boolean),
		       emulator(atom)
		     ]).
:- predicate_options(system:thread_create/3, 3,
		     [ alias(atom),
		       at_exit(callable),
		       debug(boolean),
		       inherit_from(any),
		       detached(boolean),
                       stack_limit(nonneg),
		       c_stack(nonneg),
                       queue_max_size(nonneg)
		     ]).
:- predicate_options(system:message_queue_create/2, 2,
		     [ alias(atom),
		       max_size(nonneg)
		     ]).
:- predicate_options(system:mutex_create/2, 2,
		     [ alias(atom)
		     ]).
:- predicate_options(system:thread_send_message/3, 3,
		     [ timeout(number),
		       deadline(number)
		     ]).
:- predicate_options(system:thread_get_message/3, 3,
		     [ timeout(number),
		       deadline(number)
		     ]).
:- predicate_options(system:locale_create/3, 3,
		     [ alias(atom),
		       decimal_point(atom),
		       thousands_sep(atom),
		       grouping(list(any))
		     ]).
:- predicate_options(system:term_string/3, 3,
		     [ pass_to(system:write_term/3, 3),
		       pass_to(system:read_term/3, 3)
		     ]).

