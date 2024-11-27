/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
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

:- module(exceptions,
          [ catch/4,                    % :Goal, :ErrorType, ?Ball, :Recover
            exception/2,                % :ExceptionType, ?Ball
            exception_type/2            % +Type, +Term
          ]).
:- use_module(library(error)).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    catch(0, :, ?, 0),
    exception(:,?).
:- multifile
    error_term/2,                       % Type, Formal
    exception_term/2.                   % Type, Exception

/** <module> Exception classification

Prolog catch/3 selects errors based on  unification. This is problematic
for two reasons. First, one typically  wants   the  exception term to be
more specific than the term  passed  to   the  2nd  (`Ball`) argument of
catch/3. Second, in many situations one wishes to select multiple errors
that may be raised  by  some  operations,   but  let  the  others  pass.
Unification is often not suitable  for   this.  For  example, open/3 can
raise an _existence_error_ or a _permission_error_  (and a couple more),
but  _existence_error_  are  also  raised  on,  for  example,  undefined
procedures. This is very hard  to  specify,   Below  is  an attempt that
still assumes nothing throws error(_,_).

```
    catch(open(...), error(Formal,ImplDefined),
          (   ( Formal = existence_error(source_sink,_)
              ; Formal = permission_error(open, source_sink, _)
              )
          ->  <handle>
          ;   throw(Formal, ImplDefined)
          )),
    ...
```

Besides being hard to specify,  actual   Prolog  systems  define a large
number of additional error terms  because   there  is  no reasonable ISO
exception  defined.  For   example,   SWI-Prolog    open/3   may   raise
resource_error(max_files) if the maximum number of   file handles of the
OS is exceeded.

As a result, we see a lot of Prolog   code  in the wild that simply uses
the construct below to simply fail. But, this may fail for lack of stack
space, a programmer error that causes a type error, etc. This both makes
it much harder to debug the code  and provide meaningful feedback to the
user of the application.

```
    catch(Goal, _, fail)
```

Many programing languages have their exceptions   organised by a (class)
hierarchy. Prolog has no hierarchy of terms. We introduce exception/2 as
exception(+Type, ?Term), which can both be used   as  a type test for an
exception term and as a _constraint_ for  the `Ball` of catch/3. Using a
predicate we can express abstractions over concrete exception terms with
more flexibility than  a  hierarchy.   Using  a  _multifile_  predicate,
libraries can add their exceptions  to   defined  types or introduce new
types.

The predicate catch/4 completes the interface.
*/

%!  catch(:Goal, +ExceptionType, ?Ball, :Recover)
%
%   As    catch/3,    only     catching      exceptions     for    which
%   exception(ErrorType,Ball) is true. See  error/2.   For  example, the
%   code below properly  informs  the  user   some  file  could  not  be
%   processed due do some issue with   `File`,  while propagating on all
%   other reasons while process/1 could not be executed.
%
%   ```
%       catch(process(File), file_error, Ball,
%             file_not_processed(File, Ball))
%
%   file_not_processed(File, Ball) :-
%       message_to_string(Ball, Msg),
%       format(user_error, 'Could not process ~p: ~s', [File, Msg]).
%   ```

:- noprofile(catch/4).

catch(Goal, ErrorType, Ball, Recover) :-
    exception(ErrorType, Ball),
    catch(Goal, Ball, Recover),
    del_attr(Ball, freeze).

%!  exception(:Type, --Ball) is det.
%!  exception(:Type, +Ball) is semidet.
%
%   If Ball is unbound, adds a delayed goal that tests the error belongs
%   to Type when Ball is  instantiated   (by  catch/3).  Else succeed is
%   error is of the specified Type.
%
%   Note that the delayed goal is added using freeze/2 and therefore the
%   stepwise   instantiation   of   Ball    does     not    work,   e.g.
%   exception(file_error, error(Formal,_)) immediately fails.
%
%   Error types may be  defined  or   extended  (e.g.,  by libraries) by
%   adding  clauses  to  the  multifile    predicates  error_term/2  and
%   exception_term/2. _Modules_ may (re-)define local  error types using
%   the exception_type/2 directive.

exception(Type, Ball) :-
    freeze(Ball, is_exception(Type, Ball)).

is_exception(M:Type, Ball) :-
    is_exception(Type, M, Ball).

is_exception((A;B), M, Ball) =>
    (   is_exception(A, M, Ball)
    ->  true
    ;   is_exception(B, M, Ball)
    ).
is_exception(\+A, M, Ball) =>
    \+ is_exception(A, M, Ball).
is_exception(Type, M, Ball) =>
    (   ex_term(Type, M, Pattern)
    *-> subsumes_term(Pattern, Ball),
        !
    ;   existence_error(exception_type, Type)
    ).

%!  ex_term(+Type, +Module, -Term) is nondet.

ex_term(Type, Module, error(Term,_)) :-
    (   current_predicate(Module:'$error_term'/2),
        Module:'$error_term'(Type, Term)
    *-> true
    ;   error_term(Type, Term)
    ).
ex_term(Type, Module, Term) :-
    (   current_predicate(Module:'$exception_term'/2),
        Module:'$exception_term'(Type, Term)
    *-> true
    ;   exception_term(Type, Term)
    ).

%!  error_term(?Type, ?Term) is nondet.
%
%   Describe the formal part of error(Formal,ImplDefined) exceptions.

error_term(file_error, existence_error(source_sink, _Culprit)).
error_term(file_error, permission_error(open, source_sink, _Culprit)).
error_term(file_error, resource_error(max_files)).
error_term(file_error, representation_error(max_symbolic_links)).
error_term(file_error, representation_error(max_path_length)).

error_term(network_error, socket_error(_Code, _Message)).
error_term(network_error, timeout_error(_Operation, _Culprit)).
error_term(network_error, io_error(_Operation, _Culprit)).

error_term(timeout, timeout_error(_Operation, _Culprit)).

error_term(evaluation_error, evaluation_error(_)).

%!  exception_term(?Type, ?Term) is nondet.
%
%   Describe exceptions that are not error(Formal, _) terms.

exception_term(timeout, time_limit_exceeded).
exception_term(timeout, time_limit_exceeded(_TimeLimit)).

%!  exception_type(+Type, +Term)
%
%   Declare all exceptions subsumed by Term to  be an exception of Type.
%   This declaration is module specific.

exception_type(Type, Term) :-
    throw(error(context_error(nodirective, exception_type(Type, Term)), _)).

exception_type_clause(Type, error(Formal, Var), Clause),
    ground(Type), var(Var) =>
    Clause = '$error_term'(Type, Formal).
exception_type_clause(Type, Exception, Clause),
    ground(Type) =>
    Clause = '$exception_term'(Type, Exception).

add_decl(Clause, Clauses) :-
    prolog_load_context(module, Module),
    pi_head(PI, Clause),
    (   current_predicate(Module:PI)
    ->  Clauses = Clause
    ;   Module == user
    ->  Clauses = [(:- multifile(PI)), Clause]
    ;   Clauses = [(:- discontiguous(PI)), Clause]
    ).

system:term_expansion((:-exception_type(Type, Term)), Clauses) :-
    exception_type_clause(Type, Term, Clause),
    add_decl(Clause, Clauses).


