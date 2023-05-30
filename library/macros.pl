:- module(macros,
          [ macro_position/1,           % -Position
                                        % private
            expand_macros/5,            % +M, +In, -Out, +P0, -P
            include_macros/3,           % +M,+Macro,-Expanded
            op(10, fx, #)
          ]).
:- use_module(library(terms)).
:- use_module(library(error)).
:- use_module(library(lists)).

/** <module> Macro expansion

This library defines a  macro  expansion   mechanism  that  operates  on
arbitrary terms. Unlike term_expansion/2 and goal_expansion/2, a term is
explicitly designed for expansion using the  term `#(Macro)`. Macros are
first of all intended to deal with compile time constants. They can also
be used to construct terms at compile time.

## Defining and using macros {#macros-define-and-use}

Macros are defined for  the  current  module   using  one  of  the three
constructs below.

    #define(Macro, Replacement).
    #define(Macro, Replacement) :- Code.
    #import(ModuleFile).

`Macro` is a _callable  term_,  not   being  define(_,_),  or import(_).
`Replacement` is an arbitrary Prolog  term.   `Code`  is  a Prolog _body
term_ that _must_ succeed and can be used to dynamically generate (parts
of) `Replacement`.

The `#import(ModuleFile)` definition makes  all   macros  from the given
module available for expansion in the   module it appears. Normally this
shall be appear after local macro definitions.

A macro is called  using  the  term   `#(Macro)`.  `#`  is  defined as a
low-priority (10) prefix operator to  allow   for  `#Macro`.  Macros can
appear at the following places:

  - An entire sentence (clause)
  - Any argument of a compound.  This implies also the head and body of
    a clause.
  - Anywhere in a list, including as the tail of a list
  - As a value for a dict key or as a dict key name.

Macros can __not__ appear as name of a compound or tag of a dict. A term
`#Macro` appearing in one of the allowed places __must__ have a matching
macro defined, i.e., `#Macro`  is  __always__   expanded.  An  error  is
emitted if the expansion fails. Macro   expansion is applied recursively
and thus, macros may be passed to   macro  arguments and macro expansion
may use other macros.

Macros are matched to terms  using   _Single  Sided  Unification_ (SSU),
implemented using `Head => Body` rules.   This implies that the matching
never instantiates variables in the term that is being expanded.

Below are some examples. The  first  line   defines  the  macro  and the
indented line after show example usage of the macro.

```
#define(max_width, 100).
    W < #max_width

#define(calc(Expr), Value) :- Value is Expr.
    fact(#calc(#max_width*2)).

#define(pt(X,Y), point{x:X, y:Y}).
    reply_json(json{type:polygon,
                    points:[#pt(0,0), #pt(0,5), #pt(5,0)]}).
```

Macro expansion expands terms `#(Callable)`.  If   the  argument  to the
#-term is not a `callable`, the  #-term   is  not modified. This notably
allows for `#(Var)`  as  used  by   library(clpfd)  to  indicate  that a
variable is constraint to be an (clp(fd)) integer.


## Implementation details {#macros-implementation}

A macro `#define(Macro, Expanded) :- Body.`  is, after some basic sanity
checks, translated into a rule

    '$macro'(Macro, Var), Body => Var = Expanded.

The `#import(File)` is translated into `:-   use_module(File, [])` and a
_link clause_ that links the macro expansion  from the module defined in
`File` to the current module.

Macro expansion is realised by creating a clause for term_expansion/2 in
the current module.  This  clause  results   from  expanding  the  first
`#define` or `#import` definition. Thus, if   macros  are defined before
any other local definition for term_expansion/2   it  is executed as the
first step. The macro expansion fails if no macros were encounted in the
term, allowing other term_expansion rules local   to  the module to take
effect. In other words, a term  holding   macros  is  not subject to any
other term expansion local  to  the  module.   It  is  subject  to  term
expansion defined in module `user` and  `system` that is performed after
the local expansion is completed.


## Predicates {#macros-predicates}

*/

define_macro((#define(From, To)), Clauses) =>
    valid_macro(From),
    Clause0 = ('$macro'(From, Expansion) => Expansion = To),
    prepare_module(Clause0, Clauses).
define_macro((#define(From, To) :- Cond), Clauses) =>
    valid_macro(From),
    Clause0 = ('$macro'(From, Expansion), Cond => Expansion = To),
    prepare_module(Clause0, Clauses).
define_macro((#import(File)), Clauses) =>
    use_module(File, []),
    source_file_property(File, module(M)),
    Clause0 = ('$macro'(Macro, Expansion), include_macros(M, Macro, Expansion)
                  => true),
    prepare_module(Clause0, Clauses).

define_macro(_, _) =>
    fail.

valid_macro(Macro), reserved_macro(Macro) =>
    domain_error(macro, Macro).
valid_macro(Macro), callable(Macro) =>
    true.
valid_macro(_Macro) =>
    fail.

reserved_macro(define(_,_)) => true.
reserved_macro(import(_)) => true.
reserved_macro(_) => fail.

:- multifile
    error:has_type/2.

error:has_type(macro, Term) :-
    callable(Term),
    \+ reserved_macro(Term).

prepare_module(Clause0, Clauses) :-
    prolog_load_context(module, M),
    (   is_prepared_module(M)
    ->  Clauses = Clause0
    ;   Clauses = [ (:- multifile(('$macro'/2,term_expansion/4))),
                    (term_expansion(In, PIn, Out, Pout) :-
                        expand_macros(M, In, Out, PIn, Pout)),
                    expand_macros,
                    Clause0
                  ]
    ).

is_prepared_module(M) :-
    current_predicate(M:expand_macros/0),
    \+ predicate_property(M:expand_macros, imported_from(_)).

%!  include_macros(+M, +Macro, -Expanded) is semidet.
%
%   Include macros from another module. This   predicate is a helper for
%   `#import(File)`. It calls '$macro'/2 in  M,   but  fails silently in
%   case Macro is not defined in  M  as   it  may  be defined in another
%   imported macro file or further down in the current file.

include_macros(M, Macro, Expanded) :-
    catch(M:'$macro'(Macro, Expanded),
          error(existence_error(matching_rule,
                                M:'$macro'(Macro,_)),_),
          fail).

%!  expand_macros(+Module, +TermIn, -TermOut, +PosIn, -PosOut) is semidet.
%
%   Perform macro expansion on  TermIn  with   layout  PosIn  to produce
%   TermOut with layout PosOut. The transformation   is performed if the
%   current load context module is Module (see prolog_load_context/2).
%
%   This predicate is not intended for direct usage.

expand_macros(M, T0, T, P0, P) :-
    prolog_load_context(module, M),
    \+ is_define(T0),
    expand_macros(M, T0, T, P0, P, _State0, _State),
    T \== T0.

is_define(#Macro), reserved_macro(Macro) => true.
is_define((#Macro :- _)), reserved_macro(Macro) => true.
is_define(_) => fail.

:- meta_predicate
    foldsubterms_pos(6, +, -, +, -, +, -).

expand_macros(M, T0, T, P0, P, State0, State) :-
    foldsubterms_pos(expand_macro(M), T0, T, P0, P, State0, State).

expand_macro(M, #Macro, T, P0, P, State0, State) =>
    valid_macro(Macro),
    arg_pos(1, P0, P1),
    call_macro(M, Macro, Expanded, P1, P2),
    expand_macros(M, Expanded, T, P2, P, State0, State).
expand_macro(_, \#(T0), T, P0, P, State0, State) =>
    arg_pos(1, P0, P),
    T = T0, State = State0.
expand_macro(_, _, _, _, _, _, _) =>
    fail.

call_macro(M, Macro, Expanded, P0, P) :-
    b_setval('$macro_position', P0),
    catch(M:'$macro'(Macro, Expanded),
          error(existence_error(matching_rule, _), _),
          macro_failed(Macro, P0)),
    fix_pos_shape(Macro, Expanded, P0, P),
    b_setval('$macro_position', 0).

macro_failed(Macro, TermPos) :-
    macro_error_position(TermPos, Pos),
    throw(error(existence_error(macro, Macro), Pos)).

macro_error_position(TermPos, Position) :-
    macro_position(TermPos, AtMacro),
    !,
    prolog_load_context(stream, Input),
    stream_position_to_position_term(Input, AtMacro, Position).
macro_error_position(_, _).

stream_position_to_position_term(Stream, StreamPos,
                                 stream(Stream, Line, LinePos, CharNo)) :-
    stream_position_data(line_count, StreamPos, Line),
    stream_position_data(line_position, StreamPos, LinePos),
    stream_position_data(char_count, StreamPos, CharNo).

%!  macro_position(-Position) is det.
%
%   True when Position is the position of  the macro. Position is a term
%   `File:Line:LinePos`. If `File` is unknown it is unified with `-`. If
%   Line and/or LinePos are  unknown  they   are  unified  with  0. This
%   predicate can be used in the body   of a macro definition to provide
%   the source location. The example below defines `#pp(Var)` to print a
%   variable together with the variable name and source location.
%
%   ```
%   #define(pp(Var), print_message(debug, dump_var(Pos, Name, Var))) :-
%       (   var_property(Var, name(Name))
%       ->  true
%       ;   Name = 'Var'
%       ),
%       macro_position(Pos).
%
%   :- multifile prolog:message//1.
%   prolog:message(dump_var(Pos,Name,Var)) -->
%       [ url(Pos), ': ',
%         ansi([fg(magenta),bold], '~w', [Name]), ' = ',
%         ansi(code, '~p', [Var])
%       ].
%   ```

macro_position(File:Line:LinePos) :-
    prolog_load_context(file, File),
    !,
    (   b_getval('$macro_position', TermPos),
        macro_position(TermPos, StreamPos)
    ->  stream_position_data(line_count, StreamPos, Line),
        stream_position_data(line_position, StreamPos, LinePos)
    ;   Line = 0,
        LinePos = 0
    ).
macro_position((-):0:0).

macro_position(TermPos, AtMacro) :-
    compound(TermPos),
    arg(1, TermPos, MacroStartCharCount),
    integer(MacroStartCharCount),
    prolog_load_context(stream, Input),
    stream_property(Input, reposition(true)),
    stream_property(Input, position(Here)),
    prolog_load_context(term_position, ClauseStart),
    stream_position_data(char_count, ClauseStart, ClauseStartCharCount),
    MacroStartCharCount >= ClauseStartCharCount,
    $,
    set_stream_position(Input, ClauseStart),
    Skip is MacroStartCharCount - ClauseStartCharCount,
    forall(between(1, Skip, _), get_char(Input, _)),
    stream_property(Input, position(AtMacro)),
    set_stream_position(Input, Here).

%!  fix_pos_shape(+TermIn, +TermOut, +PosIn, -PosOut) is det.
%
%   Fixup PosIn to be a position term that is compatible to Term.
%
%   @bug This predicate is largely unimplemented.

fix_pos_shape(_, _, P0, _), var(P0) =>
    true.
fix_pos_shape(_, V, P0, P),
    atomic(V),
    compound(P0), compound_name_arity(P0, _, Arity), Arity >= 2 =>
    P = F-T,
    arg(1, P0, F),
    arg(2, P0, T).
fix_pos_shape(_, _, P0, P) =>
    P = P0.

%! foldsubterms_pos(:Goal, +TermIn, -TermOut, +PosIn, -PosOut,
%!                  +State0, -State) is det.
%
%  As  foldsubterms/5,  but  also  transforms   the  layout  term.  This
%  predicate may later be moved to  e.g. library(prolog_code) to make it
%  publically available.

foldsubterms_pos(Goal, Term1, Term2, P1, P2, State0, State) :-
    call(Goal, Term1, Term2, P1, P2, State0, State),
    !.
foldsubterms_pos(Goal, Term1, Term2, P1, P2, State0, State) :-
    is_dict(Term1),
    !,
    pos_parts(dict, P1, P2, VPos1, VPos2),
    dict_pairs(Term1, Tag, Pairs1),
    fold_dict_pairs(Pairs1, Pairs2, VPos1, VPos2, Goal, State0, State),
    dict_pairs(Term2, Tag, Pairs2).
foldsubterms_pos(Goal, Term1, Term2, P1, P2, State0, State) :-
    nonvar(Term1), Term1 = [_|_],       % [] is not a list
    !,
    pos_parts(list, P1, P2, list(Elms1,Tail1), list(Elms2,Tail2)),
    fold_list(Term1, Term2, Elms1, Elms2, Tail1, Tail2, Goal, State0, State).
foldsubterms_pos(Goal, Term1, Term2, P1, P2, State0, State) :-
    compound(Term1),
    !,
    pos_parts(compound, P1, P2, ArgPos1, ArgPos2),
    same_functor(Term1, Term2, Arity),
    foldsubterms_(1, Arity, Goal, Term1, Term2, ArgPos1, ArgPos2, State0, State).
foldsubterms_pos(_, Term, Term, P, P, State, State).

:- det(fold_dict_pairs/7).
fold_dict_pairs([], [], KVPos, KVPos, _, State, State).
fold_dict_pairs([K0-V0|T0], [K-V|T1], KVPos0, KVPos, Goal, State0, State) :-
    (   nonvar(KVPos0),
        selectchk(key_value_position(F,T,SF,ST,K0,KP0,VP0), KVPos0,
                  key_value_position(F,T,SF,ST,K, KP, VP),  KVPos1)
    ->  true
    ;   true
    ),
    foldsubterms_pos(Goal, K0, K, KP0, KP, State0, State1),
    foldsubterms_pos(Goal, V0, V, VP0, VP, State1, State2),
    fold_dict_pairs(T0, T1, KVPos1, KVPos, Goal, State2, State).

:- det(fold_list/9).
fold_list(Var0, Var, EP, EP, TP0, TP, Goal, State0, State) :-
    var(Var0),
    !,
    foldsubterms_pos(Goal, Var0, Var, TP0, TP, State0, State).
fold_list([], [], [], [], TP, TP, _, State, State) :-
    !.
fold_list([H0|T0], [H|T], [EP0|EPT0], [EP1|EPT1], TP1, TP2, Goal, State0, State) :-
    !,
    foldsubterms_pos(Goal, H0, H, EP0, EP1, State0, State1),
    fold_list(T0, T, EPT0, EPT1, TP1, TP2, Goal, State1, State).
fold_list(T0, T, EP, EP, TP0, TP, Goal, State0, State) :-
    foldsubterms_pos(Goal, T0, T, TP0, TP, State0, State).

:- det(foldsubterms_/9).
foldsubterms_(I, Arity, Goal, Term1, Term2, PosIn, PosOut, State0, State) :-
    I =< Arity,
    !,
    (   PosIn = [AP1|APT1]
    ->  PosOut = [AP2|APT2]
    ;   true
    ),
    arg(I, Term1, A1),
    arg(I, Term2, A2),
    foldsubterms_pos(Goal, A1, A2, AP1, AP2, State0, State1),
    I2 is I+1,
    foldsubterms_(I2, Arity, Goal, Term1, Term2, APT1, APT2, State1, State).
foldsubterms_(_, _, _, _, _, _, [], State, State).

:- det(pos_parts/5).
pos_parts(_, Var, _, _, _), var(Var) => true.
pos_parts(Type, parentheses_term_position(F,T,In), PosOut, SubIn, SubOut) =>
    PosOut = parentheses_term_position(F,T,Out),
    pos_parts(Type, In, Out, SubIn, SubOut).
pos_parts(compound, term_position(From, To, FFrom, FTo, SubPos),
          PosOut, SubIn, SubOut) =>
    PosOut = term_position(From, To, FFrom, FTo, SubOut),
    SubIn = SubPos.
pos_parts(compound, brace_term_position(From, To, ArgPos0),
          PosOut, SubIn, SubOut) =>
    PosOut = brace_term_position(From, To, ArgPos),
    SubIn = [ArgPos0],
    SubOut = [ArgPos].
pos_parts(list, list_position(From, To, Elms, Tail),
          PosOut, SubIn, SubOut) =>
    PosOut = list_position(From, To, Elms1, Tail1),
    SubIn = list(Elms, Tail),
    SubOut = list(Elms1, Tail1).
pos_parts(dict, dict_position(From, To, TagFrom, TagTo, KVPosIn),
          PosOut, SubIn, SubOut) =>
    PosOut = dict_position(From, To, TagFrom, TagTo, SubOut),
    SubIn = KVPosIn.
pos_parts(_, _, _, _, _) =>
    true.                               % mismatch term and pos

arg_pos(_, TermPos, _), var(TermPos) => true.
arg_pos(I, parentheses_term_position(_,_,TP), AP) =>
    arg_pos(I, TP, AP).
arg_pos(I, term_position(_,_,_,_,APL), AP) =>
    ignore(nth1(I, APL, AP)).
arg_pos(1, brace_term_position(_,_,TPA), AP) =>
    AP = TPA.
arg_pos(_,_,_) =>
    true.

		 /*******************************
		 *             REGISTER		*
		 *******************************/

% Hook to deal with #define and #import if this library was loaded into
% this context.

system:term_expansion(In, Out) :-
    is_define(In),
    prolog_load_context(module, M),
    predicate_property(M:expand_macros(_,_,_,_,_), imported_from(macros)),
    $,
    define_macro(In, Out).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(domain_error(macro, Macro)) -->
    [ 'Invalid macro: ~p'-[Macro] ].
prolog:error_message(existence_error(macro, Macro)) -->
    [ 'Failed to expand macro: ~p'-[Macro] ].


		 /*******************************
		 *         IDE SUPPORT		*
		 *******************************/

:- multifile prolog_colour:term_colours/2.

prolog_colour:term_colours(#define(_Macro, _Replacement),
                           expanded - [ expanded - [ classify, classify ]]).
prolog_colour:term_colours((#define(_Macro, _Replacement) :- _Body),
                           neck(:-) - [ expanded - [ expanded - [ classify, classify ]],
                                        body
                                      ]).
prolog_colour:term_colours(#import(_File),
                           expanded - [ expanded - [ file ]]).
