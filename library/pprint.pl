/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2022, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(prolog_pretty_print,
          [ print_term/2        % +Term, +Options
          ]).
:- autoload(library(option),
            [merge_options/3, select_option/3, select_option/4,
             option/2, option/3]).

/** <module> Pretty Print Prolog terms

This module is a first  start  of   what  should  become a full-featured
pretty printer for Prolog  terms  with   many  options  and  parameters.
Eventually,  it  should  replace  portray_clause/1   and  various  other
special-purpose predicates.

@tbd This is just a quicky. We  need proper handling of portray/1, avoid
printing very long terms  multiple   times,  spacing (around operators),
etc.

@tbd Use a record for the option-processing.

@tbd The current approach is far too simple, often resulting in illegal
     terms.
*/

:- predicate_options(print_term/2, 2,
                     [ output(stream),
                       right_margin(integer),
                       left_margin(integer),
                       tab_width(integer),
                       indent_arguments(integer),
                       operators(boolean),
                       write_options(list)
                     ]).

%!  print_term(+Term, +Options) is det.
%
%   Pretty print a Prolog term. The following options are processed:
%
%     - output(+Stream)
%       Define the output stream.  Default is `user_output`
%     - right_margin(?Column)
%       Width of a line. If the output is a _tty_ and tty_size/2
%       can produce a size the default is the number of columns
%       minus 8.  Otherwise the default is 72 characters.  If the
%       Column is unbound it is unified with the computed value.
%     - left_margin(+Integer)
%       Left margin for continuation lines.  Default is the current
%       line position or `0` if that is not available.
%     - tab_width(+Integer)
%       Distance between tab-stops.  Default is 8 characters.
%     - indent_arguments(+Spec)
%       Defines how arguments of compound terms are placed.  Defined
%       values are:
%       $ `false` :
%       Simply place them left to right (no line-breaks)
%       $ `true` :
%       Place them vertically, aligned with the open bracket (not
%       implemented)
%       $ `auto` (default) :
%       As horizontal if line-width is not exceeded, vertical
%       otherwise.
%       $ An integer :
%       Place them vertically aligned, <N> spaces to the right of
%       the beginning of the head.
%     - operators(+Boolean)
%       This is the inverse of the write_term/3 option `ignore_ops`.
%       Default is to respect them.
%     - write_options(+List)
%       List of options passed to write_term/3 for terms that are
%       not further processed.  Default:
%
%       ```
%           [ numbervars(true),
%             quoted(true),
%             portray(true)
%           ]
%       ```
%     - fullstop(Boolean)
%       If `true` (default `false`), add a full stop (.) to the output.
%     - nl(Boolean)
%       If `true` (default `false`), add a newline to the output.

print_term(Term, Options) :-
    defaults(Defs0),
    select_option(write_options(WrtDefs), Defs0, Defs),
    select_option(write_options(WrtUser), Options, Options1, []),
    merge_options(WrtUser, WrtDefs, WrtOpts),
    merge_options(Options1, Defs, Options2),
    Options3 = [write_options(WrtOpts)|Options2],
    default_margin(Options3, Options4),
    \+ \+ print_term_2(Term, Options4).

print_term_2(Term, Options) :-
    prepare_term(Term, Template, Cycles, Constraints),
    option(write_options(WrtOpts), Options),
    option(max_depth(MaxDepth), WrtOpts, infinite),

    dict_create(Context, #, [max_depth(MaxDepth)|Options]),
    pp(Template, Context, Options),
    print_extra(Cycles, Context, 'where', Options),
    print_extra(Constraints, Context, 'with constraints', Options),
    (   option(fullstop(true), Options)
    ->  option(output(Out), Options),
        put_char(Out, '.')
    ;   true
    ),
    (   option(nl(true), Options)
    ->  option(output(Out2), Options),
        nl(Out2)
    ;   true
    ).

print_extra([], _, _, _) :- !.
print_extra(List, Context, Comment, Options) :-
    option(output(Out), Options),
    format(Out, ', % ~w', [Comment]),
    context(Context, indent, Indent),
    NewIndent is Indent+4,
    modify_context(Context, [indent=NewIndent], Context1),
    print_extra_2(List, Context1, Options).

print_extra_2([H|T], Context, Options) :-
    option(output(Out), Options),
    context(Context, indent, Indent),
    indent(Out, Indent, Options),
    pp(H, Context, Options),
    (   T == []
    ->  true
    ;   format(Out, ',', []),
        print_extra_2(T, Context, Options)
    ).


%!  prepare_term(+Term, -Template, -Cycles, -Constraints)
%
%   Prepare a term, possibly  holding   cycles  and  constraints for
%   printing.

prepare_term(Term, Template, Cycles, Constraints) :-
    term_attvars(Term, []),
    !,
    Constraints = [],
    '$factorize_term'(Term, Template, Factors),
    bind_non_cycles(Factors, 1, Cycles),
    numbervars(Template+Cycles+Constraints, 0, _,
               [singletons(true)]).
prepare_term(Term, Template, Cycles, Constraints) :-
    copy_term(Term, Copy, Constraints),
    '$factorize_term'(Copy, Template, Factors),
    bind_non_cycles(Factors, 1, Cycles),
    numbervars(Template+Cycles+Constraints, 0, _,
               [singletons(true)]).


bind_non_cycles([], _, []).
bind_non_cycles([V=Term|T], I, L) :-
    unify_with_occurs_check(V, Term),
    !,
    bind_non_cycles(T, I, L).
bind_non_cycles([H|T0], I, [H|T]) :-
    H = ('$VAR'(Name)=_),
    atom_concat('_S', I, Name),
    I2 is I + 1,
    bind_non_cycles(T0, I2, T).


defaults([ output(user_output),
           depth(0),
           indent_arguments(auto),
           operators(true),
           write_options([ quoted(true),
                           numbervars(true),
                           portray(true),
                           attributes(portray)
                         ]),
           priority(1200)
         ]).

default_margin(Options0, Options) :-
    default_right_margin(Options0, Options1),
    default_indent(Options1, Options).

default_right_margin(Options0, Options) :-
    option(right_margin(Margin), Options0),
    !,
    (   var(Margin)
    ->  tty_right_margin(Options0, Margin)
    ;   true
    ),
    Options = Options0.
default_right_margin(Options0, [right_margin(Margin)|Options0]) :-
    tty_right_margin(Options0, Margin).

tty_right_margin(Options, Margin) :-
    option(output(Output), Options),
    stream_property(Output, tty(true)),
    catch(tty_size(_Rows, Columns), error(_,_), fail),
    !,
    Margin is Columns - 8.
tty_right_margin(_, 72).

default_indent(Options0, Options) :-
    option(output(Output), Options0),
    (   stream_property(Output, position(Pos))
    ->  stream_position_data(line_position, Pos, Column)
    ;   Column = 0
    ),
    option(left_margin(LM), Options0, Column),
    Options = [indent(LM)|Options0].


                 /*******************************
                 *             CONTEXT          *
                 *******************************/

context(Ctx, Name, Value) :-
    get_dict(Name, Ctx, Value).

modify_context(Ctx0, Mapping, Ctx) :-
    Ctx = Ctx0.put(Mapping).

dec_depth(Ctx, Ctx) :-
    context(Ctx, max_depth, infinite),
    !.
dec_depth(Ctx0, Ctx) :-
    ND is Ctx0.max_depth - 1,
    Ctx = Ctx0.put(max_depth, ND).


                 /*******************************
                 *              PP              *
                 *******************************/

pp(Primitive, Ctx, Options) :-
    (   atomic(Primitive)
    ;   var(Primitive)
    ;   Primitive = '$VAR'(Var),
        (   integer(Var)
        ;   atom(Var)
        )
    ),
    !,
    pprint(Primitive, Ctx, Options).
pp(Portray, _Ctx, Options) :-
    option(write_options(WriteOptions), Options),
    option(portray(true), WriteOptions),
    option(output(Out), Options),
    with_output_to(Out, user:portray(Portray)),
    !.
pp(List, Ctx, Options) :-
    List = [_|_],
    !,
    context(Ctx, indent, Indent),
    context(Ctx, depth, Depth),
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    (   (   IndentStyle == false
        ->  true
        ;   IndentStyle == auto,
            print_width(List, Width, Options),
            option(right_margin(RM), Options),
            Indent + Width < RM
        )
    ->  pprint(List, Ctx, Options)
    ;   format(Out, '[ ', []),
        Nindent is Indent + 2,
        NDepth is Depth + 1,
        modify_context(Ctx, [indent=Nindent, depth=NDepth, priority=999], NCtx),
        pp_list_elements(List, NCtx, Options),
        indent(Out, Indent, Options),
        format(Out, ']', [])
    ).
pp(Dict, Ctx, Options) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, Tag, Pairs),
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    context(Ctx, indent, Indent),
    (   IndentStyle == false ; Pairs == []
    ->  pprint(Dict, Ctx, Options)
    ;   IndentStyle == auto,
        print_width(Dict, Width, Options),
        option(right_margin(RM), Options),
        Indent + Width < RM         % fits on a line, simply write
    ->  pprint(Dict, Ctx, Options)
    ;   format(atom(Buf2), '~q{ ', [Tag]),
        write(Out, Buf2),
        atom_length(Buf2, FunctorIndent),
        (   integer(IndentStyle)
        ->  Nindent is Indent + IndentStyle,
            (   FunctorIndent > IndentStyle
            ->  indent(Out, Nindent, Options)
            ;   true
            )
        ;   Nindent is Indent + FunctorIndent
        ),
        context(Ctx, depth, Depth),
        NDepth is Depth + 1,
        modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
        dec_depth(NCtx0, NCtx),
        pp_dict_args(Pairs, NCtx, Options),
        BraceIndent is Nindent - 2,         % '{ '
        indent(Out, BraceIndent, Options),
        write(Out, '}')
    ).
pp(Term, Ctx, Options) :-               % handle operators
    compound(Term),
    compound_name_arity(Term, Name, Arity),
    current_op(Prec, Type, Name),
    match_op(Type, Arity, Kind, Prec, Left, Right),
    option(operators(true), Options),
    !,
    quoted_op(Name, QName),
    option(output(Out), Options),
    context(Ctx, indent, Indent),
    context(Ctx, depth, Depth),
    context(Ctx, priority, CPrec),
    NDepth is Depth + 1,
    modify_context(Ctx, [depth=NDepth], Ctx1),
    dec_depth(Ctx1, Ctx2),
    LeftOptions  = Ctx2.put(priority, Left),
    FuncOptions  = Ctx2.put(embrace, never),
    RightOptions = Ctx2.put(priority, Right),
    (   Kind == prefix
    ->  arg(1, Term, Arg),
        (   (   space_op(Name)
            ;   need_space(Name, Arg, FuncOptions, RightOptions)
            )
        ->  Space = ' '
        ;   Space = ''
        ),
        (   CPrec >= Prec
        ->  format(atom(Buf), '~w~w', [QName, Space]),
            atom_length(Buf, AL),
            NIndent is Indent + AL,
            write(Out, Buf),
            modify_context(Ctx2, [indent=NIndent, priority=Right], Ctx3),
            pp(Arg, Ctx3, Options)
        ;   format(atom(Buf), '(~w~w', [QName,Space]),
            atom_length(Buf, AL),
            NIndent is Indent + AL,
            write(Out, Buf),
            modify_context(Ctx2, [indent=NIndent, priority=Right], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, ')', [])
        )
    ;   Kind == postfix
    ->  arg(1, Term, Arg),
        (   (   space_op(Name)
            ;   need_space(Name, Arg, FuncOptions, LeftOptions)
            )
        ->  Space = ' '
        ;   Space = ''
        ),
        (   CPrec >= Prec
        ->  modify_context(Ctx2, [priority=Left], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, '~w~w', [Space,QName])
        ;   format(Out, '(', []),
            NIndent is Indent + 1,
            modify_context(Ctx2, [indent=NIndent, priority=Left], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, '~w~w)', [Space,QName])
        )
    ;   arg(1, Term, Arg1),             % Infix operators
        arg(2, Term, Arg2),
        (   print_width(Term, Width, Options),
            option(right_margin(RM), Options),
            Indent + Width < RM
        ->  ToWide = false,
            (   (   space_op(Name)
                ;   need_space(Arg1, Name, LeftOptions, FuncOptions)
                ;   need_space(Name, Arg2, FuncOptions, RightOptions)
                )
            ->  Space = ' '
            ;   Space = ''
            )
        ;   ToWide = true,
            (   (   is_solo(Name)
                ;   space_op(Name)
                )
            ->  Space = ''
            ;   Space = ' '
            )
        ),
        (   CPrec >= Prec
        ->  (   ToWide == true,
                infix_list(Term, Name, List),
                List == [_,_|_]
            ->  Pri is min(Left,Right),
                modify_context(Ctx2, [space=Space, priority=Pri], Ctx3),
                pp_infix_list(List, QName, 2, Ctx3, Options)
            ;   modify_context(Ctx2, [priority=Left], Ctx3),
                pp(Arg1, Ctx3, Options),
                format(Out, '~w~w~w', [Space,QName,Space]),
                modify_context(Ctx2, [priority=Right], Ctx4),
                pp(Arg2, Ctx4, Options)
            )
        ;   (   ToWide == true,
                infix_list(Term, Name, List),
                List = [_,_|_]
            ->  Pri is min(Left,Right),
                format(Out, '( ', []),
                NIndent is Indent + 2,
                modify_context(Ctx2,
                           [space=Space, indent=NIndent, priority=Pri],
                               Ctx3),
                pp_infix_list(List, QName, 0, Ctx3, Options),
                indent(Out, Indent, Options),
                format(Out, ')', [])
            ;   format(Out, '(', []),
                NIndent is Indent + 1,
                modify_context(Ctx2, [indent=NIndent, priority=Left], Ctx3),
                pp(Arg1, Ctx3, Options),
                format(Out, '~w~w~w', [Space,QName,Space]),
                modify_context(Ctx2, [priority=Right], Ctx4),
                pp(Arg2, Ctx4, Options),
                format(Out, ')', [])
            )
        )
    ).
pp(Term, Ctx, Options) :-               % compound
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    context(Ctx, indent, Indent),
    (   IndentStyle == false
    ->  pprint(Term, Ctx, Options)
    ;   IndentStyle == auto,
        print_width(Term, Width, Options),
        option(right_margin(RM), Options),
        Indent + Width < RM         % fits on a line, simply write
    ->  pprint(Term, Ctx, Options)
    ;   compound_name_arguments(Term, Name, Args),
        format(atom(Buf2), '~q(', [Name]),
        write(Out, Buf2),
        atom_length(Buf2, FunctorIndent),
        (   integer(IndentStyle)
        ->  Nindent is Indent + IndentStyle,
            (   FunctorIndent > IndentStyle
            ->  indent(Out, Nindent, Options)
            ;   true
            )
        ;   Nindent is Indent + FunctorIndent
        ),
        context(Ctx, depth, Depth),
        NDepth is Depth + 1,
        modify_context(Ctx,
                       [indent=Nindent, depth=NDepth, priority=999],
                       NCtx0),
        dec_depth(NCtx0, NCtx),
        pp_compound_args(Args, NCtx, Options),
        write(Out, ')')
    ).


quoted_op(Op, Atom) :-
    is_solo(Op),
    !,
    Atom = Op.
quoted_op(Op, Q) :-
    format(atom(Q), '~q', [Op]).

%!  infix_list(+Term, ?Op, -List) is semidet.
%
%   True when List is a list of subterms  of Term that are the result of
%   the nested infix operator  Op.  Deals   both  with  `xfy`  and `yfx`
%   operators.

infix_list(Term, Op, List) :-
    phrase(infix_list(Term, Op), List).

infix_list(Term, Op) -->
    { compound(Term),
      compound_name_arity(Term, Op, 2)
    },
    (   {current_op(_Pri, xfy, Op)}
    ->  { arg(1, Term, H),
          arg(2, Term, Term2)
        },
        [H],
        infix_list(Term2, Op)
    ;   {current_op(_Pri, yfx, Op)}
    ->  { arg(1, Term, Term2),
          arg(2, Term, T)
        },
        infix_list(Term2, Op),
        [T]
    ).
infix_list(Term, Op) -->
    {atom(Op)},                      % we did something before
    [Term].

pp_infix_list([H|T], QName, IncrIndent, Ctx, Options) =>
    pp(H, Ctx, Options),
    context(Ctx, space, Space),
    (   T == []
    ->  true
    ;   option(output(Out), Options),
        format(Out, '~w~w', [Space,QName]),
        context(Ctx, indent, Indent),
        NIndent is Indent+IncrIndent,
        indent(Out, NIndent, Options),
        modify_context(Ctx, [indent=NIndent], Ctx2),
        pp_infix_list(T, QName, 0, Ctx2, Options)
    ).


%!  pp_list_elements(+List, +Ctx, +Options) is det.
%
%   Print the elements of a possibly open list as a vertical list.

pp_list_elements(_, Ctx, Options) :-
    context(Ctx, max_depth, 0),
    !,
    option(output(Out), Options),
    write(Out, '...').
pp_list_elements([H|T], Ctx0, Options) :-
    dec_depth(Ctx0, Ctx),
    pp(H, Ctx, Options),
    (   T == []
    ->  true
    ;   nonvar(T),
        T = [_|_]
    ->  option(output(Out), Options),
        write(Out, ','),
        context(Ctx, indent, Indent),
        indent(Out, Indent, Options),
        pp_list_elements(T, Ctx, Options)
    ;   option(output(Out), Options),
        context(Ctx, indent, Indent),
        indent(Out, Indent-2, Options),
        write(Out, '| '),
        pp(T, Ctx, Options)
    ).


pp_compound_args([], _, _).
pp_compound_args([H|T], Ctx, Options) :-
    pp(H, Ctx, Options),
    (   T == []
    ->  true
    ;   T = [_|_]
    ->  option(output(Out), Options),
        write(Out, ','),
        context(Ctx, indent, Indent),
        indent(Out, Indent, Options),
        pp_compound_args(T, Ctx, Options)
    ;   option(output(Out), Options),
        context(Ctx, indent, Indent),
        indent(Out, Indent-2, Options),
        write(Out, '| '),
        pp(T, Ctx, Options)
    ).


:- if(current_predicate(is_dict/1)).
pp_dict_args([Name-Value|T], Ctx, Options) :-
    option(output(Out), Options),
    line_position(Out, Pos0),
    pp(Name, Ctx, Options),
    write(Out, ':'),
    line_position(Out, Pos1),
    context(Ctx, indent, Indent),
    Indent2 is Indent + Pos1-Pos0,
    modify_context(Ctx, [indent=Indent2], Ctx2),
    pp(Value, Ctx2, Options),
    (   T == []
    ->  true
    ;   option(output(Out), Options),
        write(Out, ','),
        indent(Out, Indent, Options),
        pp_dict_args(T, Ctx, Options)
    ).
:- endif.

%       match_op(+Type, +Arity, +Precedence, -LeftPrec, -RightPrec

match_op(fx,    1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,    1, prefix,  P, _, P).
match_op(xf,    1, postfix, P, L, _) :- L is P - 1.
match_op(yf,    1, postfix, P, P, _).
match_op(xfx,   2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,   2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,   2, infix,   P, P, R) :- R is P - 1.


%!  indent(+Out, +Indent, +Options)
%
%   Newline and indent to the indicated  column. Respects the option
%   =tab_width=.  Default  is  8.  If  the  tab-width  equals  zero,
%   indentation is emitted using spaces.

indent(Out, Indent, Options) :-
    option(tab_width(TW), Options, 8),
    nl(Out),
    (   TW =:= 0
    ->  tab(Out, Indent)
    ;   Tabs is Indent // TW,
        Spaces is Indent mod TW,
        forall(between(1, Tabs, _), put(Out, 9)),
        tab(Out, Spaces)
    ).

%!  print_width(+Term, -W, +Options) is det.
%
%   Width required when printing `normally' left-to-right.

print_width(Term, W, Options) :-
    option(right_margin(RM), Options),
    option(write_options(WOpts), Options),
    (   catch(write_length(Term, W, [max_length(RM)|WOpts]),
              error(_,_), fail)      % silence uncaught exceptions from
    ->  true                         % nested portray callbacks
    ;   W = RM
    ).

%!  pprint(+Term, +Context, +Options)
%
%   The bottom-line print-routine.

pprint(Term, Ctx, Options) :-
    option(output(Out), Options),
    pprint(Out, Term, Ctx, Options).

pprint(Out, Term, Ctx, Options) :-
    option(write_options(WriteOptions), Options),
    context(Ctx, max_depth, MaxDepth),
    (   MaxDepth == infinite
    ->  write_term(Out, Term, WriteOptions)
    ;   MaxDepth =< 0
    ->  format(Out, '...', [])
    ;   write_term(Out, Term, [max_depth(MaxDepth)|WriteOptions])
    ).


		 /*******************************
		 *    SHARED WITH term_html.pl	*
		 *******************************/


%!  is_op1(+Name, -Type, -Priority, -ArgPriority, +Options) is semidet.
%
%   True if Name is an operator taking one argument of Type.

is_op1(Name, Type, Pri, ArgPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, OpType, Module:Name),
    argpri(OpType, Type, Pri, ArgPri),
    !.

argpri(fx, prefix,  Pri0, Pri) :- Pri is Pri0 - 1.
argpri(fy, prefix,  Pri,  Pri).
argpri(xf, postfix, Pri0, Pri) :- Pri is Pri0 - 1.
argpri(yf, postfix, Pri,  Pri).

%!  is_op2(+Name, -LeftPri, -Pri, -RightPri, +Options) is semidet.
%
%   True if Name is an operator taking two arguments of Type.

is_op2(Name, LeftPri, Pri, RightPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, Type, Module:Name),
    infix_argpri(Type, LeftPri, Pri, RightPri),
    !.

infix_argpri(xfx, ArgPri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(yfx, Pri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(xfy, ArgPri, Pri, Pri) :- ArgPri is Pri - 1.


%!  need_space(@Term1, @Term2, +LeftOptions, +RightOptions)
%
%   True if a space is  needed  between   Term1  and  Term2  if they are
%   printed using the given option lists.

need_space(T1, T2, _, _) :-
    (   is_solo(T1)
    ;   is_solo(T2)
    ),
    !,
    fail.
need_space(T1, T2, LeftOptions, RightOptions) :-
    end_code_type(T1, TypeR, LeftOptions.put(side, right)),
    end_code_type(T2, TypeL, RightOptions.put(side, left)),
    \+ no_space(TypeR, TypeL).

no_space(punct, _).
no_space(_, punct).
no_space(quote(R), quote(L)) :-
    !,
    R \== L.
no_space(alnum, symbol).
no_space(symbol, alnum).

%!  end_code_type(+Term, -Code, Options)
%
%   True when code is the first/last character code that is emitted
%   by printing Term using Options.

end_code_type(_, Type, Options) :-
    MaxDepth = Options.max_depth,
    integer(MaxDepth),
    Options.depth >= MaxDepth,
    !,
    Type = symbol.
end_code_type(Term, Type, Options) :-
    primitive(Term, _),
    !,
    quote_atomic(Term, S, Options),
    end_type(S, Type, Options).
end_code_type(Dict, Type, Options) :-
    is_dict(Dict, Tag),
    !,
    (   Options.side == left
    ->  end_code_type(Tag, Type, Options)
    ;   Type = punct
    ).
end_code_type('$VAR'(Var), Type, Options) :-
    Options.get(numbervars) == true,
    !,
    format(string(S), '~W', ['$VAR'(Var), [numbervars(true)]]),
    end_type(S, Type, Options).
end_code_type(List, Type, _) :-
    (   List == []
    ;   List = [_|_]
    ),
    !,
    Type = punct.
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 1),
    is_op1(Name, OpType, Pri, ArgPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   op_or_arg(OpType, Options.side, OpArg),
        (   OpArg == op
        ->  end_code_type(Name, Type, Options)
        ;   arg(1, OpTerm, Arg),
            arg_options(Options, ArgOptions),
            end_code_type(Arg, Type, ArgOptions.put(priority, ArgPri))
        )
    ).
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 2),
    is_op2(Name, LeftPri, Pri, _RightPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   arg(1, OpTerm, Arg),
        arg_options(Options, ArgOptions),
        end_code_type(Arg, Type, ArgOptions.put(priority, LeftPri))
    ).
end_code_type(Compound, Type, Options) :-
    compound_name_arity(Compound, Name, _),
    end_code_type(Name, Type, Options).

op_or_arg(prefix,  left,  op).
op_or_arg(prefix,  right, arg).
op_or_arg(postfix, left,  arg).
op_or_arg(postfix, right, op).



end_type(S, Type, Options) :-
    number(S),
    !,
    (   (S < 0 ; S == -0.0),
        Options.side == left
    ->  Type = symbol
    ;   Type = alnum
    ).
end_type(S, Type, Options) :-
    Options.side == left,
    !,
    sub_string(S, 0, 1, _, Start),
    syntax_type(Start, Type).
end_type(S, Type, _) :-
    sub_string(S, _, 1, 0, End),
    syntax_type(End, Type).

syntax_type("\"", quote(double)) :- !.
syntax_type("\'", quote(single)) :- !.
syntax_type("\`", quote(back))   :- !.
syntax_type(S, Type) :-
    string_code(1, S, C),
    (   code_type(C, prolog_identifier_continue)
    ->  Type = alnum
    ;   code_type(C, prolog_symbol)
    ->  Type = symbol
    ;   code_type(C, space)
    ->  Type = layout
    ;   Type = punct
    ).

is_solo(Var) :-
    var(Var), !, fail.
is_solo(',').
is_solo(';').
is_solo('!').

%!  primitive(+Term, -Class) is semidet.
%
%   True if Term is a primitive term, rendered using the CSS
%   class Class.

primitive(Term, Type) :- var(Term),      !, Type = 'pl-avar'.
primitive(Term, Type) :- atom(Term),     !, Type = 'pl-atom'.
primitive(Term, Type) :- string(Term),   !, Type = 'pl-string'.
primitive(Term, Type) :- integer(Term),  !, Type = 'pl-int'.
primitive(Term, Type) :- rational(Term), !, Type = 'pl-rational'.
primitive(Term, Type) :- float(Term),    !, Type = 'pl-float'.

%!  operator_module(-Module, +Options) is det.
%
%   Find the module for evaluating operators.

operator_module(Module, Options) :-
    Module = Options.get(module),
    !.
operator_module(TypeIn, _) :-
    '$current_typein_module'(TypeIn).

%!  arg_options(+Options, -OptionsOut) is det.
%
%   Increment depth in Options.

arg_options(Options, Options.put(depth, NewDepth)) :-
    NewDepth is Options.depth+1.

quote_atomic(Float, String, Options) :-
    float(Float),
    Format = Options.get(float_format),
    !,
    format(string(String), Format, [Float]).
quote_atomic(Plain, Plain, _) :-
    number(Plain),
    !.
quote_atomic(Plain, String, Options) :-
    Options.get(quoted) == true,
    !,
    (   Options.get(embrace) == never
    ->  format(string(String), '~q', [Plain])
    ;   format(string(String), '~W', [Plain, Options])
    ).
quote_atomic(Var, String, Options) :-
    var(Var),
    !,
    format(string(String), '~W', [Var, Options]).
quote_atomic(Plain, Plain, _).

space_op(:-).
