
:- use_module(library(fromonto)).
:- use_module(library(charsio)).
:- use_module(require).
:- use_module(pce_utils).
:- use_module(language(pce_messages)).

:- op(100, fx, @).
:- op(150, yfx, ?).
:- op(990, xfx, :=).
:- op(1200, xfx, :->).
:- op(1200, xfx, :<-).
:- op(1190, xfx, ::).
:- op(100,  xf,  *).
:- op(125,  xf,  ?).
:- op(150,  xf,  ...).
:- op(100,  xfx, ..).

:- dynamic compiling/1.


user:term_expansion(:-(require(Preds)),
	List ):-!,
	require:te_require_list(Preds, List).


user:(term_expansion(:-(pce_begin_class(ClassName, Super)), Expanded) :-
	term_expansion(:-(pce_begin_class(ClassName, Super, "")), Expanded)).

user:term_expansion(:-(pce_begin_class(ClassName, Super, Documentation)), 
	[ :-(initialization(pce_compile:pce_begin_class(Module:ClassName, 
				Super, Documentation))),
	  :-(initialization(
			(get(@classes, member, RealClass, Class),
			 prolog_load_context(file, File),
                         send(Class, Module:source, source_location(File,
					Line))
			 ))) ]
						):-!,
		prolog_load_context(module, Module),
		ClassName =.. [RealClass | _],
		asserta(compiling(RealClass)),
		source_location_term(source_location(_, Line)).

user:term_expansion(:-(pce_end_class),
	:-(initialization(pce_compile:pce_end_class))):-!,
		retractall(compiling(_)).

user:term_expansion(:-(pce_extend_class(ClassName)),
	:-(initialization(pce_compile:pce_extend_class(pce_boot:ClassName)))):-!,
		ClassName =.. [RealClass | _],
                asserta(compiling(RealClass)).

user:term_expansion(:-(pce_global(Reference, Goal)),
	:-(initialization((
			pce_global(Reference, Module:Goal)
		)))):-!,
	prolog_load_context(module, Module).

user:term_expansion(:-(pce_autoload(Module, File)),
	:-(initialization((
			pce_autoload(Module, File)
		)))):-!.

user:term_expansion(variable(Name, Type := Initial, Acs, Doc),
	:-(initialization((
		send(Class, Module:instance_variable,
			new(Var, variable(Name, PceType, Acs, string(Doc)))),
                send(Var, initial_value, Initial))))):-!,
	prolog_load_context(module, Module),
	access(Acs),
        current_class(Class),
        type(Type, PceType).

user:term_expansion(variable(Name, Type, Acs), 
	:-(initialization((
		send(Class, Module:instance_variable, 
				variable(Name,PceType,Acs))
		)))):-!,
	prolog_load_context(module, Module),
	access(Acs),
	current_class(Class),
	type(Type, PceType).


user:term_expansion(variable(Name, Type, Acs, Doc),
	:-(initialization((
		send(Class, Module:instance_variable, 
			variable(Name, PceType, Acs, string(Doc)))
		)))):-!,
	prolog_load_context(module, Module),
	access(Acs),
	current_class(Class),
	type(Type, PceType).


user:term_expansion(resource(Name, Type, Def),
	:-(initialization((
			send(Class, Module:resource, 
				resource(Name, @default, PceType, 
					PceDef, Class))
		)))):-!,
	prolog_load_context(module, Module),
	current_class(Class),
	to_atom(Def, PceDef),
	type(Type, PceType).


user:term_expansion(resource(Name, Type, Def, Doc),
	:-(initialization((
			send(Class, Module:resource, 
				resource(Name, @default, PceType, 
					PceDef, Class, string(Doc)))
		)))):-!,
	prolog_load_context(module, Module),
	current_class(Class),
	to_atom(Def, PceDef),
	type(Type, PceType).



user:term_expansion(handle(X, Y, Kind),
	:-(initialization((
			send(Class, Module:handle, handle(X, Y, Kind))
		)))):-!,
	prolog_load_context(module, Module),
	current_class(Class).


user:term_expansion(handle(X, Y, Kind, Name),
	:-(initialization((
			send(Class, Module:handle, handle(X, Y, Kind, Name))
		)))):-!,
	prolog_load_context(module, Module),
	current_class(Class).


user:term_expansion(delegate_to(VarName),
	:-(initialization((
			send(Class, Module:delegate, VarName)
		)))):-!,
	prolog_load_context(module, Module),
	current_class(Class).


user:term_expansion((Head :-> DocBody),
	[  :-(initialization((
			send(Class, Module:send_method,
				send_method(Selector, Types, Cascade, 
						Doc, Loc))
		))),
	  (PlHead :- Body)	 ]):-!,
		(   DocBody = (DocText::Body)
		->  Doc = string(DocText)
		;   DocBody = Body,
		    Doc = @nil
		),
		prolog_load_context(module, Module),
		source_location_term(Loc),
		current_class(Class),
		prolog_head(send, Head, Selector, Types, PlHead, Cascade).



user:term_expansion((Head :<- DocBody),
	[  :-(initialization((
			send(Class, Module:get_method,
				get_method(Selector, RType, Types, Cascade, Doc, Loc))
		))),
	  (PlHead :- Body) 	]):-!,
		(   DocBody = (DocText::Body)
		->  Doc = string(DocText)
		;   DocBody = Body,
		    Doc = @nil
		),
		prolog_load_context(module, Module),
		source_location_term(Loc),
		current_class(Class),
		return_type(Head, RType),
		prolog_head(get, Head, Selector, Types, PlHead, Cascade).

user:term_expansion(:-(new(Obj, Class)),
	:-(initialization(new(Obj, Class)))):-!.

user:term_expansion(:-(send(Obj, Sel)),
        :-(initialization(send(Obj, Sel)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg)),
        :-(initialization(send(Obj, Sel, Arg)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, 
					Arg8, Arg9)))):-!.

user:term_expansion(:-(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Arg9, Arg10)),
        :-(initialization(send(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Arg9, Arg10)))):-!.


user:term_expansion(:-(get(Obj, Sel, Out)),
        :-(initialization(get(Obj, Sel, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg, Out)),
        :-(initialization(get(Obj, Sel, Arg, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Arg9, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, 
					Arg8, Arg9, Out)))):-!.

user:term_expansion(:-(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Arg9, Arg10, Out)),
        :-(initialization(get(Obj, Sel, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7,
					 Arg8, Arg9, Arg10, Out)))):-!.

user:term_expansion(:-(free(Obj)), :-(initialization(free(Obj)))) :-!.

user:term_expansion(:-(object(Obj)), :-(initialization(object(Obj)))) :-!.

user:term_expansion(:-(object(A,B)), :-(initialization(object(A,B)))) :-!.

user:term_expansion(:-(pce_group(GroupName)), 
	:-(initialization(pce_group(GroupName)))):-!.

		 /*******************************
		 *	    HELP SYSTEM		*
		 *******************************/

user:term_expansion(:-(pce_help_file(DataBase,File)),
	:-(initialization(pce_help_file(DataBase,File)))) :-!.

		 /*******************************
		 *	      PCEDRAW		*
		 *******************************/

user:term_expansion(:-(draw_begin_shape(Name,Super,Summ,Recog)),
		    [ :-(initialization((new(_,draw_shape_class(Name,Super)))))
		    | Expansion
		    ]) :-
	user:term_expansion((:-pce_begin_class(Name, Super, Summ)),
			    BeginClassExpansion),
	append(BeginClassExpansion,
	       [ (:- initialization(draw_shapes:associate_recognisers(Recog)))
	       ],
	       Expansion).

user:(term_expansion(:-(draw_end_shape), Expanded) :-
      	term_expansion(:-(pce_end_class), Expanded)).

		 /*******************************
		 *	      PCEEMACS		*
		 *******************************/

user:term_expansion(:-(declare_emacs_mode(Mode,File)),
	:-(initialization(declare_emacs_mode(Mode,File)))) :-!.

user:term_expansion(:-(emacs_begin_mode(Mode,Super,Summ,Bind,Syntax)),
		    Expansion) :-
	concat_atom([emacs_, Mode, '_mode'], Class),
	concat_atom([emacs_, Super, '_mode'], SuperClass),
	user:term_expansion((:- pce_begin_class(Class, SuperClass, Summ)),
			    BeginClassExpansion),
	append(BeginClassExpansion,
	       [ (:- initialization(emacs_extend:emacs_mode_bindings(Mode,
								     Bind,
								     Syntax)))
	       ],
	       Expansion).

user:term_expansion(:-(emacs_extend_mode(Mode, Bind)), Expansion) :-
	concat_atom([emacs_, Mode, '_mode'], Class),
	user:term_expansion((:- pce_extend_class(Class)),
			    ExtendClassExpansion),
	append(ExtendClassExpansion,
	       [ (:- initialization(emacs_extend:emacs_mode_bindings(Mode,
								     Bind,
								     [])))
	       ],
	       Expansion).
	
user:(term_expansion(:-(emacs_end_mode), Expanded) :-
      	term_expansion(:-(pce_end_class), Expanded)).

user:term_expansion(:-(pce_host:'$load_pce'), :-(true)) :-!.
user:term_expansion(:-(push_compile_operators), :-(true)) :-!.
user:term_expansion(:-(pce_compile:push_compile_operators), :-(true)) :-!.
user:term_expansion(:-(pop_compile_operators), :-(true)) :-!.
user:term_expansion(:-(pce_compile:pop_compile_operators), :-(true)) :-!.


user:term_expansion(pce_ifhostproperty(Prop, Clause), TheClause) :-
        (   property(Prop)
        ->  TheClause = Clause
        ;   TheClause = []
        ).
user:term_expansion(pce_ifhostproperty(Prop, Then, Else), TheClause) :-
        (   property(Prop)
        ->  TheClause = Then
        ;   TheClause = Else
        ).


property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).


prolog_head(SendGet, Head, Selector, TypeVector, PlHead, Cascade) :-
        Head =.. [Selector, Receiver | Args],
        predicate_name(SendGet, Selector, PredName),
        pl_head_args(SendGet, Args, Types, PlArgs, FArgs),
        create_type_vector(Types, TypeVector),
        PlHead =.. [PredName, Receiver | PlArgs],
        (   SendGet == send
        ->  Class = message
        ;   Class = ?
        ),
        Cascade =.. [Class, @prolog, call, PredName, @receiver | FArgs].

create_type_vector([],      @default) :- !.
create_type_vector(List,    new(VectorTerm)) :-
        VectorTerm =.. [vector|List].


predicate_name(SendGet, Selector, Name) :-
	compiling(ClassName),
        concat_atom([SendGet, '_', Selector, '_', ClassName], Name).


pl_head_args(SendGet, Args, Types, PlArgs, FArgs) :-
        pl_head_args(SendGet, Args, 1, Types, PlArgs, FArgs).

pl_head_args(send, [], _, [], [], []) :- !.
pl_head_args(get,  [Return], _, [], [ReturnVar], []) :- !,
        (   var(Return)
        ->  ReturnVar = Return
        ;   Return = ReturnVar:_Type
        ).
pl_head_args(SG, [ArgAndType|RA], AN, [T|RT], [Arg|TA], [@ArgN|MArgs]) :- !,
        head_arg(ArgAndType, Arg, Type),
        type(Type, T),
        concat(arg, AN, ArgN),
        ANN is AN + 1,
        pl_head_args(SG, RA, ANN, RT, TA, MArgs).


head_arg(Var, Var, any) :-
        var(Var), !.
head_arg(Arg:Type, Arg, Type).





concat_atom([A,B], Atom) :- !,
        concat(A, B, Atom).
concat_atom(List, Atom) :-
        atoms_to_string(List, "", String),
        name(Atom, String).

atoms_to_string([], L, L).
atoms_to_string([H|T], Sofar, String) :-
        name(H, S),
        append(Sofar, S, NewSofar),
        atoms_to_string(T, NewSofar, String).


%       concat(?Atom1, ?Atom2, ?Atom3)
%       Logical concatenation of two atomics

concat(A, B, C) :-
        atomic(A), atomic(B), !,
        name(A, S0),
        name(B, S1),
        append(S0, S1, S),
        name(C, S).
concat(A, B, C) :-
        atomic(A), atomic(C), !,
        name(A, S0),
        name(C, S),
        append(S0, S1, S),
        name(B, S1).
concat(A, B, C) :-
        atomic(B), atomic(C), !,
        name(B, S1),
        name(C, S),
        append(S0, S1, S),
        name(A, S0).



type(Prolog, Pce) :-
        to_atom(Prolog, RawPce),
        canonise_type(RawPce, Pce).

canonise_type(T0, T0) :-
        concat(_, ' ...', T0), !.
canonise_type(T0, T) :-
        concat(T1, '...', T0), !,
        concat(T1, ' ...', T).
canonise_type(T, T).

to_atom(Atom, Atom) :-
        atom(Atom), !.
to_atom(Term, Atom) :-
        term_to_atom(Term, RawAtom),
%        new(S, string(RawAtom)),
%        send(S, translate, 0' , @nil),
%        get(S, value, Atom).
	Atom = RawAtom.



current_class(ClassName):-
	compiling(Class),
	concat_atom([Class, '_class'], ClassNameAtom),
	ClassName = @ClassNameAtom.


return_type(Term, RType) :-
        functor(Term, _, Arity),
        arg(Arity, Term, Last),
        (   nonvar(Last),
            Last = _:Type
        ->  type(Type, RType)
        ;   RType = @default
        ).


access(none).
access(get).
access(send).
access(both).


source_location_term(source_location(File, Line)) :-
        source_location(File, Line), !.
source_location_term(@nil).

set_source_location(Obj) :-
        source_location_term(Loc),
        send(Obj, source, Loc).


print_source_location:-
	prolog_load_context(file, File),
        prolog_load_context(term_position,
                '$stream_position'(_, Line, _, _, _)),
	write('**** File '),
	write(File),
	write(' Line '),
	write(Line),
	nl.
