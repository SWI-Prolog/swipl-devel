/*  $Id$

    Part of JPL -- SWI-Prolog/Java interface

    Author:	   Paul Singleton, Fred Dushin and Jan Wielemaker
    E-mail:	   paul@jbgb.com
    WWW:	   http://www.swi-prolog.org
    Copyright (C): 1985-2004, Paul Singleton

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

:- module(jpl,
	  [ jpl_pl_lib_version/1,
	    jpl_c_lib_version/1,
	    jpl_new/3,
	    jpl_call/4,
	    jpl_get/3,
	    jpl_set/3,
	    jpl_servlet_byref/3,
	    jpl_servlet_byval/3,
	    jpl_class_to_classname/2,
	    jpl_class_to_type/2,
	    jpl_classname_to_class/2,
	    jpl_classname_to_type/2,
	    jpl_datum_to_type/2,
	    jpl_false/1,
	    jpl_is_class/1,
	    jpl_is_false/1,
	    jpl_is_null/1,
	    jpl_is_object/1,
	    jpl_is_object_type/1,
	    jpl_is_ref/1,
	    jpl_is_true/1,
	    jpl_is_type/1,
	    jpl_is_void/1,
	    jpl_null/1,
	    jpl_object_to_class/2,
	    jpl_object_to_type/2,
	    jpl_primitive_type/1,
	    jpl_ref_to_type/2,
	    jpl_true/1,
	    jpl_type_to_class/2,
	    jpl_type_to_classname/2,
	    jpl_void/1,
	    jpl_array_to_length/2,
	    jpl_array_to_list/2,
	    jpl_datums_to_array/2,
	    jpl_enumeration_element/2,
	    jpl_enumeration_to_list/2,
	    jpl_hashtable_pair/2,
	    jpl_iterator_element/2,
	    jpl_list_to_array/2,
	    jpl_map_element/2,
	    jpl_set_element/2
	  ]).
:- use_module(library(lists)).
% suppress debugging this library
:- set_prolog_flag(generate_debug_info, false).

%------------------------------------------------------------------------------

%   term_to_chars(+Term, ?Chars)
%   unifies Chars with a printed representation of Term.  It is not at all
%   clear what representation should be used.  On the grounds that it is
%   supposed to be re-readable, I have chosen to use write_canonical.
%   Giving Chars to chars_to_term/2 will yield a *copy* of Term.

term_to_chars(Term, Chars) :-
  % start_output_to_handle(Handle, Stream),
  % write_canonical(Stream, Term),
  % close(Stream),
  % byte_stream_to_chars(Handle, Chars0),
  % free_byte_stream(Handle),
  % Chars = Chars0.
    (	atom(Term)
    ->	Term = A		% avoid superfluous quotes
    ;	system:term_to_atom(Term, A)	    % port
    ),
    atom_codes(A, Chars).

%------------------------------------------------------------------------------

% jpl_call(+X, +Mspec, +Args, -R) :-
%   X can be:
%     a type, class object or classname
%	(for static methods of the denoted class,
%	 or for static or instance methods of java.lang.Class)
%     a class instance or array
%	(for static or instance methods)
%
%   Mspec can be:
%     an atomic method name
%	(may involve dynamic overload resolution)
%     an integral method index
%	(untested: for static overload resolution)
%     a methodID/1 structure
%	(ditto)
%
%   Args must be a proper list (poss empty) of ground arguments;
%
%   finally, an attempt will be made to unify R with the returned result 

jpl_call(X, Mspec, Args, R) :-
    (	ground(X)
    ->	(   jpl_is_object(X)		    % ie class(_,_) or array(_) instance
	->  Obj = X,
	    jpl_object_to_type(Obj, Type)
	;   (X=class(_,_) ; X=array(_) )   % ie an object type
	->  jpl_type_to_class(X, Obj),
	    Type = class([java,lang],['Class'])
	;   jpl_classname_to_type(X, Tx)   % eg 'java.lang.String' or '[L'
	->  jpl_type_to_class(Tx, Obj),
	    Type = class([java,lang],['Class'])
	;   throw(error(type_error(jpl_receiver, X),
			context(jpl_call/4,
				'must be type, class object or class name')))
	)
    ;	var(X)
    ->	throw(error(instantiation_error,
		    context(jpl_call/4, _)))
    ;	throw(error(type_error(jpl_receiver, X),
		    context(jpl_call/4,
			    'must be type, class object or class name')))
    ),
    ground(Args),			    % check in jpl_call_1/5 instead?
    jpl_call_1(Type, Obj, Mspec, Args, Rx),

    R = Rx.

%------------------------------------------------------------------------------

% jpl_call_1(+Type, +Object, +MethodSpec, +Argz, -Result) :-
%   serves only jpl_call/4
%   Object may be a class object, in which case Type is class([java,lang],['Class'])
%   Argz (certain to be ground) can be:
%     a proper list of arguments Args
%	(their qty and types help resolve the method)
%     a single argument Arg
%	(defaulty corrected to Args = [Arg])
%
%   at call, Result is unbound
%   at exit, Result is bound to the returned result datum (or @void)

jpl_call_1(Tx, X, Mspec, Argz, Rx) :-	    % a static method call?
    Tx = class([java,lang],['Class']),	    % a class object...
    jpl_class_to_type(X, Tx2),
    Tx2 \== class([java,lang],['Class']),   % ...other than java.lang.Class
    !,
    (	integer(Mspec)		    % i.e. a method index
    ->	I = Mspec
    ;	atom(Mspec)		    % i.e. a method name
    ->	N = Mspec
    ;	jpl_is_methodID(Mspec)	    % i.e. a method ID
    ->	MID = Mspec
    ),
    (	is_list(Argz)
    ->	Args = Argz,
	length(Args, A)
    ;	Args = [Argz],		    % ugh: defaulty (?) single arg
	A = 1
    ),
  % consider all methods of java.lang.Class
  % and static methods of the denoted class
    findall(
	z8(T,I,N,A,Mods,MID,Tr,Tps),
	(   T = Tx,
	    jpl_method_spec(T, I, N, A, Mods, MID, Tr, Tps)
	;   T = Tx2,
	    jpl_method_spec(T, I, N, A, Mods, MID, Tr, Tps),
	    member(static, Mods)
	),
	Z8s
    ),
    (	Z8s = []
    ->	write('Warning: no arity-matching methods'), nl, fail
    ;	Z8s = [z8(T,I,N,A,Mods,MID,Tr,Tps)]
    ->	true					% exactly one method
    ;	jpl_datums_to_types(Args, Tas),	% most specific types
	findall(
	    z5(I,Mods,MID,Tr,Tps),
	    (	member(z8(T,I,N,A,Mods,MID,Tr,Tps), Z8s),
		jpl_types_fit_types(Tas, Tps)	% assignability test
	    ),
	    Z5sA				% Args-assignable methods
	),
	(   Z5sA == []
	->  write('Warning: no type-assignable methods'), nl, fail
	;   Z5sA = [z5(I,Mods,MID,Tr,Tps)]
	->  true		% exactly one applicable method
	;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tps))
	->  true		% exactly one most-specific applicable method
	;   write('Error: no single most-specific method'), nl, fail
	)
    ),
    (	T = Tx
    ->	jpl_type_to_class(Tx, Cc),
	(   member(static, Mods)
	->  jpl_call_static(Tr, Cc, MID, Tps, Args, Rx)
	;   jpl_call_instance(Tr, Cc, MID, Tps, Args, Rx)
	)
    ;	T = Tx2
    ->	jpl_call_static(Tr, X, MID, Tps, Args, Rx)
    ).

jpl_call_1(Tx, X, Mspec, Argz, Rx) :-	% regular instance object,
    (	Tx = class(_,_)
    ;	Tx = array(_)
    ),
    !,					% or instance of java.lang.Class
    (	integer(Mspec)			% i.e. a method index
    ->	I = Mspec
    ;	atom(Mspec)			% i.e. a method name
    ->	N = Mspec
    ;	jpl_is_methodID(Mspec)		% i.e. a method ID
    ->	MID = Mspec
    ),
    (	is_list(Argz)
    ->	Args = Argz,
	length(Args, A)
    ;	Args = [Argz],
	A = 1
    ),
  % Tx = class(Ps,Cs),
    findall(
	z5(I,Mods,MID,Tr,Tps),
	jpl_method_spec(Tx, I, N, A, Mods, MID, Tr, Tps),
	Z5s
    ),
    (	Z5s = []
    ->	write('Warning: no arity-matching methods'), nl, fail
    ;	Z5s = [z5(I,Mods,MID,Tr,Tps)]
    ->	true					% exactly one static method
    ;	jpl_datums_to_types(Args, Tas),	% most specific types
	findall(
	    z5(I,Mods,MID,Tr,Tps),		% those to which Args is assignable
	    (	member(z5(I,Mods,MID,Tr,Tps), Z5s),
		jpl_types_fit_types(Tas, Tps)	% assignability test
	    ),
	    Z5sA				% Args-assignable methods
	),
	(   Z5sA == []
	->  write('Warning: no type-assignable methods'), nl, fail
	;   Z5sA = [z5(I,Mods,MID,Tr,Tps)]
	->  true				% exactly one applicable method
	;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tps))
	->  true				% exactly one most-specific applicable method
	;   write('Error: no single most-specific method'), nl, fail
	)
    ),
    (	member(static, Mods)
    ->	jpl_object_to_class(X, Cx),
	jpl_call_static(Tr, Cx, MID, Tps, Args, Rx)
    ;	jpl_call_instance(Tr, X, MID, Tps, Args, Rx)
    ).

%------------------------------------------------------------------------------

jpl_call_instance(void, Class, MID, Ts, As, R) :-
    jCallVoidMethod(Class, MID, Ts, As),
    R = @(void).

jpl_call_instance(boolean, Class, MID, Ts, As, R) :-
    jCallBooleanMethod(Class, MID, Ts, As, R).

jpl_call_instance(byte, Class, MID, Ts, As, R) :-
    jCallByteMethod(Class, MID, Ts, As, R).

jpl_call_instance(char, Class, MID, Ts, As, R) :-
    jCallCharMethod(Class, MID, Ts, As, R).

jpl_call_instance(short, Class, MID, Ts, As, R) :-
    jCallShortMethod(Class, MID, Ts, As, R).

jpl_call_instance(int, Class, MID, Ts, As, R) :-
    jCallIntMethod(Class, MID, Ts, As, R).

jpl_call_instance(long, Class, MID, Ts, As, R) :-
    jCallLongMethod(Class, MID, Ts, As, R).

jpl_call_instance(float, Class, MID, Ts, As, R) :-
    jCallFloatMethod(Class, MID, Ts, As, R).

jpl_call_instance(double, Class, MID, Ts, As, R) :-
    jCallDoubleMethod(Class, MID, Ts, As, R).

jpl_call_instance(array(_), Class, MID, Ts, As, R) :-
    jCallObjectMethod(Class, MID, Ts, As, R).

jpl_call_instance(class(_,_), Class, MID, Ts, As, R) :-
    jCallObjectMethod(Class, MID, Ts, As, R).

%------------------------------------------------------------------------------

jpl_call_static(void, Class, MID, Ts, As, R) :-
    jCallStaticVoidMethod(Class, MID, Ts, As),
    R = @(void).

jpl_call_static(boolean, Class, MID, Ts, As, R) :-
    jCallStaticBooleanMethod(Class, MID, Ts, As, R).

jpl_call_static(byte, Class, MID, Ts, As, R) :-
    jCallStaticByteMethod(Class, MID, Ts, As, R).

jpl_call_static(char, Class, MID, Ts, As, R) :-
    jCallStaticCharMethod(Class, MID, Ts, As, R).

jpl_call_static(short, Class, MID, Ts, As, R) :-
    jCallStaticShortMethod(Class, MID, Ts, As, R).

jpl_call_static(int, Class, MID, Ts, As, R) :-
    jCallStaticIntMethod(Class, MID, Ts, As, R).

jpl_call_static(long, Class, MID, Ts, As, R) :-
    jCallStaticLongMethod(Class, MID, Ts, As, R).

jpl_call_static(float, Class, MID, Ts, As, R) :-
    jCallStaticFloatMethod(Class, MID, Ts, As, R).

jpl_call_static(double, Class, MID, Ts, As, R) :-
    jCallStaticDoubleMethod(Class, MID, Ts, As, R).

jpl_call_static(array(_), Class, MID, Ts, As, R) :-
    jCallStaticObjectMethod(Class, MID, Ts, As, R).

jpl_call_static(class(_,_), Class, MID, Ts, As, R) :-
    jCallStaticObjectMethod(Class, MID, Ts, As, R).

%------------------------------------------------------------------------------

%type	jpl_fergus_find_candidate(list(T), T, T, list(T))

jpl_fergus_find_candidate([], Candidate, Candidate, []).

jpl_fergus_find_candidate([X|Xs], Candidate0, Candidate, Rest) :-
    (	jpl_fergus_greater(X, Candidate0)
    ->	Candidate1 = X,
	Rest = [Candidate0|Rest1]
    ;	Candidate1 = Candidate0,
	Rest = [X|Rest1]
    ),
    jpl_fergus_find_candidate(Xs, Candidate1, Candidate, Rest1).

%------------------------------------------------------------------------------

jpl_fergus_greater(z5(_,_,_,_,Tps1), z5(_,_,_,_,Tps2)) :-
    jpl_types_fit_types(Tps1, Tps2).
jpl_fergus_greater(z3(_,_,Tps1), z3(_,_,Tps2)) :-
    jpl_types_fit_types(Tps1, Tps2).

%------------------------------------------------------------------------------

%type	jpl_fergus_is_the_greatest(list(T), T)

% jpl_fergus_is_the_greatest(Xs, GreatestX) :-
%   Xs is a list of things for which jpl_fergus_greater/2 defines a partial ordering;
%   GreatestX is one of those, than which none is greater;
%   fails if there is more than one such;
%   this algorithm was contributed to c.l.p by Fergus Henderson in response to my
%   "there must be a better way" challenge: there was, this is it

jpl_fergus_is_the_greatest([X|Xs], Greatest) :-
    jpl_fergus_find_candidate(Xs, X, Greatest, Rest),
    forall(member(R, Rest),
	   jpl_fergus_greater(Greatest, R)).

%------------------------------------------------------------------------------

% jpl_get(+X, +Fspec, -V) :-
%   X can be:
%     a class object, a classname, or an (object or array) type
%	(for static fields, or java.lang.Class fields);
%     a class instance
%	(for non-static fields)
%     an array
%	(for 'length' pseudo field, or indexed element retrieval),
%   but not:
%     a String
%	(clashes with class name; anyway, String has no fields to retrieve)
%
%   Fspec can be an atomic field name,
%	or an integral field index,
%	or a fieldID/1
%	or a variable (field names, or array indices, are generated
%	or a pair I-J of integers or variables (array subranges are generated)
%
%   finally, an attempt will be made to unify V with the retrieved value

jpl_get(X, Fspec, V) :-
    (	jpl_is_object(X)	% i.e. instance of class(_,_) or array(_) type
    ->	jpl_object_to_type(X, Type),
	Obj = X
    ;	jpl_is_type(X)		% e.g. class([java,lang],['String']), array(int)
    ->	jpl_type_to_class(X, Obj),
	Type = class([java,lang],['Class'])
    ;	atom(X)		% e.g. 'java.lang.String', '[L'
    ->	jpl_classname_to_type(X, Tx),
	jpl_type_to_class(Tx, Obj),
	Type = class([java,lang],['Class'])
    ),
    jpl_get_1(Type, Obj, Fspec, Vx),
    V = Vx.

%------------------------------------------------------------------------------

% jpl_get_1(+Type, +Object, +FieldSpec, -ResultValue) :-
%   Object may be a class object, in which case Type is class([java,lang],['Class'])
%   ResultValue (Vx below) is guaranteed unbound on entry,
%   and will, before exit, be unified with the retrieved value

jpl_get_1(Tx, X, Fspec, Vx) :-		% static field?
    Tx = class([java,lang],['Class']),	% a class object...
    jpl_class_to_type(X, Tx2),
    Tx2 \== Tx,			% ...other than java.lang.Class itself
    !,
    (	integer(Fspec)		% assume it's a field or array index
    ->	I = Fspec
    ;	atom(Fspec)		% assume it's a field name
    ->	N = Fspec
    ;	jpl_is_fieldID(Fspec)	% assume it's a JNI field ID
    ->	FID = Fspec
    ;	var(Fspec)		% experimentally...
    ->	N = Fspec		% we'll generate field names
    ),
  % consider static and instance fields of java.lang.Class
  % and static fields of the denoted class
    findall(
	z2(T,I,N,Mods,FID,Tf),
	(   T = Tx,
	    jpl_field_spec(T, I, N, Mods, FID, Tf)
	;   T = Tx2,
	    jpl_field_spec(T, I, N, Mods, FID, Tf),
	    member(static, Mods)
	),
	Z2s
    ),
    (	Z2s = [],
	nonvar(Fspec)
    ->	write('Warning: no matching fields'), nl, fail
    ;	member(z2(T,I,N,Mods,FID,Tf), Z2s),	% generate one or many
	(   T = Tx
	->  jpl_type_to_class(Tx, Cc),
	    (	member(static, Mods)
	    ->	jpl_get_static_field(Tf, Cc, FID, Vx)
	    ;	jpl_get_field(Tf, Cc, FID, Vx)
	    )
	;   T = Tx2
	->  jpl_get_static_field(Tf, X, FID, Vx)
	)
    ).

jpl_get_1(Tx, X, Fspec, Vx) :-
    Tx = class(_,_),
    !,
    (	integer(Fspec)		    % assume it's a field or array index
    ->	I = Fspec
    ;	atom(Fspec)		    % assume it's a field name
    ->	N = Fspec
    ;	jpl_is_fieldID(Fspec)	    % assume it's a JNI field ID (???)
    ->	FID = Fspec
    ;	var(Fspec)
    ->	N = Fspec		    % we'll generate field names
    ),
  % instance object, or the java.lang.Class object itself
    findall(z(I,N,Mods,FID,Tf),
	jpl_field_spec(Tx, I, N, Mods, FID, Tf),
	Zs
    ),
    (	Zs = []
    ->	write('Warning: no matching fields'), nl, fail
    ;	member(z(I,N,Mods,FID,Tf), Zs),    % generate one or many
	(   member(static, Mods)
	->  jpl_object_to_class(X, Cx),
	    jpl_get_static_field(Tf, Cx, FID, Vx)
	;   jpl_get_field(Tf, X, FID, Vx)
	)   
    ).

jpl_get_1(array(T), X, Fspec, Vx) :-
    (	integer(Fspec),
	Fspec >= 0		    % leave HiBound check to Java...
    ->	jpl_get_array_element(T, X, Fspec, Vx)
    ;	Fspec == length		    % special-case this solitary array "method"
    ->	jGetArrayLength(X, Len),
	Vx = Len
    ;	var(Fspec)
    ->	jGetArrayLength(X, Len),   % generate length field and elements
	Max is Len-1,
	(   Fspec = length,
	    Vx = Len
	;   between(0, Max, Fspec),
	    jpl_get_array_element(T, X, Fspec, Vx)
	)
    ;	Fspec = N-M,
	(integer(N) ; var(N) ),
	(integer(M) ; var(M) )
    ->	jGetArrayLength(X, Len),
	Max is Len-1,
	between(0, Max, N),
      % Mmin is N-1,		    % generates many empty sublists
	Mmin is N,		    % generates no empty sublists
				    % you want _one_ empty sublist? which one? ...
	between(Mmin, Max, M),
	jpl_get_array_elements(T, X, N, M, Vx)
    ).

%------------------------------------------------------------------------------

% jpl_get_array_element(+JspType, +Obj, +N, -Vc) :-
%   Obj is an array of JspType;
%   Vc is (unified with a JPL repn of) its Nth element (numbered from 0)
%   NB the nonsense with Xhi and Xlo should be expunged:
%   values should be converted to terms within foreign code

jpl_get_array_element(T, Obj, N, Vc) :-
    (	(   T = class(_,_)
	;   T = array(_)
	)
    ->	jGetObjectArrayElement(Obj, N, Vr)
    ;	jpl_primitive_type(T)
    ->	jni_type_to_xput_code(T, Xc),
	jni_alloc_buffer(Xc, 1, Bp),			% one-element buf for a T
	jpl_get_primitive_array_region(T, Obj, N, 1, Bp),
	jni_fetch_buffer_value(Bp, 0, Xhi, Xlo, Xc),	% zero-th element
	jni_convert_primitive_in(T, Vr, Xhi, Xlo),
	jni_free_buffer(Bp)
    ),
    Vr = Vc.

%------------------------------------------------------------------------------

% jpl_get_array_elements(+Type, +Object, +N, +M, -Rs) :-
%   serves only jpl_get_1
%   Rs will always be unbound on entry

jpl_get_array_elements(T, Obj, N, M, Rs) :-
    (	(   T = class(_,_)
	;   T = array(_)
	)
    ->	jpl_get_object_array_elements(Obj, N, M, Rs)
    ;	jpl_get_primitive_array_elements(T, Obj, N, M, Rs)
    ).

%------------------------------------------------------------------------------

jpl_get_field(boolean, Obj, FieldID, R) :-
    jGetBooleanField(Obj, FieldID, R).

jpl_get_field(byte, Obj, FieldID, R) :-
    jGetByteField(Obj, FieldID, R).

jpl_get_field(char, Obj, FieldID, R) :-
    jGetCharField(Obj, FieldID, R).

jpl_get_field(short, Obj, FieldID, R) :-
    jGetShortField(Obj, FieldID, R).

jpl_get_field(int, Obj, FieldID, R) :-
    jGetIntField(Obj, FieldID, R).

jpl_get_field(long, Obj, FieldID, R) :-
    jGetLongField(Obj, FieldID, R).

jpl_get_field(float, Obj, FieldID, R) :-
    jGetFloatField(Obj, FieldID, R).

jpl_get_field(double, Obj, FieldID, R) :-
    jGetDoubleField(Obj, FieldID, R).

jpl_get_field(class(_,_), Obj, FieldID, R) :-
    jGetObjectField(Obj, FieldID, R).

jpl_get_field(array(_), Obj, FieldID, R) :-
    jGetObjectField(Obj, FieldID, R).

%------------------------------------------------------------------------------

% jpl_get_object_array_elements(+Type, +Object, +N, +M, -Vcs) :-
%   Type is some object or array type;
%   Object should be a (zero-based) array of element-type Type;
%   N is an integer, 0 =< N < length(Object);
%   M is an integer, N-1 =< M < length(Object);
%   at call, Vcs will be unbound;
%   at exit, Vcs will be a list of refs to the array's elements from index N to M inclusive

jpl_get_object_array_elements(Obj, N, M, Vcs) :-
    (	N =< M
    ->	Vcs = [Vc|Vcs2],
	jGetObjectArrayElement(Obj, N, Vc),
	Nx is N+1,
	jpl_get_object_array_elements(Obj, Nx, M, Vcs2)
    ;	Vcs = []
    ).

%------------------------------------------------------------------------------

% jpl_get_primitive_array_elements(+JspType, +Object, +N, +M, -Vcs) :-
%   Object should be a (zero-based) Java array of (primitive) JspType;
%   Vcs will be unbound on entry,
%   and on exit will be a list of the elements from index N to M inclusive

jpl_get_primitive_array_elements(T, Obj, N, M, Vcs) :-
    Size is M-N+1,
    (	Size == 0
    ->	Vcs = []
    ;	jni_type_to_xput_code(T, Xc),
	jni_alloc_buffer(Xc, Size, Bp),
	jpl_get_primitive_array_region(T, Obj, N, Size, Bp),
	jpl_primitive_buffer_to_array(T, Xc, Bp, 0, Size, Vcs),
	jni_free_buffer(Bp)
    ).

%------------------------------------------------------------------------------

jpl_get_primitive_array_region(boolean, Obj, N, S, I) :-
    jGetBooleanArrayRegion(Obj, N, S, jbuf(I,boolean)).

jpl_get_primitive_array_region(byte, Obj, N, S, I) :-
    jGetByteArrayRegion(Obj, N, S, jbuf(I,byte)).

jpl_get_primitive_array_region(char, Obj, N, S, I) :-
    jGetCharArrayRegion(Obj, N, S, jbuf(I,char)).

jpl_get_primitive_array_region(short, Obj, N, S, I) :-
    jGetShortArrayRegion(Obj, N, S, jbuf(I,short)).

jpl_get_primitive_array_region(int, Obj, N, S, I) :-
    jGetIntArrayRegion(Obj, N, S, jbuf(I,int)).

jpl_get_primitive_array_region(long, Obj, N, S, I) :-
    jGetLongArrayRegion(Obj, N, S, jbuf(I,long)).

jpl_get_primitive_array_region(float, Obj, N, S, I) :-
    jGetFloatArrayRegion(Obj, N, S, jbuf(I,float)).

jpl_get_primitive_array_region(double, Obj, N, S, I) :-
    jGetDoubleArrayRegion(Obj, N, S, jbuf(I,double)).

%------------------------------------------------------------------------------

jpl_get_static_field(boolean, Obj, FieldID, R) :-
    jGetStaticBooleanField(Obj, FieldID, R).

jpl_get_static_field(byte, Obj, FieldID, R) :-
    jGetStaticByteField(Obj, FieldID, R).

jpl_get_static_field(char, Obj, FieldID, R) :-
    jGetStaticCharField(Obj, FieldID, R).

jpl_get_static_field(short, Obj, FieldID, R) :-
    jGetStaticShortField(Obj, FieldID, R).

jpl_get_static_field(int, Obj, FieldID, R) :-
    jGetStaticIntField(Obj, FieldID, R).

jpl_get_static_field(long, Obj, FieldID, R) :-
    jGetStaticLongField(Obj, FieldID, R).

jpl_get_static_field(float, Obj, FieldID, R) :-
    jGetStaticFloatField(Obj, FieldID, R).

jpl_get_static_field(double, Obj, FieldID, R) :-
    jGetStaticDoubleField(Obj, FieldID, R).

jpl_get_static_field(class(_,_), Obj, FieldID, R) :-
    jGetStaticObjectField(Obj, FieldID, R).

jpl_get_static_field(array(_), Obj, FieldID, R) :-
    jGetStaticObjectField(Obj, FieldID, R).

%------------------------------------------------------------------------------

% jpl_new(+X, +Argz, -V) :-
%   X can be:
%    a suitable type
%	i.e. any class(_,_), array(_) or primitive (i.e. not void)
%    a class object
%	i.e. whose type is class([java,lang],['Class'])
%    an atomic classname
%	e.g. 'java.lang.String'
%	e.g. 'Ljava.lang.String;'
%    an atomic descriptor
%	e.g. '[I'
%
%   if X denotes a primitive type and Argz is castable to a value of that type,
%   then V is that value (a pointless mode of operation, but somehow complete...)
%
%   if X denotes an array type and Argz is a non-negative integer,
%   then V is a new array of that many elements,
%   initialised to the appropriate default value;
%
%   if X denotes an array type and Argz is a list of datums,
%   each of which is (independently) castable to the array element type,
%   then V is a new array of as many elements as Argz has members,
%   initialised to the results of casting the respective members of Argz;
%
%   if X denotes a non-array object type and Argz is a list of datums,
%   then V is the result of an invocation of that type's most specifically-typed
%   constructor to whose respective parameters the members of Argz are assignable

jpl_new(X, Argz, V) :-
    ground(Argz),
    (	jpl_is_type(X)			% NB only class(_,_) or array(_)
    ->	Tx = X
    ;	jpl_is_object(X),
	jpl_object_to_type(X, class([java,lang],['Class']))
    ->	jpl_class_to_type(X, Tx)	% get the type this class denotes
    ;	atom(X)			% eg 'java.lang.String', '[L'
    ->	jpl_classname_to_type(X, Tx)
    ),

  % jpl_type_to_classname(Tx, Classname), writeq([jpl_new,Classname,Argz]), nl,

    jpl_new_1(Tx, Argz, Vx),
    V = Vx.

%------------------------------------------------------------------------------

% jpl_new_1(+Tx, +Argz, -Vx) :-
%   (serves only jpl_new/3)
%
%   Tx can be:
%     a class(_,_) or array(_) type;
%
%   Argz can be:
%     a ground, proper list of constructor arguments
%     a single argument
%	(sloppy convenience, probably best ditched)
%
%   at call, Vx is unbound;
%   at exit, Vx is bound to a new, initialised ref (or a value) of type Tx

jpl_new_1(class(Ps,Cs), Argz, Vx) :-
    !,					    % green (see below)
    jpl_type_to_class(class(Ps,Cs), Cx),   % ensure class is findable
    Tx = class(Ps,Cs),
    (	is_list(Argz)			% canonise sloppy Argz to Args
    ->	Args = Argz,
	length(Args, A)
    ;	Args = [Argz],
	A = 1
    ),
    N = '<init>',	% JNI's constructor naming convention for GetMethodID()
    Tr = void,		% all constructors have this return "type"
    findall(
	z3(I,MID,Tps),
	jpl_method_spec(Tx, I, N, A, _Mods, MID, Tr, Tps), % cached
	Z3s
    ),
    (	Z3s == []			    % no matches
    ->	write('Warning: no arity-matching constructors'), nl, fail
    ;	Z3s = [z3(I,MID,Tps)]		    % exactly one match
    ->	true
    ;	jpl_datums_to_types(Args, Tas),    % most specific types
	findall(
	    z3(I,MID,Tps),  % select those to which Args is assignable
	    (	member(z3(I,MID,Tps), Z3s),
		jpl_types_fit_types(Tas, Tps)
	    ),
	    Z3sA
	),
	(   Z3sA == []
	->  write('Warning: no type-assignable constructors'), nl, fail
	;   Z3sA = [z3(I,MID,Tps)]
	->  true
	;   jpl_z3s_to_most_specific_z3(Z3sA, z3(I,MID,Tps))
	->  true
	;   write('Error: no single most-specific constructor'), nl, fail
	)
    ),
    jNewObject(Cx, MID, Tps, Args, Vx),
    jpl_cache_type_of_ref(Tx, Vx).	    % since we know it

jpl_new_1(array(T), Argz, Vx) :-
    !,
    (	integer(Argz),		% integer I -> array[0..I-1] of default values
	Argz >= 0
    ->	Len is Argz
    ;	is_list(Argz)	    % [V1,..VN] -> array[0..N-1] of respective values
    ->	length(Argz, Len)
    ),
    jpl_new_array(T, Len, Vx), % NB may throw out-of-memory exception
    (	nth0(I, Argz, Arg),	% nmember fails silently when Argz is integer
	jpl_set(Vx, I, Arg),
	fail
    ;	true
    ),
    jpl_cache_type_of_ref(array(T), Vx).   % since we know it

jpl_new_1(T, Argz, Vx) :-	% dubious completist generation of new primitive...
    (	Argz == []
    ->	jpl_primitive_type_default_value(T, Vx)
    ;	(   Argz = [Arg]
	;   Argz = Arg		% tolerate sloppiness...
	)
    ->	jpl_primitive_type_term_to_value(T, Argz, Vx)
    ).

%------------------------------------------------------------------------------

% jpl_new_array(+ElementType, +Length, -NewArray) :-

jpl_new_array(boolean, Len, A) :-
    jNewBooleanArray(Len, A).

jpl_new_array(byte, Len, A) :-
    jNewByteArray(Len, A).

jpl_new_array(char, Len, A) :-
    jNewCharArray(Len, A).

jpl_new_array(short, Len, A) :-
    jNewShortArray(Len, A).

jpl_new_array(int, Len, A) :-
    jNewIntArray(Len, A).

jpl_new_array(long, Len, A) :-
    jNewLongArray(Len, A).

jpl_new_array(float, Len, A) :-
    jNewFloatArray(Len, A).

jpl_new_array(double, Len, A) :-
    jNewDoubleArray(Len, A).

jpl_new_array(array(T), Len, A) :-
    jpl_type_to_class(array(T), C),
    jNewObjectArray(Len, C, @(null), A).	% initialise each element to null

jpl_new_array(class(Ps,Cs), Len, A) :-
    jpl_type_to_class(class(Ps,Cs), C),
    jNewObjectArray(Len, C, @(null), A).   

%------------------------------------------------------------------------------

% jpl_set(+X, +Fspec, +V) :-
%   basically, sets the Fspec-th field of object X to value V
%
%   X can be:
%     a class object, a classname, or an (object or array) type
%	(for static fields, or java.lang.Class fields)
%     a class instance
%	(for non-static fields)
%     an array
%	(for indexed element or subrange assignment)
%   but not:
%     a String (no fields to retrieve)
%
%   Fspec can be:
%     an atomic field name
%	(overloading will be resolved dynamically)
%     an integral field index
%	(static resolution: not tried yet)
%     a fieldID/1
%	(static resolution: not tried yet)
%     a variable
%	(field names, or array indices, are generated)
%     an array index I
%	(X must be an array object: X[I] is assigned V)
%     a pair I-J of integers (J can be a variable)
%	(X must be an array object, V must be a list of values: X[I-J] will be assigned V)
%
%   V must be ground (although one day we may pass variables to JPL?!)

jpl_set(X, Fspec, V) :-
    ground(V),			% only defined for ground datums
    (	jpl_is_object(X)	% i.e. of class(_,_) or array(_) type
    ->	jpl_object_to_type(X, Type),
	Obj = X
    ;	jpl_is_type(X)		% e.g. class([java,lang],['String']), array(int)
    ->	jpl_type_to_class(X, Obj),
	Type = class([java,lang],['Class'])
    ;	atom(X)		% e.g. 'java.lang.String', '[L'
    ->	jpl_classname_to_type(X, Tx),
	jpl_type_to_class(Tx, Obj),
	Type = class([java,lang],['Class'])
    ),
    jpl_set_1(Type, Obj, Fspec, V).

%------------------------------------------------------------------------------

% jpl_set_1(+Type, +Object, +FieldSpec, +Value) :-
%   Object may be a class object, in which case Type is class([java,lang],['Class'])

jpl_set_1(Tx, X, Fspec, V) :-		% static field?
    Tx = class([java,lang],['Class']),	% a class object...
    jpl_class_to_type(X, Tx2),
    Tx2 \== Tx,				% ...denoting other than java.lang.Class
    !,
    (	integer(Fspec)			% assume it's a field index
    ->	I = Fspec
    ;	atom(Fspec)			% assume it's a field name
    ->	N = Fspec
    ;	jpl_is_fieldID(Fspec)		% assume it's a JNI field ID
    ->	FID = Fspec
    ),
  % consider static and instance fields of java.lang.Class
  % and static fields of the denoted class
    findall(z2(T,I,N,Mods,FID,Tf),
	(   T = Tx,
	    jpl_field_spec(T, I, N, Mods, FID, Tf)
	;   T = Tx2,
	    jpl_field_spec(T, I, N, Mods, FID, Tf),
	    member(static, Mods)
	),
	Z2s
    ),
    (	Z2s = []
    ->	write('Warning: no matching fields'), nl
    ;	Z2s = [z2(T,I,N,Mods,FID,Tf)]
    ->	(   T = Tx
	->  jpl_type_to_class(Tx, Cc),
	    (	member(static, Mods)
	    ->	jpl_set_static_field(Tf, Cc, FID, V)
	    ;	jpl_set_field(Tf, Cc, FID, V)
	    )
	;   T = Tx2
	->  jpl_set_static_field(Tf, X, FID, V)
	)
    ;	write('Warning: many matching fields'), nl, fail
    ).

jpl_set_1(Tx, X, Fspec, V) :-	% instance field?
    Tx = class(_,_),	% instance object, or the java.lang.Class object itself
    !,
    (	integer(Fspec)		    % assume it's a field or array index
    ->	I = Fspec
    ;	atom(Fspec)		    % assume it's a field name
    ->	N = Fspec
    ;	jpl_is_fieldID(Fspec)	    % assume it's a JNI field ID
    ->	FID = Fspec
    ),
    findall(
	z(I,N,Mods,FID,Tf),
	jpl_field_spec(Tx, I, N, Mods, FID, Tf),
	Zs
    ),
    (	Zs = []
    ->	write('Warning: no matching fields'), nl
    ;	Zs = [z(I,N,Mods,FID,Tf)]
    ->	(   member(static, Mods)
	->  jpl_object_to_class(X, Cx),
	    jpl_set_static_field(Tf, Cx, FID, V)
	;   jpl_set_field(Tf, X, FID, V)
	)
    ;	write('Warning: many matching fields'), nl, fail
    ).

jpl_set_1(array(T), X, Fspec, V) :-
    (	is_list(V)
    ->	Vs = V
    ;	Vs = [V]		    % sloppy single-argument form
    ),
    length(Vs, Iv),
    (	integer(Fspec),	    % single-element assignment
	Fspec >= 0,
	Iv is 1
    ->	N is Fspec
    ;	Fspec = N-M,		    % element-sequence assignment
	integer(N),
	M is N+Iv-1
    ->	true
    ;	Fspec == length
    ->	write('Error: cannot assign a value to final variable length'), nl, fail
    ;	write('Error: bad field spec for array set '(Fspec)), nl, fail
    ),
    jpl_set_2(T, X, N, Iv, Vs).

%------------------------------------------------------------------------------

% jpl_set_2(+Type, +ArrayObject, +Offset, +DatumCount, +Datums) :-
%   Datums, of which there are DatumCount,
%   are stashed in successive elements of ArrayObject
%   which is an array of Type
%   starting at the Offset-th (numbered from 0)

jpl_set_2(T, A, N, I, Ds) :-
    (	jpl_datums_to_types(Ds, Tds),	    % most specialised types
	jpl_types_fit_type(Tds, T)	    % all assignable?
    ->	true
    ;	write('Warning: not all datums can be assigned to array element type'), nl, fail
    ),
    (	(   T = class(_,_)
	;   T = array(_)
	)
    ->	(   nth0(J, Ds, D),		    % for each datum
	    Nd is N+J,			    % compute array index
	    jSetObjectArrayElement(A, Nd, D),
	    fail			    % iterate
	;   true
	)
    ;	jpl_primitive_type(T)
    ->	jni_type_to_xput_code(T, Xc),
	jni_alloc_buffer(Xc, I, Bp),	    % I-element buf of required type
	jpl_set_3(Ds, T, 0, Bp),
	jpl_set_elements(T, A, N, I, Bp),
	jni_free_buffer(Bp)
    ;	fail				    % bad type (exception?)
    ).

%------------------------------------------------------------------------------

% jpl_set_3(+Values, +Type, +BufferIndex, +BufferPointer) :-
%   successive members of Values are stashed as (primitive) Type
%   from the BufferIndex-th element (numbered from 0) onwards
%   of the buffer indicated by BufferPointer

jpl_set_3([], _, _, _).

jpl_set_3([V|Vs], Tprim, Ib, Bp) :-
    jni_convert_primitive_out(Tprim, V, Xhi, Xlo, Xc),
    jni_stash_buffer_value(Bp, Ib, Xhi, Xlo, Xc),
    Ibnext is Ib+1,
    jpl_set_3(Vs, Tprim, Ibnext, Bp).

%------------------------------------------------------------------------------

jpl_set_elements(boolean, Obj, N, I, Bp) :-
    jSetBooleanArrayRegion(Obj, N, I, jbuf(Bp,boolean)).

jpl_set_elements(char, Obj, N, I, Bp) :-
    jSetCharArrayRegion(Obj, N, I, jbuf(Bp,char)).

jpl_set_elements(byte, Obj, N, I, Bp) :-
    jSetByteArrayRegion(Obj, N, I, jbuf(Bp,byte)).

jpl_set_elements(short, Obj, N, I, Bp) :-
    jSetShortArrayRegion(Obj, N, I, jbuf(Bp,short)).

jpl_set_elements(int, Obj, N, I, Bp) :-
    jSetIntArrayRegion(Obj, N, I, jbuf(Bp,int)).

jpl_set_elements(long, Obj, N, I, Bp) :-
    jSetLongArrayRegion(Obj, N, I, jbuf(Bp,long)).

jpl_set_elements(float, Obj, N, I, Bp) :-
    jSetFloatArrayRegion(Obj, N, I, jbuf(Bp,float)).

jpl_set_elements(double, Obj, N, I, Bp) :-
    jSetDoubleArrayRegion(Obj, N, I, jbuf(Bp,double)).

%------------------------------------------------------------------------------

jpl_set_field(boolean, Obj, FieldID, R) :-
    jSetBooleanField(Obj, FieldID, R).

jpl_set_field(byte, Obj, FieldID, R) :-
    jSetByteField(Obj, FieldID, R).

jpl_set_field(char, Obj, FieldID, R) :-
    jSetCharField(Obj, FieldID, R).

jpl_set_field(short, Obj, FieldID, R) :-
    jSetShortField(Obj, FieldID, R).

jpl_set_field(int, Obj, FieldID, R) :-
    jSetIntField(Obj, FieldID, R).

jpl_set_field(long, Obj, FieldID, R) :-
    jSetLongField(Obj, FieldID, R).

jpl_set_field(float, Obj, FieldID, R) :-
    jSetFloatField(Obj, FieldID, R).

jpl_set_field(double, Obj, FieldID, R) :-
    jSetDoubleField(Obj, FieldID, R).

jpl_set_field(class(_,_), Obj, FieldID, R) :-
    jSetObjectField(Obj, FieldID, R).

jpl_set_field(array(_), Obj, FieldID, R) :-
    jSetObjectField(Obj, FieldID, R).

%------------------------------------------------------------------------------

jpl_set_static_field(boolean, Obj, FieldID, R) :-
    jSetStaticBooleanField(Obj, FieldID, R).

jpl_set_static_field(byte, Obj, FieldID, R) :-
    jSetStaticByteField(Obj, FieldID, R).

jpl_set_static_field(char, Obj, FieldID, R) :-
    jSetStaticCharField(Obj, FieldID, R).

jpl_set_static_field(short, Obj, FieldID, R) :-
    jSetStaticShortField(Obj, FieldID, R).

jpl_set_static_field(int, Obj, FieldID, R) :-
    jSetStaticIntField(Obj, FieldID, R).

jpl_set_static_field(long, Obj, FieldID, R) :-
    jSetStaticLongField(Obj, FieldID, R).

jpl_set_static_field(float, Obj, FieldID, R) :-
    jSetStaticFloatField(Obj, FieldID, R).

jpl_set_static_field(double, Obj, FieldID, R) :-
    jSetStaticDoubleField(Obj, FieldID, R).

jpl_set_static_field(class(_,_), Obj, FieldID, R) :-
    jSetStaticObjectField(Obj, FieldID, R).

jpl_set_static_field(array(_), Obj, FieldID, R) :-
    jSetStaticObjectField(Obj, FieldID, R).

%------------------------------------------------------------------------------

% jpl_z3s_to_most_specific_z3(+Zs, -Z) :-
%   Zs is a list of arity-matching, type-suitable z3(I,MID,Tps)
%   Z is the single most specific element of Zs,
%   i.e. that than which no other z3/3 has a more specialised signature;
%   fails if there is more than one such

jpl_z3s_to_most_specific_z3(Zs, Z) :-
    jpl_fergus_is_the_greatest(Zs, Z).

%------------------------------------------------------------------------------

% jpl_z5s_to_most_specific_z5(+Zs, -Z) :-
%   Zs is a list of arity-matching, type-suitable z5(I,Mods,MID,Tr,Tps)
%   Z is the single most specific element of Zs,
%   i.e. that than which no other z5/5 has a more specialised signature
%   (this fails if there is more than one such)

jpl_z5s_to_most_specific_z5(Zs, Z) :-
    jpl_fergus_is_the_greatest(Zs, Z).

%------------------------------------------------------------------------------

% jpl_pl_lib_version(-VersionString) :-
% jpl_pl_lib_version(-Major, -Minor, -Patch, -Status) :-

jpl_pl_lib_version(VersionString) :-
    jpl_pl_lib_version(Major, Minor, Patch, Status),
    concat_atom([Major,'.',Minor,'.',Patch,'-',Status], VersionString).


jpl_pl_lib_version(3, 0, 3, alpha).

%------------------------------------------------------------------------------

% jpl_type_alfa(0'$) -->	% presumably not allowed
%   "$".			% given the "inner class" syntax?

jpl_type_alfa(0'_) -->
    "_",
    !.

jpl_type_alfa(C) -->
    [C], { C>=0'a, C=<0'z },
    !.

jpl_type_alfa(C) -->
    [C], { C>=0'A, C=<0'Z }.

%------------------------------------------------------------------------------

jpl_type_alfa_num(C) -->
    jpl_type_alfa(C),
    !.

jpl_type_alfa_num(C) -->
    [C], { C>=0'0, C=<0'9 }.

%------------------------------------------------------------------------------

jpl_type_array_classname(array(T)) -->
    "[", jpl_type_classname_2(T).

%------------------------------------------------------------------------------

jpl_type_array_descriptor(array(T)) -->
    "[", jpl_type_descriptor_1(T).

%------------------------------------------------------------------------------

jpl_type_bare_class_descriptor(class(Ps,Cs)) -->
    jpl_type_slashed_package_parts(Ps), jpl_type_class_parts(Cs).

%------------------------------------------------------------------------------

jpl_type_bare_classname(class(Ps,Cs)) -->
    jpl_type_dotted_package_parts(Ps), jpl_type_class_parts(Cs).

%------------------------------------------------------------------------------

jpl_type_class_descriptor(class(Ps,Cs)) -->
    "L", jpl_type_bare_class_descriptor(class(Ps,Cs)), ";".

%------------------------------------------------------------------------------

jpl_type_class_part(N) -->
    jpl_type_id(N).

%------------------------------------------------------------------------------

jpl_type_class_parts([C|Cs]) -->
    jpl_type_class_part(C), jpl_type_inner_class_parts(Cs).

%------------------------------------------------------------------------------

jpl_type_classname_1(T) -->
    jpl_type_bare_classname(T),
    !.

jpl_type_classname_1(T) -->
    jpl_type_array_classname(T),
    !.

jpl_type_classname_1(T) -->
    jpl_type_primitive(T).

%------------------------------------------------------------------------------

jpl_type_classname_2(T) -->
    jpl_type_delimited_classname(T).

jpl_type_classname_2(T) -->
    jpl_type_array_classname(T).

jpl_type_classname_2(T) -->
    jpl_type_primitive(T).

%------------------------------------------------------------------------------

jpl_type_delimited_classname(Class) -->
    "L", jpl_type_bare_classname(Class), ";".

%------------------------------------------------------------------------------

jpl_type_descriptor_1(T) -->
    jpl_type_primitive(T),
    !.

jpl_type_descriptor_1(T) -->
    jpl_type_class_descriptor(T),
    !.

jpl_type_descriptor_1(T) -->
    jpl_type_array_descriptor(T),
    !.

jpl_type_descriptor_1(T) -->
    jpl_type_method_descriptor(T).

%------------------------------------------------------------------------------

jpl_type_dotted_package_parts([P|Ps]) -->
    jpl_type_package_part(P), ".", !, jpl_type_dotted_package_parts(Ps).

jpl_type_dotted_package_parts([]) -->
    [].

%------------------------------------------------------------------------------

jpl_type_findclassname(T) -->
    jpl_type_bare_class_descriptor(T).

jpl_type_findclassname(T) -->
    jpl_type_array_descriptor(T).

%------------------------------------------------------------------------------

jpl_type_id(A) -->
    { nonvar(A) -> atom_codes(A,[C|Cs]) ; true },
    jpl_type_alfa(C), jpl_type_id_rest(Cs),
    { atom_codes(A, [C|Cs]) }.

%------------------------------------------------------------------------------

jpl_type_id_rest([C|Cs]) -->
    jpl_type_alfa_num(C), !, jpl_type_id_rest(Cs).

jpl_type_id_rest([]) -->
    [].

%------------------------------------------------------------------------------

jpl_type_id_v2(A) -->			% inner class name parts (empirically)
    { nonvar(A) -> atom_codes(A,Cs) ; true },
    jpl_type_id_rest(Cs),
    { atom_codes(A, Cs) }.

%------------------------------------------------------------------------------

jpl_type_inner_class_part(N) -->
    jpl_type_id_v2(N).

%------------------------------------------------------------------------------

jpl_type_inner_class_parts([C|Cs]) -->
    "$", jpl_type_inner_class_part(C), !, jpl_type_inner_class_parts(Cs).

jpl_type_inner_class_parts([]) -->
    [].

%------------------------------------------------------------------------------

jpl_type_method_descriptor(method(Ts,T)) -->
    "(", jpl_type_method_descriptor_args(Ts), ")", jpl_type_method_descriptor_return(T).

%------------------------------------------------------------------------------

jpl_type_method_descriptor_args([T|Ts]) -->
    jpl_type_descriptor_1(T), !, jpl_type_method_descriptor_args(Ts).

jpl_type_method_descriptor_args([]) -->
    [].

%------------------------------------------------------------------------------

jpl_type_method_descriptor_return(T) -->
    jpl_type_void(T).

jpl_type_method_descriptor_return(T) -->
    jpl_type_descriptor_1(T).

%------------------------------------------------------------------------------

jpl_type_package_part(N) -->
    jpl_type_id(N).

%------------------------------------------------------------------------------

jpl_type_primitive(boolean) -->
    "Z",
    !.

jpl_type_primitive(byte) -->
    "B",
    !.

jpl_type_primitive(char) -->
    "C",
    !.

jpl_type_primitive(short) -->
    "S",
    !.

jpl_type_primitive(int) -->
    "I",
    !.

jpl_type_primitive(long) -->
    "J",
    !.

jpl_type_primitive(float) -->
    "F",
    !.

jpl_type_primitive(double) -->
    "D".

%------------------------------------------------------------------------------

jpl_type_slashed_package_parts([P|Ps]) -->
    jpl_type_package_part(P), "/", !, jpl_type_slashed_package_parts(Ps).

jpl_type_slashed_package_parts([]) -->
    [].

%------------------------------------------------------------------------------

jpl_type_void(void) -->
    "V".

%------------------------------------------------------------------------------

%type	jCallBooleanMethod(object, method_id, types, datums, boolean)

% jCallBooleanMethod(+Obj, +MethodID, +Types, +Args, -Rbool) :-

jCallBooleanMethod(Obj, MethodID, Types, Args, Rbool) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(39, Obj, MethodID, ArgBuf, Rbool).

%------------------------------------------------------------------------------

%type	jCallByteMethod(object, method_id, types, datums, byte)

% jCallByteMethod(+Obj, +MethodID, +Types, +Args, -Rbyte) :-

jCallByteMethod(Obj, MethodID, Types, Args, Rbyte) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(42, Obj, MethodID, ArgBuf, Rbyte).

%------------------------------------------------------------------------------

%type	jCallCharMethod(object, method_id, types, datums, char)

% jCallCharMethod(+Obj, +MethodID, +Types, +Args, -Rchar) :-

jCallCharMethod(Obj, MethodID, Types, Args, Rchar) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(45, Obj, MethodID, ArgBuf, Rchar).

%------------------------------------------------------------------------------

%type	jCallDoubleMethod(object, method_id, types, datums, double)

% jCallDoubleMethod(+Obj, +MethodID, +Types, +Args, -Rdouble) :-

jCallDoubleMethod(Obj, MethodID, Types, Args, Rdouble) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(60, Obj, MethodID, ArgBuf, Rdouble).

%------------------------------------------------------------------------------

%type	jCallFloatMethod(object, method_id, types, datums, float)

% jCallFloatMethod(+Obj, +MethodID, +Types, +Args, -Rfloat) :-

jCallFloatMethod(Obj, MethodID, Types, Args, Rfloat) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(57, Obj, MethodID, ArgBuf, Rfloat).

%------------------------------------------------------------------------------

%type	jCallIntMethod(object, method_id, types, datums, int)

% jCallIntMethod(+Obj, +MethodID, +Types, +Args, -Rint) :-

jCallIntMethod(Obj, MethodID, Types, Args, Rint) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(51, Obj, MethodID, ArgBuf, Rint).

%------------------------------------------------------------------------------

%type	jCallLongMethod(object, method_id, types, datums, long)

% jCallLongMethod(+Obj, +MethodID, +Types, +Args, -Rlong) :-

jCallLongMethod(Obj, MethodID, Types, Args, Rlong) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(54, Obj, MethodID, ArgBuf, Rlong).

%------------------------------------------------------------------------------

%type	jCallObjectMethod(object, method_id, types, datums, object)

% jCallObjectMethod(+Obj, +MethodID, +Types, +Args, -Robj) :-

jCallObjectMethod(Obj, MethodID, Types, Args, Robj) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(36, Obj, MethodID, ArgBuf, Robj).

%------------------------------------------------------------------------------

%type	jCallShortMethod(object, method_id, types, datums, short)

% jCallShortMethod(+Obj, +MethodID, +Types, +Args, -Rshort) :-

jCallShortMethod(Obj, MethodID, Types, Args, Rshort) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(48, Obj, MethodID, ArgBuf, Rshort).

%------------------------------------------------------------------------------

%type	jCallStaticBooleanMethod(class, types, datums, boolean)

% jCallStaticBooleanMethod(+Class, +MethodID, +Types, +Args, -Rbool) :-

jCallStaticBooleanMethod(Class, MethodID, Types, Args, Rbool) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(119, Class, MethodID, ArgBuf, Rbool).

%------------------------------------------------------------------------------

%type	jCallStaticByteMethod(class, method_id, types, datums, byte)

% jCallStaticByteMethod(+Class, +MethodID, +Types, +Args, -Rbyte) :-

jCallStaticByteMethod(Class, MethodID, Types, Args, Rbyte) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(122, Class, MethodID, ArgBuf, Rbyte).

%------------------------------------------------------------------------------

%type	jCallStaticCharMethod(class, method_id, types, datums, char)

% jCallStaticCharMethod(+Class, +MethodID, +Types, +Args, -Rchar) :-

jCallStaticCharMethod(Class, MethodID, Types, Args, Rchar) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(125, Class, MethodID, ArgBuf, Rchar).

%------------------------------------------------------------------------------

%type	jCallStaticDoubleMethod(class, method_id, types, datums, double)

% jCallStaticDoubleMethod(+Class, +MethodID, +Types, +Args, -Rdouble) :-

jCallStaticDoubleMethod(Class, MethodID, Types, Args, Rdouble) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(140, Class, MethodID, ArgBuf, Rdouble).

%------------------------------------------------------------------------------

%type	jCallStaticFloatMethod(class, method_id, types, datums, float)

% jCallStaticFloatMethod(+Class, +MethodID, +Types, +Args, -Rfloat) :-

jCallStaticFloatMethod(Class, MethodID, Types, Args, Rfloat) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(137, Class, MethodID, ArgBuf, Rfloat).

%------------------------------------------------------------------------------

%type	jCallStaticIntMethod(class, method_id, types, datums, int)

% jCallStaticIntMethod(+Class, +MethodID, +Types, +Args, -Rint) :-

jCallStaticIntMethod(Class, MethodID, Types, Args, Rint) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(131, Class, MethodID, ArgBuf, Rint).

%------------------------------------------------------------------------------

%type	jCallStaticLongMethod(class, method_id, types, datums, long)

% jCallStaticLongMethod(+Class, +MethodID, +Types, +Args, -Rlong) :-

jCallStaticLongMethod(Class, MethodID, Types, Args, Rlong) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(134, Class, MethodID, ArgBuf, Rlong).

%------------------------------------------------------------------------------

%type	jCallStaticObjectMethod(class, method_id, types, datums, object)

% jCallStaticObjectMethod(+Class, +MethodID, +Types, +Args, -Robj) :-

jCallStaticObjectMethod(Class, MethodID, Types, Args, Robj) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(116, Class, MethodID, ArgBuf, Robj).

%------------------------------------------------------------------------------

%type	jCallStaticShortMethod(class, method_id, types, datums, short)

% jCallStaticShortMethod(+Class, +MethodID, +Types, +Args, -Rshort) :-

jCallStaticShortMethod(Class, MethodID, Types, Args, Rshort) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(128, Class, MethodID, ArgBuf, Rshort).

%------------------------------------------------------------------------------

%type	jCallStaticVoidMethod(class, method_id, types, datums)

% jCallStaticVoidMethod(+Class, +MethodID, +Types, +Args) :-

jCallStaticVoidMethod(Class, MethodID, Types, Args) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_void(143, Class, MethodID, ArgBuf).

%------------------------------------------------------------------------------

%type	jCallVoidMethod(object, method_id, types, datums)

% jCallVoidMethod(+Obj, +MethodID, +Types, +Args) :-

jCallVoidMethod(Obj, MethodID, Types, Args) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_void(63, Obj, MethodID, ArgBuf).

%------------------------------------------------------------------------------

%type	jFindClass(findclassname, class)

% jFindClass(+ClassName, -Class) :-

jFindClass(ClassName, Class) :-
    jni_func(6, ClassName, Class).

%------------------------------------------------------------------------------

%type	jGetArrayLength(array, int)

% jGetArrayLength(+Array, -Size) :-

jGetArrayLength(Array, Size) :-
    jni_func(171, Array, Size).

%------------------------------------------------------------------------------

%type	jGetBooleanArrayRegion(boolean_array, int, int, boolean_buf)

% jGetBooleanArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetBooleanArrayRegion(Array, Start, Len, Buf) :-
    jni_void(199, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetBooleanField(object, field_id, boolean)

% jGetBooleanField(+Obj, +FieldID, -Rbool) :-

jGetBooleanField(Obj, FieldID, Rbool) :-
    jni_func(96, Obj, FieldID, Rbool).

%------------------------------------------------------------------------------

%type	jGetByteArrayRegion(byte_array, int, int, byte_buf)

% jGetByteArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetByteArrayRegion(Array, Start, Len, Buf) :-
    jni_void(200, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetByteField(object, field_id, byte)

% jGetByteField(+Obj, +FieldID, -Rbyte) :-

jGetByteField(Obj, FieldID, Rbyte) :-
    jni_func(97, Obj, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type	jGetCharArrayRegion(char_array, int, int, char_buf)

% jGetCharArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetCharArrayRegion(Array, Start, Len, Buf) :-
    jni_void(201, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetCharField(object, field_id, char)

% jGetCharField(+Obj, +FieldID, -Rchar) :-

jGetCharField(Obj, FieldID, Rchar) :-
    jni_func(98, Obj, FieldID, Rchar).

%------------------------------------------------------------------------------

%type	jGetDoubleArrayRegion(double_array, int, int, double_buf)

% jGetDoubleArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetDoubleArrayRegion(Array, Start, Len, Buf) :-
    jni_void(206, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetDoubleField(object, field_id, double)

% jGetDoubleField(+Obj, +FieldID, -Rdouble) :-

jGetDoubleField(Obj, FieldID, Rdouble) :-
    jni_func(103, Obj, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type	jGetFieldID(class, descriptor, field_id)

% jGetFieldID(+Class, +Name, +Typedescriptor, -FieldID) :-

jGetFieldID(Class, Name, Type, FieldID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(94, Class, Name, TD, FieldID).

%------------------------------------------------------------------------------

%type	jGetFloatArrayRegion(float_array, int, int, float_buf)

% jGetFloatArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetFloatArrayRegion(Array, Start, Len, Buf) :-
    jni_void(205, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetFloatField(object, field_id, float)

% jGetFloatField(+Obj, +FieldID, -Rfloat) :-

jGetFloatField(Obj, FieldID, Rfloat) :-
    jni_func(102, Obj, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type	jGetIntArrayRegion(int_array, int, int, int_buf)

% jGetIntArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetIntArrayRegion(Array, Start, Len, Buf) :-
    jni_void(203, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetIntField(object, field_id, int)

% jGetIntField(+Obj, +FieldID, -Rint) :-

jGetIntField(Obj, FieldID, Rint) :-
    jni_func(100, Obj, FieldID, Rint).

%------------------------------------------------------------------------------

%type	jGetLongArrayRegion(long_array, int, int, long_buf)

% jGetLongArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetLongArrayRegion(Array, Start, Len, Buf) :-
    jni_void(204, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetLongField(object, field_id, long)

% jGetLongField(+Obj, +FieldID, -Rlong) :-

jGetLongField(Obj, FieldID, Rlong) :-
    jni_func(101, Obj, FieldID, Rlong).

%------------------------------------------------------------------------------

%type	jGetMethodID(class, name, descriptor, method_id)

% jGetMethodID(+Class, +Name, +TypeDescriptor, -MethodID) :-

jGetMethodID(Class, Name, Type, MethodID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(33, Class, Name, TD, MethodID).

%------------------------------------------------------------------------------

%type	jGetObjectArrayElement(object_array, int, object)

% jGetObjectArrayElement(+Array, +Index, -Obj) :-

jGetObjectArrayElement(Array, Index, Obj) :-
    jni_func(173, Array, Index, Obj).

%------------------------------------------------------------------------------

%type	jGetObjectClass(object, class)

% jGetObjectClass(+Object, -Class) :-

jGetObjectClass(Object, Class) :-
    jni_func(31, Object, Class).

%------------------------------------------------------------------------------

%type	jGetObjectField(object, field_id, object)

% jGetObjectField(+Obj, +FieldID, -RObj) :-

jGetObjectField(Obj, FieldID, Robj) :-
    jni_func(95, Obj, FieldID, Robj).

%------------------------------------------------------------------------------

%type	jGetShortArrayRegion(short_array, int, int, short_buf)

% jGetShortArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetShortArrayRegion(Array, Start, Len, Buf) :-
    jni_void(202, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jGetShortField(object, field_id, short)

% jGetShortField(+Obj, +FieldID, -Rshort) :-

jGetShortField(Obj, FieldID, Rshort) :-
    jni_func(99, Obj, FieldID, Rshort).

%------------------------------------------------------------------------------

%type	jGetStaticBooleanField(class, field_id, boolean)

% jGetStaticBooleanField(+Class, +FieldID, -Rbool) :-

jGetStaticBooleanField(Class, FieldID, Rbool) :-
    jni_func(146, Class, FieldID, Rbool).

%------------------------------------------------------------------------------

%type	jGetStaticByteField(class, field_id, byte)

% jGetStaticByteField(+Class, +FieldID, -Rbyte) :-

jGetStaticByteField(Class, FieldID, Rbyte) :-
    jni_func(147, Class, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type	jGetStaticCharField(class, field_id, char)

% jGetStaticCharField(+Class, +FieldID, -Rchar) :-

jGetStaticCharField(Class, FieldID, Rchar) :-
    jni_func(148, Class, FieldID, Rchar).

%------------------------------------------------------------------------------

%type	jGetStaticDoubleField(class, field_id, double)

% jGetStaticDoubleField(+Class, +FieldID, -Rdouble) :-

jGetStaticDoubleField(Class, FieldID, Rdouble) :-
    jni_func(153, Class, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type	jGetStaticFieldID(class, name, field_id)

% jGetStaticFieldID(+Class, +Name, +TypeDescriptor, -FieldID) :-

jGetStaticFieldID(Class, Name, Type, FieldID) :-
    jpl_type_to_descriptor(Type, TD),		    % cache this?
    jni_func(144, Class, Name, TD, FieldID).

%------------------------------------------------------------------------------

%type	jGetStaticFloatField(class, field_id, float)

% jGetStaticFloatField(+Class, +FieldID, -Rfloat) :-

jGetStaticFloatField(Class, FieldID, Rfloat) :-
    jni_func(152, Class, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type	jGetStaticIntField(class, field_id, int)

% jGetStaticIntField(+Class, +FieldID, -Rint) :-

jGetStaticIntField(Class, FieldID, Rint) :-
    jni_func(150, Class, FieldID, Rint).

%------------------------------------------------------------------------------

%type	jGetStaticLongField(class, field_id, long)

% jGetStaticLongField(+Class, +FieldID, -Rlong) :-

jGetStaticLongField(Class, FieldID, Rlong) :-
    jni_func(151, Class, FieldID, Rlong).

%------------------------------------------------------------------------------

%type	jGetStaticMethodID(class, name, method_id)

% jGetStaticMethodID(+Class, +Name, +TypeDescriptor, -MethodID) :-

jGetStaticMethodID(Class, Name, Type, MethodID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(113, Class, Name, TD, MethodID).

%------------------------------------------------------------------------------

%type	jGetStaticObjectField(class, field_id, object)

% jGetStaticObjectField(+Class, +FieldID, -RObj) :-

jGetStaticObjectField(Class, FieldID, Robj) :-
    jni_func(145, Class, FieldID, Robj).

%------------------------------------------------------------------------------

%type	jGetStaticShortField(class, field_id, short)

% jGetStaticShortField(+Class, +FieldID, -Rshort) :-

jGetStaticShortField(Class, FieldID, Rshort) :-
    jni_func(149, Class, FieldID, Rshort).

%------------------------------------------------------------------------------

%type	jGetSuperclass(object, object)

% jGetSuperclass(+Class1, -Class2) :-

jGetSuperclass(Class1, Class2) :-
    jni_func(10, Class1, Class2).

%------------------------------------------------------------------------------

%type	jIsAssignableFrom(object, object)

% jIsAssignableFrom(+Class1, +Class2) :-

jIsAssignableFrom(Class1, Class2) :-
    jni_func(11, Class1, Class2, @(true)).

%------------------------------------------------------------------------------

%type	jNewBooleanArray(int, boolean_array)

% jNewBooleanArray(+Length, -Array) :-

jNewBooleanArray(Length, Array) :-
    jni_func(175, Length, Array).

%------------------------------------------------------------------------------

%type	jNewByteArray(int, byte_array)

% jNewByteArray(+Length, -Array) :-

jNewByteArray(Length, Array) :-
    jni_func(176, Length, Array).

%------------------------------------------------------------------------------

%type	jNewCharArray(int, char_array)

% jNewCharArray(+Length, -Array) :-

jNewCharArray(Length, Array) :-
    jni_func(177, Length, Array).

%------------------------------------------------------------------------------

%type	jNewDoubleArray(int, double_array)

% jNewDoubleArray(+Length, -Array) :-

jNewDoubleArray(Length, Array) :-
    jni_func(182, Length, Array).

%------------------------------------------------------------------------------

%type	jNewFloatArray(int, float_array)

% jNewFloatArray(+Length, -Array) :-

jNewFloatArray(Length, Array) :-
    jni_func(181, Length, Array).

%------------------------------------------------------------------------------

%type	jNewIntArray(int, int_array)

% jNewIntArray(+Length, -Array) :-

jNewIntArray(Length, Array) :-
    jni_func(179, Length, Array).

%------------------------------------------------------------------------------

%type	jNewLongArray(int, long_array)

% jNewLongArray(+Length, -Array) :-

jNewLongArray(Length, Array) :-
    jni_func(180, Length, Array).

%------------------------------------------------------------------------------

%type	jNewObject(class, method_id, types, datums, object)

% jNewObject(+Class, +MethodID, +Types, +Args, -Obj) :-

jNewObject(Class, MethodID, Types, Args, Obj) :-
    jni_params_put(Args, Types, ArgBuf),
    jni_func(30, Class, MethodID, ArgBuf, Obj).

%------------------------------------------------------------------------------

%type	jNewObjectArray(int, class, object, object_array)

% jNewObjectArray(+Len, +Class, +InitVal, -Array) :-

jNewObjectArray(Len, Class, InitVal, Array) :-
    jni_func(172, Len, Class, InitVal, Array).

%------------------------------------------------------------------------------

%type	jNewShortArray(int, short_array)

% jNewShortArray(+Length, -Array) :-

jNewShortArray(Length, Array) :-
    jni_func(178, Length, Array).

%------------------------------------------------------------------------------

%type	jSetBooleanArrayRegion(boolean_array, int, int, boolean_buf)

% jSetBooleanArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetBooleanArrayRegion(Array, Start, Len, Buf) :-
    jni_void(207, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetBooleanField(object, field_id, boolean)

% jSetBooleanField(+Obj, +FieldID, +Rbool) :-

jSetBooleanField(Obj, FieldID, Rbool) :-
    jni_void(105, Obj, FieldID, Rbool).

%------------------------------------------------------------------------------

%type	jSetByteArrayRegion(byte_array, int, int, byte_buf)

% jSetByteArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetByteArrayRegion(Array, Start, Len, Buf) :-
    jni_void(208, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetByteField(object, field_id, byte)

% jSetByteField(+Obj, +FieldID, +Rbyte) :-

jSetByteField(Obj, FieldID, Rbyte) :-
    jni_void(106, Obj, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type	jSetCharArrayRegion(char_array, int, int, char_buf)

% jSetCharArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetCharArrayRegion(Array, Start, Len, Buf) :-
    jni_void(209, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetCharField(object, field_id, char)

% jSetCharField(+Obj, +FieldID, +Rchar) :-

jSetCharField(Obj, FieldID, Rchar) :-
    jni_void(107, Obj, FieldID, Rchar).

%------------------------------------------------------------------------------

%type	jSetDoubleArrayRegion(double_array, int, int, double_buf)

% jSetDoubleArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetDoubleArrayRegion(Array, Start, Len, Buf) :-
    jni_void(214, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetDoubleField(object, field_id, double)

% jSetDoubleField(+Obj, +FieldID, +Rdouble) :-

jSetDoubleField(Obj, FieldID, Rdouble) :-
    jni_void(112, Obj, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type	jSetFloatArrayRegion(float_array, int, int, float_buf)

% jSetFloatArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetFloatArrayRegion(Array, Start, Len, Buf) :-
    jni_void(213, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetFloatField(object, field_id, float)

% jSetFloatField(+Obj, +FieldID, +Rfloat) :-

jSetFloatField(Obj, FieldID, Rfloat) :-
    jni_void(111, Obj, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type	jSetIntArrayRegion(int_array, int, int, int_buf)

% jSetIntArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetIntArrayRegion(Array, Start, Len, Buf) :-
    jni_void(211, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetIntField(object, field_id, int)

% jSetIntField(+Obj, +FieldID, +Rint) :-

jSetIntField(Obj, FieldID, Rint) :-
    jni_void(109, Obj, FieldID, Rint).

%------------------------------------------------------------------------------

%type	jSetLongArrayRegion(long_array, int, int, long_buf)

% jSetLongArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetLongArrayRegion(Array, Start, Len, Buf) :-
    jni_void(212, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetLongField(object, field_id, long)

% jSetLongField(+Obj, +FieldID, +Rlong) :-

jSetLongField(Obj, FieldID, Rlong) :-
    jni_void(110, Obj, FieldID, Rlong).

%------------------------------------------------------------------------------

%type	jSetObjectArrayElement(object_array, int, object)

% jSetObjectArrayElement(+Array, +Index, +Obj) :-

jSetObjectArrayElement(Array, Index, Obj) :-
    jni_void(174, Array, Index, Obj).

%------------------------------------------------------------------------------

%type	jSetObjectField(object, field_id, object)

% jSetObjectField(+Obj, +FieldID, +RObj) :-

jSetObjectField(Obj, FieldID, Robj) :-
    jni_void(104, Obj, FieldID, Robj).

%------------------------------------------------------------------------------

%type	jSetShortArrayRegion(short_array, int, int, short_buf)

% jSetShortArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetShortArrayRegion(Array, Start, Len, Buf) :-
    jni_void(210, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type	jSetShortField(object, field_id, short)

% jSetShortField(+Obj, +FieldID, +Rshort) :-

jSetShortField(Obj, FieldID, Rshort) :-
    jni_void(108, Obj, FieldID, Rshort).

%------------------------------------------------------------------------------

%type	jSetStaticBooleanField(class, field_id, boolean)

% jSetStaticBooleanField(+Class, +FieldID, +Rbool) :-

jSetStaticBooleanField(Class, FieldID, Rbool) :-
    jni_void(155, Class, FieldID, Rbool).

%------------------------------------------------------------------------------

%type	jSetStaticByteField(class, field_id, byte)

% jSetStaticByteField(+Class, +FieldID, +Rbyte) :-

jSetStaticByteField(Class, FieldID, Rbyte) :-
    jni_void(156, Class, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type	jSetStaticCharField(class, field_id, char)

% jSetStaticCharField(+Class, +FieldID, +Rchar) :-

jSetStaticCharField(Class, FieldID, Rchar) :-
    jni_void(157, Class, FieldID, Rchar).

%------------------------------------------------------------------------------

%type	jSetStaticDoubleField(class, field_id, double)

% jSetStaticDoubleField(+Class, +FieldID, +Rdouble) :-

jSetStaticDoubleField(Class, FieldID, Rdouble) :-
    jni_void(162, Class, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type	jSetStaticFloatField(class, field_id, float)

% jSetStaticFloatField(+Class, +FieldID, +Rfloat) :-

jSetStaticFloatField(Class, FieldID, Rfloat) :-
    jni_void(161, Class, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type	jSetStaticIntField(class, field_id, int)

% jSetStaticIntField(+Class, +FieldID, +Rint) :-

jSetStaticIntField(Class, FieldID, Rint) :-
    jni_void(159, Class, FieldID, Rint).

%------------------------------------------------------------------------------

%type	jSetStaticLongField(class, field_id, long)

% jSetStaticLongField(+Class, +FieldID, +Rlong) :-

jSetStaticLongField(Class, FieldID, Rlong) :-
    jni_void(160, Class, FieldID, Rlong).

%------------------------------------------------------------------------------

%type	jSetStaticObjectField(class, field_id, object)

% jSetStaticObjectField(+Class, +FieldID, +Robj) :-

jSetStaticObjectField(Class, FieldID, Robj) :-
    jni_void(154, Class, FieldID, Robj).

%------------------------------------------------------------------------------

%type	jSetStaticShortField(class, field_id, short)

% jSetStaticShortField(+Class, +FieldID, +Rshort) :-

jSetStaticShortField(Class, FieldID, Rshort) :-
    jni_void(158, Class, FieldID, Rshort).

%------------------------------------------------------------------------------

% jni_convert_primitive_in(+JspType, -Term, +Xhi, +Xlo) :-
%   a value of JspType returned
%   (by jni_primitive_buffer_to_array or jni_get_array_element)
%   as integers Xhi and Xlo
%   is canonically (?) represented by Term;
%   object types are bound in only where obvious (e.g. jstring, but not jthrowable)
%   NB this oughta be done in foreign code...
%   NB the following NBs are obsolete...
%   NB this code assumes that null jbufs are never returned...
%   NB XputMode is determined solely by JniType, but is needed earlier (for jni_call)
%   NB this proc should be defined for all return types of supported JNI functions
%   (see jni_proto/5)
%   NB 'jsize' is synonymous with 'jint'

jni_convert_primitive_in(boolean, T,	0, Xlo) :-
    (	Xlo == 0
    ->	T = @(false)
    ;	Xlo == 1
    ->	T = @(true)
    ).
jni_convert_primitive_in(byte,	   I,	0,   I).
jni_convert_primitive_in(char,	   I,	0,   I).
jni_convert_primitive_in(short,   I,	0,   I).
jni_convert_primitive_in(int,	   I,	0,   I).
jni_convert_primitive_in(long,	   T, Xhi, Xlo) :-
    (	(   Xhi == 0,
	    Xlo >= 0	%  small non-negative
	;   Xhi == -1,
	    Xlo < 0	%  small negative
	)
    ->	T = Xlo
    ;	T = jlong(Xhi,Xlo)
    ).
jni_convert_primitive_in(float,   F,	0,   F).
jni_convert_primitive_in(double,  F,	0,   F).

%------------------------------------------------------------------------------

% jni_convert_primitive_out(+JspType, +Term, -Xhi, -Xlo, -JniXputCode) :-
%   to pass Term to a JNI function as (primitive) JspType,
%   send Xhi, Xlo with JniXputCode
%   NB all this oughta be done in foreign code...
%   NB JniXputCode is determined by both JspType and Term
%   NB JniXputCode determines reassembly, widening and casting in foreign code

jni_convert_primitive_out(boolean, @(false),	   0,  0,  1).	    % JNI_XPUT_BOOLEAN

jni_convert_primitive_out(boolean, @(true),	   0,  1,  1).	    % JNI_XPUT_BOOLEAN

jni_convert_primitive_out(char,    I,		   0,  I,  3) :-    % JNI_XPUT_CHAR
    integer(I),
    I >=     0,
    I =< 65535.

jni_convert_primitive_out(byte,    I,		   0,  I,  2) :-    % JNI_XPUT_BYTE
    integer(I),
    I >= -128,
    I =<  127.

jni_convert_primitive_out(short,   I,		   0,  I,  4) :-    % JNI_XPUT_SHORT
    integer(I),
    I >= -32768,
    I =<  32767.

jni_convert_primitive_out(int,	    I,		   0,  I,  5) :-    % JNI_XPUT_INT
    integer(I),
    I >= -2147483648,
    I =<  2147483647.

jni_convert_primitive_out(long,    I,		  V2,  I,  6) :-    % JNI_XPUT_LONG
    integer(I),
    (	I < 0
    ->	V2 = -1
    ;	V2 = 0
    ).

jni_convert_primitive_out(long,    jlong(V2,V1), V2, V1,  6) :-    % JNI_XPUT_LONG
    integer(V1),
    integer(V2).

jni_convert_primitive_out(float,   F,		   0,  F,  7) :-    % JNI_XPUT_FLOAT
    float(F).

jni_convert_primitive_out(float,   I,		   0,  F,  7) :-    % JNI_XPUT_FLOAT
    integer(I),
    F is float(I).

jni_convert_primitive_out(float,   jlong(V2,V1), V2, V1, 10) :-    % JNI_XPUT_LONG_TO_FLOAT
    integer(V1),
    integer(V2).

jni_convert_primitive_out(double,  I,		   0,  F,  9) :-    % JNI_XPUT_FLOAT_TO_DOUBLE
    integer(I),
    F is float(I).

jni_convert_primitive_out(double,  jlong(V2,V1), V2, V1, 11) :-    % JNI_XPUT_LONG_TO_DOUBLE
    integer(V1),
    integer(V2).

jni_convert_primitive_out(double,  F,		   0,  F,  9) :-    % JNI_XPUT_FLOAT_TO_DOUBLE
    float(F).

%------------------------------------------------------------------------------

% jni_params_put(+Args, +Types, -ArgBuf)  :-
%   the old form used a static buffer, hence was not re-entrant;
%   the new form allocates a buffer of one jvalue per arg,
%   puts the (converted) args into respective elements, then returns it
%   (the caller is responsible for freeing it)

%jni_params_put(As, Ts)  :-
%   jni_ensure_jvm,			% in case e.g. NewStringUTF() is called
%   jni_params_put_1(As, 0, Ts).


jni_params_put(As, Ts, ArgBuf)	 :-
    jni_ensure_jvm,			% in case e.g. NewStringUTF() is called
    length(As, N),
    jni_type_to_xput_code(jvalue, Xc), % Xc will be 15
    jni_alloc_buffer(Xc, N, ArgBuf),
    jni_params_put_1(As, 0, Ts, ArgBuf).

%------------------------------------------------------------------------------

% jni_params_put_1(+Args, +N, +JPLTypes, +ArgBuf) :-
%   Args is a (full or partial) list of args-not-yet-stashed,
%   and Types are their (JPL) types (e.g. 'boolean');
%   N is the arg and buffer index (0+) at which the head of Args is to be stashed;
%   the old form used a static buffer and hence was non-reentrant;
%   the new form uses a dynamically allocated buffer (which oughta be freed after use)
%
%NB if the (user-provided) actual params were to be unsuitable for conversion
%NB to the method-required types, this would fail silently (without freeing the buffer);
%NB it's not clear whether the overloaded-method-resolution ensures that all args
%NB are convertible

%jni_params_put_1([], _, []).

%jni_params_put_1([A|As], N, [Tjni|Ts]) :-		% type checking?
%   N2 is N+1,
%   jni_params_put_1(As, N2, Ts),			% stash last param first
%   (	jni_type_to_xput_code(Tjni, Xc)
%   ->	jni_param_put(N, Xc, A)			% foreign
%   ;	fail						% oughta raise an exception?
%   ).


jni_params_put_1([], _, [], _).

jni_params_put_1([A|As], N, [Tjni|Ts], ArgBuf) :-	% type checking?
    (	jni_type_to_xput_code(Tjni, Xc)
    ->	jni_param_put(N, Xc, A, ArgBuf)		% foreign
    ;	fail						% oughta raise an exception?
    ),
    N2 is N+1,
    jni_params_put_1(As, N2, Ts, ArgBuf).		% stash remaining params (if any)

%------------------------------------------------------------------------------

% jni_type_to_xput_code(+JspType, -JniXputCode) :-
%   NB JniXputCode determines widening and casting in foreign code
%   NB the codes could be compiled into jni_method_spec_cache etc.
%   instead of, or as well as, types (for - small - efficiency gain)

jni_type_to_xput_code(boolean,	    1).	    % JNI_XPUT_BOOLEAN

jni_type_to_xput_code(byte,	    2).	    % JNI_XPUT_BYTE

jni_type_to_xput_code(char,	    3).	    % JNI_XPUT_CHAR

jni_type_to_xput_code(short,	    4).	    % JNI_XPUT_SHORT

jni_type_to_xput_code(int,	    5).	    % JNI_XPUT_INT

jni_type_to_xput_code(long,	    6).	    % JNI_XPUT_LONG

jni_type_to_xput_code(float,	    7).	    % JNI_XPUT_FLOAT

jni_type_to_xput_code(double,	    8).	    % JNI_XPUT_DOUBLE

jni_type_to_xput_code(class(_,_), 12).	    % JNI_XPUT_REF

jni_type_to_xput_code(array(_),   12).	    % JNI_XPUT_REF

jni_type_to_xput_code(jvalue,	   15).	    % JNI_XPUT_JVALUE

%------------------------------------------------------------------------------

% jpl_class_to_constructor_array(+Class, -MethodArray) :-
%   might this be done more efficiently in foreign code?

jpl_class_to_constructor_array(Cx, Ma) :-
    jpl_classname_to_class('java.lang.Class', CC),	% cached?
    jGetMethodID(
	CC,
	getConstructors,
	method([],array(class([java,lang,reflect],['Constructor']))),
	MID
    ),
    jCallObjectMethod(Cx, MID, [], [], Ma).

%------------------------------------------------------------------------------

% jpl_class_to_constructors(+Class, -Methods) :-

jpl_class_to_constructors(Cx, Ms) :-
    jpl_class_to_constructor_array(Cx, Ma),
    jpl_object_array_to_list(Ma, Ms).

%------------------------------------------------------------------------------

% jpl_class_to_field_array(+Class, -FieldArray) :-

jpl_class_to_field_array(Cx, Fa) :-
    jpl_classname_to_class('java.lang.Class', CC),
    jGetMethodID(
	CC,
	getFields,
	method([],array(class([java,lang,reflect],['Field']))),
	MID
    ),
    jCallObjectMethod(Cx, MID, [], [], Fa).

%------------------------------------------------------------------------------

% jpl_class_to_fields(+Class, -Fields) :-

jpl_class_to_fields(C, Fs) :-
    jpl_class_to_field_array(C, Fa),
    jpl_object_array_to_list(Fa, Fs).

%------------------------------------------------------------------------------

% jpl_class_to_method_array(+Class, -MethodArray) :-
%   migrate into foreign code for efficiency?

jpl_class_to_method_array(Cx, Ma) :-
    jpl_classname_to_class('java.lang.Class', CC),	% cached?
    jGetMethodID(
	CC,
	getMethods,
	method([],array(class([java,lang,reflect],['Method']))),
	MID
    ),
    jCallObjectMethod(Cx, MID, [], [], Ma).

%------------------------------------------------------------------------------

% jpl_class_to_methods(+Class, -Methods) :-

jpl_class_to_methods(Cx, Ms) :-
    jpl_class_to_method_array(Cx, Ma),
    jpl_object_array_to_list(Ma, Ms).

%------------------------------------------------------------------------------

% jpl_constructor_to_modifiers(+Method, -Modifiers) :-
%   migrate into foreign code for efficiency?

jpl_constructor_to_modifiers(X, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Constructor', Cx),   % cached?
    jpl_method_to_modifiers_1(X, Cx, Ms).

%------------------------------------------------------------------------------

% jpl_constructor_to_name(+Method, -Name) :-
%   it is a JNI convention that each constructor behaves (at least,
%   for reflection), as a method whose name is '<init>'

jpl_constructor_to_name(_X, '<init>').

%------------------------------------------------------------------------------

% jpl_constructor_to_parameter_types(+Method, -ParameterTypes) :-
%   migrate to foreign code for efficiency?

jpl_constructor_to_parameter_types(X, Tps) :-
    jpl_classname_to_class('java.lang.reflect.Constructor', Cx),   % cached?
    jpl_method_to_parameter_types_1(X, Cx, Tps).

%------------------------------------------------------------------------------

% jpl_constructor_to_return_type(+Method, -Type) :-
%   it is a JNI convention that, for the purposes of retrieving a MethodID,
%   a constructor has a return type of 'void'

jpl_constructor_to_return_type(_X, void).

%------------------------------------------------------------------------------

% jpl_field_spec(+ClassType, -Index, -Name, -Modifiers, -MID, -Type) :-

jpl_field_spec(Tc, I, N, Mods, MID, T) :-
    (	jpl_field_spec_is_cached(Tc)
    ->	jpl_field_spec_cache(Tc, I, N, Mods, MID, T)
    ;	jpl_type_to_class(Tc, C),
	jpl_class_to_fields(C, Fs),
	jpl_field_spec_1(Fs, Tc),
	assert(jpl_field_spec_is_cached(Tc)),
	jpl_field_spec_cache(Tc, I, N, Mods, MID, T)
    ).

%------------------------------------------------------------------------------

jpl_field_spec_1(Fs, Tc) :-
    (	jpl_type_to_class(Tc, C),
	nth1(I, Fs, F),
	jpl_field_to_name(F, N),
	jpl_field_to_modifiers(F, Mods),
	jpl_field_to_type(F, T),
	(   member(static, Mods)
	->  jGetStaticFieldID(C, N, T, MID)
	;   jGetFieldID(C, N, T, MID)
	),
	assert(jpl_field_spec_cache(Tc,I,N,Mods,MID,T)),
	fail
    ;	true
    ).

%------------------------------------------------------------------------------

:- dynamic jpl_field_spec_cache/6.

%------------------------------------------------------------------------------

:- dynamic jpl_field_spec_is_cached/1.

%------------------------------------------------------------------------------

%type	jpl_field_to_modifiers(object, ordset(modifier))

% jpl_field_to_modifiers(+Field, -Modifiers) :-

jpl_field_to_modifiers(F, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jpl_method_to_modifiers_1(F, Cf, Ms).

%------------------------------------------------------------------------------

% jpl_field_to_name(+Field, -Name) :-

jpl_field_to_name(F, N) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jpl_method_to_name_1(F, Cf, N).

%------------------------------------------------------------------------------

%type	jpl_field_to_type(object, type)

% jpl_field_to_type(+Field, -Type) :-

jpl_field_to_type(F, Tf) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jGetMethodID(Cf, getType, method([],class([java,lang],['Class'])), MID),
    jCallObjectMethod(F, MID, [], [], Cr),
    jpl_class_to_type(Cr, Tf).

%------------------------------------------------------------------------------

%type	jpl_method_spec(type, integer, name, arity, ordset(modifier), method_id, type, list(type))

% jpl_method_spec(+ClassType, -Index, -Name, -Arity, -Modifiers, -MID, -ReturnType, -ParameterTypes) :-
%   generates pertinent details of all accessible methods of ClassType,
%   populating or using the cache as appropriate

jpl_method_spec(Tc, I, N, A, Mods, MID, Tr, Tps) :-
    (	jpl_method_spec_is_cached(Tc)
    ->	jpl_method_spec_cache(Tc, I, N, A, Mods, MID, Tr, Tps)
    ;	jpl_type_to_class(Tc, Cx),
	jpl_class_to_constructors(Cx, Xs),
	jpl_class_to_methods(Cx, Ms),
	jpl_method_spec_1(Tc, Xs, Ms),
	assert(jpl_method_spec_is_cached(Tc)),
	jpl_method_spec_cache(Tc, I, N, A, Mods, MID, Tr, Tps)
    ).

%------------------------------------------------------------------------------

%type	jpl_method_spec_1(type, list(method), list(method))

jpl_method_spec_1(Tc, Xs, Ms) :-
    (	jpl_type_to_class(Tc, C),
	(   nth1(I, Xs, X),	% generate constructors, numbered from 1
	    jpl_constructor_to_name(X, N),
	    jpl_constructor_to_modifiers(X, Mods),
	    jpl_constructor_to_return_type(X, Tr),
	    jpl_constructor_to_parameter_types(X, Tps)
	;   length(Xs, J0),
	    nth1(J, Ms, M),	% generate members, continuing numbering
	    I is J0+J,
	    jpl_method_to_name(M, N),
	    jpl_method_to_modifiers(M, Mods),
	    jpl_method_to_return_type(M, Tr),
	    jpl_method_to_parameter_types(M, Tps)
	),
	length(Tps, A),	% arity
	(   member(static, Mods)
	->  jGetStaticMethodID(C, N, method(Tps,Tr), MID)
	;   jGetMethodID(C, N, method(Tps,Tr), MID)
	),
	assert(jpl_method_spec_cache(Tc,I,N,A,Mods,MID,Tr,Tps)),
	fail
    ;	true
    ).

%------------------------------------------------------------------------------

:- dynamic jpl_method_spec_cache/8.

%------------------------------------------------------------------------------

:- dynamic jpl_method_spec_is_cached/1.

%------------------------------------------------------------------------------

% jpl_method_to_modifiers(+Method, -ModifierSet) :-

jpl_method_to_modifiers(M, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jpl_method_to_modifiers_1(M, Cm, Ms).

%------------------------------------------------------------------------------

%type	jpl_method_to_modifiers_1(object, object, ordset(modifier))

% jpl_method_to_modifiers_1(+Method, +ConstructorClass, -ModifierSet) :-

jpl_method_to_modifiers_1(XM, Cxm, Ms) :-
    jGetMethodID(Cxm, getModifiers, method([],int), MID),
    jCallIntMethod(XM, MID, [], [], I),
    jpl_modifier_int_to_modifiers(I, Ms).

%------------------------------------------------------------------------------

% jpl_method_to_name(+Method, -Name) :-

jpl_method_to_name(M, N) :-
    jpl_classname_to_class('java.lang.reflect.Method', CM),
    jpl_method_to_name_1(M, CM, N).

%------------------------------------------------------------------------------

jpl_method_to_name_1(M, CM, N) :-
    jGetMethodID(CM, getName, method([],class([java,lang],['String'])), MID),
    jCallObjectMethod(M, MID, [], [], N).

%------------------------------------------------------------------------------

% jpl_method_to_parameter_types(+Method, -Types) :-

jpl_method_to_parameter_types(M, Tps) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jpl_method_to_parameter_types_1(M, Cm, Tps).

%------------------------------------------------------------------------------

jpl_method_to_parameter_types_1(XM, Cxm, Tps) :-
    jGetMethodID(Cxm, getParameterTypes, method([],array(class([java,lang],['Class']))), MID),
    jCallObjectMethod(XM, MID, [], [], Atp),
    jpl_object_array_to_list(Atp, Ctps),
    jpl_classes_to_types(Ctps, Tps).

%------------------------------------------------------------------------------

% jpl_method_to_return_type(+Method, -Type) :-

jpl_method_to_return_type(M, Tr) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jGetMethodID(Cm, getReturnType, method([],class([java,lang],['Class'])), MID),
    jCallObjectMethod(M, MID, [], [], Cr),
    jpl_class_to_type(Cr, Tr).

%------------------------------------------------------------------------------

jpl_modifier_bit(public,	0x001).
jpl_modifier_bit(private,	0x002).
jpl_modifier_bit(protected,	0x004).
jpl_modifier_bit(static,	0x008).
jpl_modifier_bit(final,		0x010).
jpl_modifier_bit(synchronized,	0x020).
jpl_modifier_bit(volatile,	0x040).
jpl_modifier_bit(transient,	0x080).
jpl_modifier_bit(native,	0x100).
jpl_modifier_bit(interface,	0x200).
jpl_modifier_bit(abstract,	0x400).

%------------------------------------------------------------------------------

%type	jpl_modifier_int_to_modifiers(integer, ordset(modifier))

% jpl_modifier_int_to_modifiers(+Int, -ModifierSet) :-
%   ModifierSet is an ordered (hence canonical) list,
%   possibly empty (although I suspect never in practice?),
%   of modifier atoms, e.g. [public,static]

jpl_modifier_int_to_modifiers(I, Ms) :-
    setof(
	M,				    %  should use e.g. set_of_all/3
	B^(jpl_modifier_bit(M, B),
	    (B /\ I) =\= 0
	),
	Ms
    ).

%------------------------------------------------------------------------------

% jpl_servlet_byref(+Config, +Request, +Response) :-

jpl_servlet_byref(Config, Request, Response) :-
    jpl_call(Config, getServletContext, [], Context),

    jpl_call(Response, setStatus, [200], _),
    jpl_call(Response, setContentType, ['text/html'], _),
    jpl_call(Response, getWriter, [], W),

    jpl_call(W, println, ['<html><head></head><body><h2>jpl_servlet_byref/3 says:</h2><pre>'], _),

    jpl_call(W, println, ['\nservlet context stuff:'], _),

    jpl_call(Context, getInitParameterNames, [], ContextInitParameterNameEnum),
    jpl_enumeration_to_list(ContextInitParameterNameEnum, ContextInitParameterNames),
    length(ContextInitParameterNames, NContextInitParameterNames),
    concat_atom(['\tContext.InitParameters = ',NContextInitParameterNames], NContextInitParameterNamesMsg),
    jpl_call(W, println, [NContextInitParameterNamesMsg], _),
    (	member(ContextInitParameterName, ContextInitParameterNames),
	jpl_call(Context, getInitParameter, [ContextInitParameterName], ContextInitParameter),
	concat_atom(['\t\tContext.InitParameter[',ContextInitParameterName,'] = ',ContextInitParameter], ContextInitParameterMsg),
	jpl_call(W, println, [ContextInitParameterMsg], _),
	fail
    ;	true
    ),

    jpl_call(Context, getMajorVersion, [], MajorVersion),
    concat_atom(['\tContext.MajorVersion = ',MajorVersion], MajorVersionMsg),
    jpl_call(W, println, [MajorVersionMsg], _),

    jpl_call(Context, getMinorVersion, [], MinorVersion),
    concat_atom(['\tContext.MinorVersion = ',MinorVersion], MinorVersionMsg),
    jpl_call(W, println, [MinorVersionMsg], _),

    jpl_call(Context, getServerInfo, [], ServerInfo),
    concat_atom(['\tContext.ServerInfo = ',ServerInfo], ServerInfoMsg),
    jpl_call(W, println, [ServerInfoMsg], _),

    jpl_call(W, println, ['\nservlet config stuff:'], _),

    jpl_call(Config, getServletName, [], ServletName),
    (	ServletName == @(null)
    ->	ServletNameAtom = null
    ;	ServletNameAtom = ServletName
    ),
    concat_atom(['\tConfig.ServletName = ',ServletNameAtom], ServletNameMsg),
    jpl_call(W, println, [ServletNameMsg], _),

    jpl_call(Config, getInitParameterNames, [], ConfigInitParameterNameEnum),
    jpl_enumeration_to_list(ConfigInitParameterNameEnum, ConfigInitParameterNames),
    length(ConfigInitParameterNames, NConfigInitParameterNames),
    concat_atom(['\tConfig.InitParameters = ',NConfigInitParameterNames], NConfigInitParameterNamesMsg),
    jpl_call(W, println, [NConfigInitParameterNamesMsg], _),
    (	member(ConfigInitParameterName, ConfigInitParameterNames),
	jpl_call(Config, getInitParameter, [ConfigInitParameterName], ConfigInitParameter),
	concat_atom(['\t\tConfig.InitParameter[',ConfigInitParameterName,'] = ',ConfigInitParameter], ConfigInitParameterMsg),
	jpl_call(W, println, [ConfigInitParameterMsg], _),
	fail
    ;	true
    ),

    jpl_call(W, println, ['\nrequest stuff:'], _),

    jpl_call(Request, getAttributeNames, [], AttributeNameEnum),
    jpl_enumeration_to_list(AttributeNameEnum, AttributeNames),
    length(AttributeNames, NAttributeNames),
    concat_atom(['\tRequest.Attributes = ',NAttributeNames], NAttributeNamesMsg),
    jpl_call(W, println, [NAttributeNamesMsg], _),
    (	member(AttributeName, AttributeNames),
	jpl_call(Request, getAttribute, [AttributeName], Attribute),
	jpl_call(Attribute, toString, [], AttributeString),
	concat_atom(['\t\tRequest.Attribute[',AttributeName,'] = ',AttributeString], AttributeMsg),
	jpl_call(W, println, [AttributeMsg], _),
	fail
    ;	true
    ),

    jpl_call(Request, getCharacterEncoding, [], CharacterEncoding),
    (	CharacterEncoding == @(null)
    ->	CharacterEncodingAtom = ''
    ;	CharacterEncodingAtom = CharacterEncoding
    ),
    concat_atom(['\tRequest.CharacterEncoding',' = ',CharacterEncodingAtom], CharacterEncodingMsg),
    jpl_call(W, println, [CharacterEncodingMsg], _),

    jpl_call(Request, getContentLength, [], ContentLength),
    concat_atom(['\tRequest.ContentLength',' = ',ContentLength], ContentLengthMsg),
    jpl_call(W, println, [ContentLengthMsg], _),

    jpl_call(Request, getContentType, [], ContentType),
    (	ContentType == @(null)
    ->	ContentTypeAtom = ''
    ;	ContentTypeAtom = ContentType
    ),
    concat_atom(['\tRequest.ContentType',' = ',ContentTypeAtom], ContentTypeMsg),
    jpl_call(W, println, [ContentTypeMsg], _),

    jpl_call(Request, getParameterNames, [], ParameterNameEnum),
    jpl_enumeration_to_list(ParameterNameEnum, ParameterNames),
    length(ParameterNames, NParameterNames),
    concat_atom(['\tRequest.Parameters = ',NParameterNames], NParameterNamesMsg),
    jpl_call(W, println, [NParameterNamesMsg], _),
    (	member(ParameterName, ParameterNames),
	jpl_call(Request, getParameter, [ParameterName], Parameter),
	concat_atom(['\t\tRequest.Parameter[',ParameterName,'] = ',Parameter], ParameterMsg),
	jpl_call(W, println, [ParameterMsg], _),
	fail
    ;	true
    ),

    jpl_call(Request, getProtocol, [], Protocol),
    concat_atom(['\tRequest.Protocol',' = ',Protocol], ProtocolMsg),
    jpl_call(W, println, [ProtocolMsg], _),

    jpl_call(Request, getRemoteAddr, [], RemoteAddr),
    concat_atom(['\tRequest.RemoteAddr',' = ',RemoteAddr], RemoteAddrMsg),
    jpl_call(W, println, [RemoteAddrMsg], _),

    jpl_call(Request, getRemoteHost, [], RemoteHost),
    concat_atom(['\tRequest.RemoteHost',' = ',RemoteHost], RemoteHostMsg),
    jpl_call(W, println, [RemoteHostMsg], _),

    jpl_call(Request, getScheme, [], Scheme),
    concat_atom(['\tRequest.Scheme',' = ',Scheme], SchemeMsg),
    jpl_call(W, println, [SchemeMsg], _),

    jpl_call(Request, getServerName, [], ServerName),
    concat_atom(['\tRequest.ServerName',' = ',ServerName], ServerNameMsg),
    jpl_call(W, println, [ServerNameMsg], _),

    jpl_call(Request, getServerPort, [], ServerPort),
    concat_atom(['\tRequest.ServerPort',' = ',ServerPort], ServerPortMsg),
    jpl_call(W, println, [ServerPortMsg], _),

    jpl_call(Request, isSecure, [], @(Secure)),
    concat_atom(['\tRequest.Secure',' = ',Secure], SecureMsg),
    jpl_call(W, println, [SecureMsg], _),

    jpl_call(W, println, ['\nHTTP request stuff:'], _),

    jpl_call(Request, getAuthType, [], AuthType),
    (	AuthType == @(null)
    ->	AuthTypeAtom = ''
    ;	AuthTypeAtom = AuthType
    ),
    concat_atom(['\tRequest.AuthType',' = ',AuthTypeAtom], AuthTypeMsg),
    jpl_call(W, println, [AuthTypeMsg], _),

    jpl_call(Request, getContextPath, [], ContextPath),
    (	ContextPath == @(null)
    ->	ContextPathAtom = ''
    ;	ContextPathAtom = ContextPath
    ),
    concat_atom(['\tRequest.ContextPath',' = ',ContextPathAtom], ContextPathMsg),
    jpl_call(W, println, [ContextPathMsg], _),

    jpl_call(Request, getCookies, [], CookieArray),
    (	CookieArray == @(null)
    ->	Cookies = []
    ;	jpl_array_to_list(CookieArray, Cookies)
    ),
    length(Cookies, NCookies),
    concat_atom(['\tRequest.Cookies',' = ',NCookies], NCookiesMsg),
    jpl_call(W, println, [NCookiesMsg], _),
    (	nth0(NCookie, Cookies, Cookie),
	concat_atom(['\t\tRequest.Cookie[',NCookie,']'], CookieMsg),
	jpl_call(W, println, [CookieMsg], _),

	jpl_call(Cookie, getName, [], CookieName),
	concat_atom(['\t\t\tRequest.Cookie.Name = ',CookieName], CookieNameMsg),
	jpl_call(W, println, [CookieNameMsg], _),

	jpl_call(Cookie, getValue, [], CookieValue),
	concat_atom(['\t\t\tRequest.Cookie.Value = ',CookieValue], CookieValueMsg),
	jpl_call(W, println, [CookieValueMsg], _),

	jpl_call(Cookie, getPath, [], CookiePath),
	(   CookiePath == @(null)
	->  CookiePathAtom = ''
	;   CookiePathAtom = CookiePath
	),
	concat_atom(['\t\t\tRequest.Cookie.Path = ',CookiePathAtom], CookiePathMsg),
	jpl_call(W, println, [CookiePathMsg], _),

	jpl_call(Cookie, getComment, [], CookieComment),
	(   CookieComment == @(null)
	->  CookieCommentAtom = ''
	;   CookieCommentAtom = CookieComment
	),
	concat_atom(['\t\t\tRequest.Cookie.Comment = ',CookieCommentAtom], CookieCommentMsg),
	jpl_call(W, println, [CookieCommentMsg], _),

	jpl_call(Cookie, getDomain, [], CookieDomain),
	(   CookieDomain == @(null)
	->  CookieDomainAtom = ''
	;   CookieDomainAtom = CookieDomain
	),
	concat_atom(['\t\t\tRequest.Cookie.Domain = ',CookieDomainAtom], CookieDomainMsg),
	jpl_call(W, println, [CookieDomainMsg], _),

	jpl_call(Cookie, getMaxAge, [], CookieMaxAge),
	concat_atom(['\t\t\tRequest.Cookie.MaxAge = ',CookieMaxAge], CookieMaxAgeMsg),
	jpl_call(W, println, [CookieMaxAgeMsg], _),

	jpl_call(Cookie, getVersion, [], CookieVersion),
	concat_atom(['\t\t\tRequest.Cookie.Version = ',CookieVersion], CookieVersionMsg),
	jpl_call(W, println, [CookieVersionMsg], _),

	jpl_call(Cookie, getSecure, [], @(CookieSecure)),
	concat_atom(['\t\t\tRequest.Cookie.Secure',' = ',CookieSecure], CookieSecureMsg),
	jpl_call(W, println, [CookieSecureMsg], _),

	fail
    ;	true
    ),

    jpl_call(W, println, ['</pre></body></html>'], _),

    true.

%------------------------------------------------------------------------------

jpl_servlet_byval(MM, CT, Ba) :-
	CT = 'text/html',
	multimap_to_atom(MM, MMa),
	concat_atom(['<html><head></head><body>',
		     '<h2>jpl_servlet_byval/3 says:</h2><pre>',
		     MMa,
		     '</pre></body></html>'
		    ], Ba).

%------------------------------------------------------------------------------

%type	jpl_cache_type_of_ref(jpl_type, ref)

% jpl_cache_type_of_ref(+Type, +Ref) :-
%   Type must be a proper JPL type;
%   Ref must be a proper JPL reference (not void);
%   Type is memoed as the type of the referenced object (unless it's null)
%   by iref (so as not to disable atom-based GC)
%   NB obsolete lemmas must be watched-out-for and removed

jpl_cache_type_of_ref(T, @(Tag)) :-
    (	\+ ground(T)
    ->	write('[jpl_cache_type_of_ref/2: arg 1 is not ground]'), nl,
	fail
    ;	\+ atom(Tag)
    ->	write('[jpl_cache_type_of_ref/2: arg 2 is not an atomic-tag ref]'), nl,
	fail
    ;	Tag == null
    ->	true					% silently ignore null refs
    ;	jni_tag_to_iref(Tag, Iref)
    ->	(   jpl_iref_type_cache(Iref, TC)	% assume TC == T
	->  (	T == TC
	    ->	true
	    ; % write('[JPL: found obsolete tag-type lemma...]'), nl,	% keep statistics? (why?)
		retractall(jpl_iref_type_cache(Iref,_)),
		assert(jpl_iref_type_cache(Iref,T))
	    )
	;   assert(jpl_iref_type_cache(Iref,T))
	)
    ;	write('[jpl_cache_type_of_ref/2: jni_tagatom_to_iref(Tag,_) failed]'), nl,
	fail
    ).

%------------------------------------------------------------------------------

% jpl_class_tag_type_cache(-Tag, -ClassType) :-
%   Tag is the tag part of an @Tag reference
%   to a JVM instance of java.lang.Class
%   which denotes ClassType;
%   we index on Tag rather than on Iref so as to keep these objects around
%   even after an atom garbage collection
%   (if needed once, they are likely to be needed again)

:- dynamic jpl_class_tag_type_cache/2.

%------------------------------------------------------------------------------

% jpl_class_to_ancestor_classes(+Class, -AncestorClasses) :-

jpl_class_to_ancestor_classes(C, Cas) :-
    (	jpl_class_to_super_class(C, Ca)
    ->	Cas = [Ca|Cas2],
	jpl_class_to_ancestor_classes(Ca, Cas2)
    ;	Cas = []
    ).

%------------------------------------------------------------------------------

% jpl_class_to_classname(+Class, -ClassName) :-
%   Class is a reference to a class object;
%   ClassName is its canonical (?) source-syntax (dotted) name,
%   e.g. 'java.util.Date'
%   not used outside jni_junk and jpl_test;
%   oughta use the available caches...

jpl_class_to_classname(C, CN) :-
    jpl_call(C, getName, [], CN).

%------------------------------------------------------------------------------

% jpl_class_to_raw_classname(+Class, -ClassName) :-

jpl_class_to_raw_classname(Cobj, CN) :-
    jpl_classname_to_class('java.lang.Class', CC),	% cached?
    jGetMethodID(CC, getName, method([],class([java,lang],['String'])), MIDgetName),
    jCallObjectMethod(Cobj, MIDgetName, [], [], S),
    S = CN.

%------------------------------------------------------------------------------

% jpl_class_to_raw_classname_chars(+Class, -ClassnameChars) :-
%   Class is a reference to a class object;
%   ClassnameChars is a chars representation of its dotted name, e.g.
%   "java.util.Date"

jpl_class_to_raw_classname_chars(Cobj, CsCN) :-
    jpl_class_to_raw_classname(Cobj, CN),
    atom_codes(CN, CsCN).

%------------------------------------------------------------------------------

jpl_class_to_super_class(C, Cx) :-
    jGetSuperclass(C, Cx),
    Cx \== @(null),	    % as returned when C is java.lang.Object, i.e. no superclass
    jpl_cache_type_of_ref(class([java,lang],['Class']), Cx).	

%------------------------------------------------------------------------------

% jpl_class_to_type(+ClassObject, -Type) :-
%   ClassObject is a reference to a class object of Type
%   NB should ensure that, if not found in cache, then cache is updated;
%   intriguingly (?), getParameterTypes returns class objects with names
%   'boolean', 'byte' etc. and even 'void' (?!)

jpl_class_to_type(@(Tag), Type) :-
    (	jpl_class_tag_type_cache(Tag, Tx)
    ->	true
    ;	jpl_class_to_raw_classname_chars(@(Tag), Cs),	% uncached
	jpl_classname_chars_to_type(Cs, Tr),
	jpl_type_to_canonical_type(Tr, Tx),
	assert(jpl_class_tag_type_cache(Tag,Tx))
    ->	true	% the elseif goal should be determinate, but just in case...
    ),
    Type = Tx.

%------------------------------------------------------------------------------

jpl_classes_to_types([], []).

jpl_classes_to_types([C|Cs], [T|Ts]) :-
    jpl_class_to_type(C, T),
    jpl_classes_to_types(Cs, Ts).

%------------------------------------------------------------------------------

jpl_classname_chars_to_type(Cs, Type) :-
    (	phrase(jpl_type_classname_1(Type), Cs)
    ->	true
    ).

%------------------------------------------------------------------------------

% jpl_classname_to_class(+ClassName, -Class) :-
%   ClassName unambiguously represents a class,
%   e.g. 'java.lang.String'
%   Class is a (canonical) reference to the corresponding class object

jpl_classname_to_class(N, C) :-
    jpl_classname_to_type(N, T),   % cached
    jpl_type_to_class(T, C).	

%------------------------------------------------------------------------------

% jpl_classname_to_type(+Classname, -Type) :-
%   Classname is a source-syntax (dotted) class name,
%   e.g. 'java.util.Date', '[java.util.Date' or '[L'
%   Type is its corresponding JPL type structure,
%   e.g. class([java,util],['Date']), array(class([java,util],['Date'])), ...

jpl_classname_to_type(CN, T) :-
    (	jpl_classname_type_cache(CN, Tx)
    ->	Tx = T
    ;	atom_codes(CN, CsCN),
	phrase(jpl_type_classname_1(T), CsCN)
    ->	assert(jpl_classname_type_cache(CN,T)),
	true
    ).

%------------------------------------------------------------------------------

:- dynamic jpl_classname_type_cache/2.

%------------------------------------------------------------------------------

% jpl_datum_to_type(+Datum, -Type) :-
%   Datum must be a proper JPL representation
%   of an instance of one (or more) Java types;
%   Type is the unique most specialised type of which Datum denotes an instance;
%   N.B. 3 is an instance of byte, char, short, int and long,
%   of which byte and char are the joint, overlapping most specialised types,
%   so this relates 3 to the pseudo subtype 'char_byte'

jpl_datum_to_type(D, T) :-
    (	jpl_value_to_type(D, T)
    ->	true
    ;	jpl_ref_to_type(D, T)
    ).

%------------------------------------------------------------------------------

jpl_datums_to_most_specific_common_ancestor_type([D], T) :-
    jpl_datum_to_type(D, T).

jpl_datums_to_most_specific_common_ancestor_type([D1,D2|Ds], T0) :-
    jpl_datum_to_type(D1, T1),
    jpl_type_to_ancestor_types(T1, Ts1),
    jpl_datums_to_most_specific_common_ancestor_type_1([D2|Ds], [T1|Ts1], [T0|_]).

%------------------------------------------------------------------------------

jpl_datums_to_most_specific_common_ancestor_type_1([], Ts, Ts).

jpl_datums_to_most_specific_common_ancestor_type_1([D|Ds], Ts1, Ts0) :-
    jpl_datum_to_type(D, Tx),
    jpl_lineage_types_type_to_common_lineage_types(Ts1, Tx, Ts2),
    jpl_datums_to_most_specific_common_ancestor_type_1(Ds, Ts2, Ts0).

%------------------------------------------------------------------------------

% jpl_datums_to_types(+Datums, -Types) :-
%   each member of Datums is a JPL value or ref,
%   denoting an instance of some Java type,
%   and the corresponding member of Types denotes the most specialised type
%   of which it is an instance (including some I invented for the overlaps
%   between char and short, etc,)

jpl_datums_to_types([], []).

jpl_datums_to_types([D|Ds], [T|Ts]) :-
    jpl_datum_to_type(D, T),
    jpl_datums_to_types(Ds, Ts).

%------------------------------------------------------------------------------

% jpl_false(-X) :-
%   X is (by unification) the proper JPL datum which represents the Java value 'false'
%   c.f. jpl_is_false/1

jpl_false(@(false)).

%------------------------------------------------------------------------------

% jpl_ground_is_type(+X) :-
%   X, known to be ground, is (or at least superficially resembles :-) a JPL type

jpl_ground_is_type(X) :-
    jpl_primitive_type(X),
    !.

jpl_ground_is_type(array(X)) :-
    jpl_ground_is_type(X).

jpl_ground_is_type(class(_,_)).

jpl_ground_is_type(method(_,_)).

%------------------------------------------------------------------------------

:- dynamic jpl_iref_type_cache/2.

%------------------------------------------------------------------------------

% jpl_is_class(?X) :-
%   X is a JPL ref to a java.lang.Class object

jpl_is_class(X) :-
    jpl_is_object(X),
    jpl_object_to_type(X, class([java,lang],['Class'])).

%------------------------------------------------------------------------------

% jpl_is_false(?X) :-
%   X is the proper JPL datum which represents the Java value 'false';
%   whatever, no further instantiation of X occurs

jpl_is_false(X) :-
    X == @(false).

%------------------------------------------------------------------------------

% jpl_is_fieldID(?X) :-
%   X is a proper JPL field ID structure (jfieldID/1);
%   applications should not be messing with these (?);
%   whatever, no further instantiation of X occurs

jpl_is_fieldID(jfieldID(X)) :-	    % NB a var arg may get bound...
    integer(X).

%------------------------------------------------------------------------------

% jpl_is_methodID(?X) :-
%   X is a proper JPL method ID structure (jmethodID/1);
%   applications should not be messing with these (?);
%   whatever, no further instantiation of X occurs

jpl_is_methodID(jmethodID(X)) :-   % NB a var arg may get bound...
    integer(X).

%------------------------------------------------------------------------------

% jpl_is_null(?X) :-
%   X is the proper JPL datum which represents Java's 'null' reference;
%   whatever, no further instantiation of X occurs

jpl_is_null(X) :-
    X == @(null).

%------------------------------------------------------------------------------

% jpl_is_object(?X) :-
%   X is a proper, plausible JPL object reference;
%   NB this checks only syntax, not whether the object exists;
%   whatever, no further instantiation of X occurs

jpl_is_object(X) :-
    jpl_is_ref(X),	% (syntactically, at least...)
    X \== @(null).

%------------------------------------------------------------------------------

% jpl_is_object_type(+T) :-
%   T is an object (class or array) type,
%   not e.g. a primitive, null or void

jpl_is_object_type(T) :-
    \+ var(T),
    jpl_non_var_is_object_type(T).

%------------------------------------------------------------------------------

% jpl_is_ref(?T) :-
%   the arbitrary term T is a proper, syntactically plausible JPL reference,
%   either to a Java object
%   (which may not exist, although a jpl_is_current_ref/1 might be useful)
%   or to Java's notional but important 'null' non-object;
%   whatever, no further instantiation of X occurs;
%   NB to distinguish tags from void/false/true,
%   could check initial character(s) or length? or adopt strong/weak scheme...

jpl_is_ref(@(Y)) :-
    atom(Y),	    % presumably a (garbage-collectable) tag
    Y \== void,	    % not a ref
    Y \== false,    % not a ref
    Y \== true.	    % not a ref

%------------------------------------------------------------------------------

% jpl_is_true(?X) :-
%   X is a proper JPL datum, representing the Java value 'true';
%   whatever, no further instantiation of X occurs

jpl_is_true(X) :-
    X == @(true).

%------------------------------------------------------------------------------

% jpl_is_type(+X) :-

jpl_is_type(X) :-
    ground(X),
    jpl_ground_is_type(X).

%------------------------------------------------------------------------------

% jpl_is_void(?X) :-
%   X is the proper JPL datum which represents the pseudo Java value 'void'
%   (which is returned by jpl_call/4 when invoked on void methods);
%   NB you can try passing 'void' back to Java, but it won't ever be interested;
%   whatever, no further instantiation of X occurs

jpl_is_void(X) :-
    X == @(void).

%------------------------------------------------------------------------------

jpl_lineage_types_type_to_common_lineage_types(Ts, Tx, Ts0) :-
    (	append(_, [Tx|Ts2], Ts)
    ->	[Tx|Ts2] = Ts0
    ;	jpl_type_to_super_type(Tx, Tx2)
    ->	jpl_lineage_types_type_to_common_lineage_types(Ts, Tx2, Ts0)
    ).

%------------------------------------------------------------------------------

jpl_non_var_is_object_type(class(_,_)).

jpl_non_var_is_object_type(array(_)).

%------------------------------------------------------------------------------

% jpl_null(-X) :-
%   X is (by unification) the proper JPL datum which represents the Java reference 'null';
%   c.f. jpl_is_null/1

jpl_null(@(null)).

%------------------------------------------------------------------------------

% jpl_object_array_to_list(+ArrayObject, -Values) :-
%   Values is a list of JPL values (primitive values or object references)
%   representing the respective elements of ArrayObject

jpl_object_array_to_list(A, Vs) :-
    jpl_array_to_length(A, N),
    jpl_object_array_to_list_1(A, 0, N, Vs).

%------------------------------------------------------------------------------

% jpl_object_array_to_list_1(+A, +I, +N, -Xs) :-

jpl_object_array_to_list_1(A, I, N, Xs) :-
    (	I == N
    ->	Xs = []
    ;	jGetObjectArrayElement(A, I, X),
	Xs = [X|Xs2],
	J is I+1,
	jpl_object_array_to_list_1(A, J, N, Xs2)
    ).

%------------------------------------------------------------------------------

% jpl_object_to_class(+Object, -Class) :-
%   Class is a (canonical) reference to the (canonical) class object
%   which represents the (most specific) class of Object
%   NB Object must be a valid object, else...
%   NB wot's the point of caching the type if we don't look there first?

jpl_object_to_class(Obj, C) :-
    jGetObjectClass(Obj, C),
    jpl_cache_type_of_ref(class([java,lang],['Class']), C).

%------------------------------------------------------------------------------

% jpl_object_to_type(+Object, -Type) :-
%   Object must be a proper JPL reference to a Java object
%   (i.e. a class or array instance, but not null, void or String);
%   Type is the JPL type of that object

jpl_object_to_type(@(Tag), Type) :-
    jpl_tag_to_type(Tag, Type).

%------------------------------------------------------------------------------

jpl_object_type_to_super_type(T, Tx) :-
    (	(   T = class(_,_)
	;   T = array(_)
	)
    ->	jpl_type_to_class(T, C),
	jpl_class_to_super_class(C, Cx),
	Cx \== @(null),
	jpl_class_to_type(Cx, Tx)
    ).

%------------------------------------------------------------------------------

% jpl_primitive_buffer_to_array(+Type, +Xc, +Bp, +I, +Size, -Vcs) :-
%   Bp points to a buffer of (sufficient) Type values;
%   Vcs will be unbound on entry,
%   and on exit will be a list of Size of them, starting at index I
%   (the buffer is indexed from zero)

jpl_primitive_buffer_to_array(T, Xc, Bp, I, Size, [Vc|Vcs]) :-
    jni_fetch_buffer_value(Bp, I, Xhi, Xlo, Xc),
    jni_convert_primitive_in(T, Vc, Xhi, Xlo),
    Ix is I+1,
    (	Ix < Size
    ->	jpl_primitive_buffer_to_array(T, Xc, Bp, Ix, Size, Vcs)
    ;	Vcs = []
    ).

%------------------------------------------------------------------------------

jpl_primitive_type(boolean).
jpl_primitive_type(char).
jpl_primitive_type(byte).
jpl_primitive_type(short).
jpl_primitive_type(int).
jpl_primitive_type(long).
jpl_primitive_type(float).
jpl_primitive_type(double).

%------------------------------------------------------------------------------

% jpl_primitive_type_default_value(-Type, -Value) :-
%   each element of any array of (primitive) Type created by jpl_new/3,
%   or any instance of (primitive) Type created by jpl_new/3,
%   should be initialised to Value (to mimic Java semantics)

jpl_primitive_type_default_value(boolean, @(false)).
jpl_primitive_type_default_value(char,	   0).
jpl_primitive_type_default_value(byte,	   0).
jpl_primitive_type_default_value(short,   0).
jpl_primitive_type_default_value(int,	   0).
jpl_primitive_type_default_value(long,	   0).
jpl_primitive_type_default_value(float,   0.0).
jpl_primitive_type_default_value(double,  0.0).

%------------------------------------------------------------------------------

jpl_primitive_type_super_type(T, Tx) :-
    (	jpl_type_fits_type_direct_prim(T, Tx)
    ;	jpl_type_fits_type_direct_xtra(T, Tx)
    ).

%------------------------------------------------------------------------------

% jpl_primitive_type_term_to_value(+Type, +Term, -Val) :-
%   Term, after widening iff appropriate, represents an instance of Type;
%   Val is the instance of Type which it represents (often the same thing);
%   currently used only by jpl_new_1 when creating an "instance"
%   of a primitive type (which may be misguided completism - you can't
%   do that in Java)

jpl_primitive_type_term_to_value(Type, Term, Val) :-
    (	jpl_primitive_type_term_to_value_1(Type, Term, Val)
    ->	true
    ).

%------------------------------------------------------------------------------

% jpl_primitive_type_term_to_value_1(+Type, +RawValue, -WidenedValue) :-
%   I'm not worried about structure duplication here
%   NB this oughta be done in foreign code...

jpl_primitive_type_term_to_value_1(boolean, @(false), @(false)).

jpl_primitive_type_term_to_value_1(boolean, @(true), @(true)).

jpl_primitive_type_term_to_value_1(char, I, I) :-
    integer(I),
    I >= 0,
    I =< 65535.

jpl_primitive_type_term_to_value_1(byte, I, I) :-
    integer(I),
    I >= -128,
    I =<  127.

jpl_primitive_type_term_to_value_1(short, I, I) :-
    integer(I),
    I >= -32768,
    I =<  32767.

jpl_primitive_type_term_to_value_1(int, I, I) :-
    integer(I).

jpl_primitive_type_term_to_value_1(long, I, I) :-
    integer(I).

jpl_primitive_type_term_to_value_1(long, jlong(Xhi,Xlo), jlong(Xhi,Xlo)) :-
    integer(Xhi),
    integer(Xlo).

jpl_primitive_type_term_to_value_1(float, I, F) :-
    integer(I),
    F is float(I).

jpl_primitive_type_term_to_value_1(float, F, F) :-
    float(F).

jpl_primitive_type_term_to_value_1(double, I, F) :-
    integer(I),
    F is float(I).

jpl_primitive_type_term_to_value_1(double, F, F) :-
    float(F).

%------------------------------------------------------------------------------

jpl_primitive_type_to_ancestor_types(T, Ts) :-
    (	jpl_primitive_type_super_type(T, Ta)
    ->	Ts = [Ta|Tas],
	jpl_primitive_type_to_ancestor_types(Ta, Tas)
    ;	Ts = []
    ).

%------------------------------------------------------------------------------

jpl_primitive_type_to_super_type(T, Tx) :-
    jpl_primitive_type_super_type(T, Tx).

%------------------------------------------------------------------------------

% jpl_ref_to_type(+Ref, -Type) :-
%   Ref must be a proper JPL reference (to an object, null or void);
%   Type is its type

jpl_ref_to_type(@(X), T) :-
    (	X == null
    ->	T = null
    ;	X == void
    ->	T = void
    ;	jpl_tag_to_type(X, T)
    ).

%------------------------------------------------------------------------------

% jpl_tag_to_type(+Tag, -Type) :-
%   Tag must be an (atomic) object tag;
%   Type is its type (either from the cache or by reflection);

jpl_tag_to_type(Tag, Type) :-
    jni_tag_to_iref(Tag, Iref),
    (	jpl_iref_type_cache(Iref, T)
    ->	true				    % T is Tag's type
    ;	jpl_object_to_class(@(Tag), Cobj), % else get ref to class obj
	jpl_class_to_type(Cobj, T),	    % get type of class it denotes
	assert(jpl_iref_type_cache(Iref,T))
    ),
    Type = T.

%------------------------------------------------------------------------------

% jpl_true(-X) :-
%   X is (by unification) the proper JPL datum which represents the Java value 'true';
%   c.f. jpl_is_true/1

jpl_true(@(true)).

%------------------------------------------------------------------------------

jpl_type_fits_type(Tx, Ty) :-
    (	jpl_type_fits_type_1(Tx, Ty)
    ->	true
    ).

%------------------------------------------------------------------------------

% jpl_type_fits_type_1(+T1, +T2) :-
%   it doesn't matter that this leaves choicepoints; it serves only jpl_type_fits_type/2

jpl_type_fits_type_1(T, T).

jpl_type_fits_type_1(class(Ps1,Cs1), class(Ps2,Cs2)) :-
    jpl_type_to_class(class(Ps1,Cs1), C1),
    jpl_type_to_class(class(Ps2,Cs2), C2),
    jIsAssignableFrom(C1, C2).

jpl_type_fits_type_1(array(T1), class(Ps2,Cs2)) :-
    jpl_type_to_class(array(T1), C1),
    jpl_type_to_class(class(Ps2,Cs2), C2),
    jIsAssignableFrom(C1, C2).

jpl_type_fits_type_1(array(T1), array(T2)) :-
    jpl_type_to_class(array(T1), C1),
    jpl_type_to_class(array(T2), C2),
    jIsAssignableFrom(C1, C2).

jpl_type_fits_type_1(null, class(_,_)).

jpl_type_fits_type_1(null, array(_)).

jpl_type_fits_type_1(T1, T2) :-
    jpl_type_fits_type_xprim(T1, T2).

%------------------------------------------------------------------------------

jpl_type_fits_type_direct_prim(float, double).
jpl_type_fits_type_direct_prim(long,  float).
jpl_type_fits_type_direct_prim(int,   long).
jpl_type_fits_type_direct_prim(char,  int).
jpl_type_fits_type_direct_prim(short, int).
jpl_type_fits_type_direct_prim(byte,  short).

%------------------------------------------------------------------------------

jpl_type_fits_type_direct_xprim(Tp, Tq) :-
    jpl_type_fits_type_direct_prim(Tp, Tq).

jpl_type_fits_type_direct_xprim(Tp, Tq) :-
    jpl_type_fits_type_direct_xtra(Tp, Tq).

%------------------------------------------------------------------------------

% jpl_type_fits_type_direct_xtra(pos_int,    int).
  jpl_type_fits_type_direct_xtra(char_int,   int).
  jpl_type_fits_type_direct_xtra(char_int,   char).
  jpl_type_fits_type_direct_xtra(char_short, short).
  jpl_type_fits_type_direct_xtra(char_short, char).
  jpl_type_fits_type_direct_xtra(char_byte,  byte).
  jpl_type_fits_type_direct_xtra(char_byte,  char).
% jpl_type_fits_type_direct_xtra(neg_byte,   byte).
% jpl_type_fits_type_direct_xtra(neg_short,  short).
% jpl_type_fits_type_direct_xtra(neg_int,    int).

%------------------------------------------------------------------------------

% jpl_type_fits_type_xprim(-Tp, -T) :-
%   indeterminate: serves only jpl_type_fits_type_1/2

jpl_type_fits_type_xprim(Tp, T) :-
    jpl_type_fits_type_direct_xprim(Tp, Tq),
    (	Tq = T
    ;	jpl_type_fits_type_xprim(Tq, T)
    ).

%------------------------------------------------------------------------------

% jpl_type_to_ancestor_types(+T, -Tas) :-
%   this fails to accommodate the assignability of null...

jpl_type_to_ancestor_types(T, Tas) :-
    (	(   T = class(_,_)
	;   T = array(_)
	)
    ->	jpl_type_to_class(T, C),
	jpl_class_to_ancestor_classes(C, Cas),
	jpl_classes_to_types(Cas, Tas)
    ;	jpl_primitive_type_to_ancestor_types(T, Tas)
    ->	true
    ).

%------------------------------------------------------------------------------

jpl_type_to_canonical_type(array(T), array(Tc)) :-
    !,
    jpl_type_to_canonical_type(T, Tc).

jpl_type_to_canonical_type(class([],[void]), void) :-
    !.

jpl_type_to_canonical_type(class([],[N]), N) :-
    jpl_primitive_type(N),
    !.

jpl_type_to_canonical_type(class(Ps,Cs), class(Ps,Cs)) :-
    !.

jpl_type_to_canonical_type(void, void) :-
    !.

jpl_type_to_canonical_type(P, P) :-
    jpl_primitive_type(P).

%------------------------------------------------------------------------------

% jpl_type_to_class(+Type, -ClassObject) :-
%   incomplete types are now never cached (or otherwise passed around),
%   but we check anyway (?)
%   jFindClass throws an exception if FCN can't be found

jpl_type_to_class(T, @(Tag)) :-
    ground(T),
    (	jpl_class_tag_type_cache(ClassTag,T)
    ->	Tag = ClassTag
    ;	(   jpl_type_to_findclassname(T, FCN)	% peculiar syntax for FindClass()
	->  jFindClass(FCN, @(ClassTag)),	% which caches type of @ClassTag
	    jpl_cache_type_of_ref(T, @(ClassTag))
	),
	assert(jpl_class_tag_type_cache(ClassTag,T))
    ),
    Tag = ClassTag.

%------------------------------------------------------------------------------

%eg jpl_type_to_classname(class([java,util],['Date']), 'java.util.Date')

% jpl_type_to_classname(+Type, -ClassName) :-
%   Type, which is a class or array type (not sure about the others...),
%   is denoted by ClassName in dotted syntax

jpl_type_to_classname(T, CN) :-
    (	phrase(jpl_type_classname_1(T), Cs)
    ->	atom_codes(CNx, Cs),				    % green commit to first solution
	CN = CNx
    ).

%------------------------------------------------------------------------------

% jpl_type_to_descriptor(+Type, -Descriptor) :-
%   Type (denoting any Java type, and can also be a JPL method/2 structure)
%   is represented by Descriptor (JVM internal syntax)
%   I'd cache this, but I'd prefer more efficient indexing on types (hashed?)

jpl_type_to_descriptor(T, D) :-
    (	phrase(jpl_type_descriptor_1(T), Cs)
    ->	atom_codes(Dx, Cs),
	D = Dx
    ).

%------------------------------------------------------------------------------

% jpl_type_to_findclassname(+Type, -FindClassName) :-
%   FindClassName denotes Type (class or array only)
%   in the syntax required peculiarly by FindClass()

jpl_type_to_findclassname(T, FCN) :-
    (	phrase(jpl_type_findclassname(T), Cs)
    ->	atom_codes(FCNx, Cs),
	FCN = FCNx
    ).

%------------------------------------------------------------------------------

jpl_type_to_super_type(T, Tx) :-
    (	jpl_object_type_to_super_type(T, Tx)
    ->	true
    ;	jpl_primitive_type_to_super_type(T, Tx)
    ->	true
    ).

%------------------------------------------------------------------------------

% jpl_types_fit_type(+Types, +Type) :-
%   each member of Types is (independently) (if that means anything)
%   assignable to Type
%   e.g. for dynamic type check when attempting to assign list of values to array

jpl_types_fit_type([], _).

jpl_types_fit_type([T1|T1s], T2) :-
    jpl_type_fits_type(T1, T2),
    jpl_types_fit_type(T1s, T2).

%------------------------------------------------------------------------------

jpl_types_fit_types([], []).

jpl_types_fit_types([T1|T1s], [T2|T2s]) :-
    jpl_type_fits_type(T1, T2),
    jpl_types_fit_types(T1s, T2s).

%------------------------------------------------------------------------------

% jpl_value_to_type(+Value, -Type) :-
%   Value must be a proper JPL datum other than a ref
%   i.e. primitive, String or void;
%   it is of (unique most specific) Type

jpl_value_to_type(V, T) :-
    ground(V),				% critically assumed by jpl_value_to_type_1/2
    (	jpl_value_to_type_1(V, Tv)	% 2nd arg must be unbound
    ->	T = Tv
    ).

%------------------------------------------------------------------------------

% jpl_value_to_type_1(+Value, -Type) :-
%   Type is the unique most specific JPL type of which Value represents an instance;
%   called solely by jpl_value_to_type/2, which commits to first solution;
%
%   NB integer values are of JPL-peculiar uniquely most specific subtypes,
%   i.e. char_byte, char_short, char_int, pos_int, neg_byte, neg_short, neg_int
%   but all are understood by the JPL-internal utilities which call this proc
%
%   NB could replace
%	{pos_int,neg_int} -> proper_int (or int?)
%	neg_byte -> proper_byte (or byte?)
%	neg_short -> proper_short (or short?)
%
%   NB we regard float as subtype of double
%
%   NB objects and refs always have straightforward types

jpl_value_to_type_1(@(false), boolean).

jpl_value_to_type_1(@(true), boolean).

jpl_value_to_type_1(jlong(_,_), long).	    % discrimination, not validation

jpl_value_to_type_1(A, class([java,lang],['String'])) :-   % yes it's a "value"
    atom(A).

jpl_value_to_type_1(I, T) :-
    integer(I),
    (	I >= 0	->  (	I < 128	    ->	T = char_byte
		    ;	I < 32768   ->	T = char_short
		    ;	I < 65536   ->	T = char_int
				    ;	T = int		% was pos_int
		    )
    ;	I >= -128   ->	T = byte			% was neg_byte
    ;	I >= -32768 ->	T = short			% was neg_short
		    ;	T = int				% was neg_int
    ).

jpl_value_to_type_1(F, float) :-
    float(F).

%------------------------------------------------------------------------------

% jpl_void(-X) :-
%   X is (by unification) the proper JPL datum which represents the pseudo Java value 'void';
%   c.f. jpl_is_void/1

jpl_void(@(void)).

%------------------------------------------------------------------------------

%type	jpl_array_to_length(array, integer)

% jpl_array_to_length(+ArrayObject, -Length) :-
%   must validate ArrayObject before making the JNI call...

jpl_array_to_length(A, N) :-
    (	jpl_ref_to_type(A, array(_))	% can this be done cheaper e.g. in foreign code?
    ->	jGetArrayLength(A, N)		% *must* be array, else undefined (crash?)
    ).

%------------------------------------------------------------------------------

%type	jpl_array_to_list(array, list(datum))

% jpl_array_to_list(+Array, -Elements) :-

jpl_array_to_list(A, Es) :-
    jpl_array_to_length(A, Len),
    (	Len > 0
    ->	LoBound is 0,
	HiBound is Len-1,
	jpl_get(A, LoBound-HiBound, Es)
    ;	Es = []
    ).

%------------------------------------------------------------------------------

%type	jpl_datums_to_array(list(datum), array)

% jpl_datums_to_array(+Ds, -A) :-
%   A will be a ref to a new JVM array,
%   whose base type is the most specific Java type
%   of which each member of Datums is (directly or indirectly) an instance;
%   NB this fails (without warning, currently) if:
%	Ds is an empty list (no base type can be inferred)
%	Ds contains a primitive value and an object or array ref (no common supertype)

jpl_datums_to_array(Ds, A) :-
    ground(Ds),
    jpl_datums_to_most_specific_common_ancestor_type(Ds, T),
    jpl_new(array(T), Ds, A).

%------------------------------------------------------------------------------

%type	jpl_enumeration_element(object, datum)

% jpl_enumeration_element(+Enumeration, -Element) :-
%   generates each Element from the Enumeration

jpl_enumeration_element(En, E) :-
    (	jpl_call(En, hasMoreElements, [], @(true))
    ->	jpl_call(En, nextElement, [], Ex),
	(   E = Ex
	;   jpl_enumeration_element(En, E)
	)
    ).

%------------------------------------------------------------------------------

%type	jpl_enumeration_to_list(object, list(datum))

% jpl_enumeration_to_list(+Enumeration, -Elements) :-

jpl_enumeration_to_list(EN, Es) :-
    (	jpl_call(EN, hasMoreElements, [], @(true))
    ->	jpl_call(EN, nextElement, [], E),
	Es = [E|Es1],
	jpl_enumeration_to_list(EN, Es1)
    ;	Es = []
    ).

%------------------------------------------------------------------------------

%type	jpl_hashtable_pair(object, pair(datum,datum))

% jpl_hashtable_pair(+HashTable, -KeyValuePair) :-
%   generates Key-Value pairs from the given HashTable
%   NB String is converted to atom but Integer is presumably returned as an object ref

jpl_hashtable_pair(HT, K-V) :-
    jpl_call(HT, keys, [], Ek),
    jpl_enumeration_to_list(Ek, Ks),
    member(K, Ks),
    jpl_call(HT, get, [K], V).

%------------------------------------------------------------------------------

%type	jpl_iterator_element(object, datum)

% jpl_iterator_element(+Iterator, -Element) :-

jpl_iterator_element(I, E) :-
    (	jpl_call(I, hasNext, [], @(true))
    ->	(   jpl_call(I, next, [], E)	    % surely it's steadfast...
	;   jpl_iterator_element(I, E)
	)
    ).

%------------------------------------------------------------------------------

%type	jpl_list_to_array(list(datum), array)

% jpl_list_to_array(+Datums, -Array) :-
%   Datums is a proper list of JPL datums (values or refs);
%   if they have a most specific common supertype,
%   Array is an array, of that base type,
%   whose respective elements are Datums

jpl_list_to_array(Ds, A) :-
    jpl_datums_to_array(Ds, A).

%------------------------------------------------------------------------------

%type	jpl_map_element(object, pair(datum,datum))

% jpl_map_element(+Map, -KeyValue) :-
%   Map must be an instance of any implementation of the java.util.Map interface;
%   this generates each Key-Value pair from the Map

jpl_map_element(M, K-V) :-
    jpl_call(M, entrySet, [], ES),
    jpl_set_element(ES, E),
    jpl_call(E, getKey, [], K),
    jpl_call(E, getValue, [], V).

%------------------------------------------------------------------------------

%type	jpl_set_element(object, datum)

% jpl_set_element(+Set, -Element) :-

jpl_set_element(S, E) :-
    jpl_call(S, iterator, [], I),
    jpl_iterator_element(I, E).


%------------------------------------------------------------------------------

% is_pair(?T) :-
%   I define a half-decent "pair" as having a ground key (any val)

is_pair(0) :- !, fail.
is_pair(Key-_Val) :-
    ground(Key).

%------------------------------------------------------------------------------

is_pairs(0) :- !, fail.
is_pairs([]).
is_pairs([H|T]) :-
    is_pair(H),
    is_pairs(T).

%------------------------------------------------------------------------------

qp_atom_to_chars(A, Cs) :-
	atom_codes(A, Cs).

%------------------------------------------------------------------------------

qp_chars_to_atom(Cs, A) :-
	atom_codes(A, Cs).

%------------------------------------------------------------------------------

multimap_to_atom(KVs, A) :-
	multimap_to_atom_1(KVs, "", Cz, []),
	flatten(Cz, Cs),
	qp_chars_to_atom(Cs, A).

%------------------------------------------------------------------------------

multimap_to_atom_1([], _, Cs, Cs).

multimap_to_atom_1([K-V|KVs], T, Cs1, Cs0) :-
    qp_atom_to_chars(K, CsK),
    Cs1 = [T,CsK," = "|Cs2],
    (	is_list(V)
    ->	(   is_pairs(V)
	->  V = V2
	;   findall(
		N-Ve,
		nth1(N, V, Ve),
		V2
	    )
	),
	T2 = ["	   ",T],
	Cs2 = [10|Cs2a],
	multimap_to_atom_1(V2, T2, Cs2a, Cs3)
    ;	term_to_chars(V, CsV),
	Cs2 = [CsV,10|Cs3]
    ),
    multimap_to_atom_1(KVs, T, Cs3, Cs0).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:error_message/3.

prolog:error_message(java_exception(Ex)) -->
	(   { jpl_call(Ex, toString, [], Msg)
	    }
	->  [ 'Java exception: ~w'-[Msg] ]
	;   [ 'Java exception: ~w'-[Ex] ]
	).


		 /*******************************
		 *	       PATHS		*
		 *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(jar, swi(lib)).


		 /*******************************
		 *         LOAD THE JVM		*
		 *******************************/

%	check_java_environment
%	
%	Verify the Java environment.  Preferably   we  would create, but
%	most Unix systems do not   allow putenv("LD_LIBRARY_PATH=..." in
%	the current process. A suggesting found on  the net is to modify
%	LD_LIBRARY_PATH right at startup and  next execv() yourself, but
%	this doesn't work if we want to load Java on demand or if Prolog
%	itself is embedded in another application.
%	
%	So, after reading lots of pages on   the web, I decided checking
%	the environment and producing a sensible   error  message is the
%	best we can do.
%	
%	Please not that Java2 doesn't require   $CLASSPATH to be set, so
%	we do not check for that.

check_java_environment :-
	check_lib(java),
	check_lib(jvm).

check_lib(Name) :-
	check_shared_object(Name, File, EnvVar, Absolute),
	(   Absolute == (-)
	->  (   current_prolog_flag(windows, true)
	    ->	A = '%', Z = '%'
	    ;	A = '$', Z = ''
	    ),
	    sformat(Msg, 'Please add directory holding ~w to ~w~w~w',
		    [ File, A, EnvVar, Z ]),
	    throw(error(existence_error(library, Name),
			context(_, Msg)))
	;   true
	).

check_shared_object(Name, File, EnvVar, Absolute) :-
	libfile(Name, File),
	library_search_path(Path, EnvVar),
	(   member(Dir, Path),
	    concat_atom([Dir, File], /, Absolute),
	    exists_file(Absolute)
	->  true
	;   Absolute = (-)
	).

libfile(Base, File) :-
	current_prolog_flag(unix, true), !,
	atom_concat(lib, Base, F0),
	current_prolog_flag(shared_object_extension, Ext),
	file_name_extension(F0, Ext, File).
libfile(Base, File) :-
	current_prolog_flag(windows, true), !,
	current_prolog_flag(shared_object_extension, Ext),
	file_name_extension(Base, Ext, File).

library_search_path(Path, 'LD_LIBRARY_PATH') :-
	current_prolog_flag(unix, true), !,
	(   getenv('LD_LIBRARY_PATH', Env),
	    concat_atom(Path, ':', Env)
	->  true
	;   Path = []
	).
library_search_path(Path, 'PATH') :-
	current_prolog_flag(windows, true), !,
	(   getenv('PATH', Env),
	    concat_atom(Path, ';', Env)
	->  true
	;   Path = []
	).

%	add_jpl_to_classpath/0
%	
%	Add jpl.jar to $CLASSPATH to facilitate callbacks

add_jpl_to_classpath :-
	absolute_file_name(jar('jpl.jar'),
			   [ access(read)
			   ], JplJAR), !,
	(   getenv('CLASSPATH', Old)
	->  true
	;   Old = '.'
	),
	(   current_prolog_flag(windows, true)
	->  Sep = (';')
	;   Sep = (:)
	),
	concat_atom([JplJAR, Old], Sep, New),
	setenv('CLASSPATH', New).

%	libjpl(-Spec)
%	
%	Return the spec for loading the   JPL shared object. This shared
%	object must be called libjpl.so as the Java System.loadLibrary()
%	call used by jpl.jar adds the lib* prefix.

libjpl(File) :-
	(   current_prolog_flag(unix, true)
	->  File = foreign(libjpl)
	;   File = foreign(jpl)
	).

%	add_java_to_ldpath/0
%	
%	Adds the directories holding jvm.dll and java.dll to the %PATH%.
%	This appears to work on Windows. Unfortunately most Unix systems
%	appear to inspect the content of LD_LIBRARY_PATH only once.

add_java_to_ldpath :-
	current_prolog_flag(windows, true), !,
	add_java_dir(jvm, '\\jre\\bin\\client', Extra1),
	add_java_dir(java, '\\jre\\bin', Extra2),
	append(Extra1, Extra2, Extra),
	(   Extra \== []
	->  print_message(informational,
			  format('Added ~w to %PATH%', [Extra])),
	    getenv('PATH', Path0),
	    concat_atom([Path0|Extra], ';', Path),
	    setenv('PATH', Path)
	;   true
	).
add_java_to_ldpath.

add_java_dir(DLL, SubPath, Dir) :-
	(   check_shared_object(DLL, _, _Var, Abs),
	    Abs \== (-)
	->  Dir = []
	;   java_home(JavaHome)
	->  atom_concat(JavaHome, SubPath, ClientDir),
	    Dir = [ClientDir]
	;   Dir = []
	).
	    
java_home(Home) :-
	getenv('JAVA_HOME', Home),
	exists_directory(Home), !.
java_home(Home) :-
	current_prolog_flag(windows, true), !,
	Key0 = 'HKEY_LOCAL_MACHINE/Software/JavaSoft/Java Development Kit',
	win_registry_get_value(Key0, 'CurrentVersion', Version),
	concat_atom([Key0, Version], /, Key),
	win_registry_get_value(Key, 'JavaHome', Home),
	exists_directory(Home), !.
java_home(Home) :-
	current_prolog_flag(unix, true),
	member(Home, [ '/usr/lib/java',
		       '/usr/local/lib/java'
		     ]),
	exists_directory(Home), !.

:- dynamic
	jvm_ready/0.
:- volatile
	jvm_ready/0.

setup_jvm :-
	jvm_ready, !.
setup_jvm :-
	add_jpl_to_classpath,
	add_java_to_ldpath,
	libjpl(JPL),
	catch(load_foreign_library(JPL), E, report_java_setup_problem(E)),
	assert(jvm_ready).

report_java_setup_problem(E) :-
	print_message(error, E),
	check_java_environment.

:- initialization
   setup_jvm.
