/*  $Id$

	Part of JPL -- SWI-Prolog/Java interface

	Author:        Paul Singleton, Fred Dushin and Jan Wielemaker
	E-mail:        paul@jbgb.com
	WWW:           http://www.swi-prolog.org
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
	  [ jpl_get_default_jvm_opts/1,
	    jpl_set_default_jvm_opts/1,
	    jpl_get_actual_jvm_opts/1,
	    jpl_pl_lib_version/1,
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
	    jpl_terms_to_array/2,
	    jpl_map_element/2,
	    jpl_set_element/2
	  ]).
:- use_module(library(lists)).

% suppress debugging this library
:- set_prolog_flag(generate_debug_info, false).

%------------------------------------------------------------------------------

jpl_get_default_jvm_opts( Opts) :-
	jni_get_default_jvm_opts( Opts).

%------------------------------------------------------------------------------

jpl_set_default_jvm_opts( Opts) :-
	is_list( Opts),
	length( Opts, N),
	jni_set_default_jvm_opts( N, Opts).

%------------------------------------------------------------------------------

jpl_get_actual_jvm_opts( Opts) :-
	jni_get_actual_jvm_opts( Opts).

%------------------------------------------------------------------------------

jpl_supported_jni_version( 1, Minor) :-
	(   Minor = 4,
	jni_supported_jvm_version( 1, Minor)
	;   Minor = 2,
	jni_supported_jvm_version( 1, Minor)
	;   Minor = 1,
	jni_supported_jvm_version( 1, Minor)
	).

%------------------------------------------------------------------------------

jpl_assert( Fact) :-
	(   jpl_assert_policy( Fact, yes)
	->  assert( Fact)
	;   true
	).

%------------------------------------------------------------------------------

jpl_assert_policy( jpl_field_spec_cache(_,_,_,_,_,_), yes).
jpl_assert_policy( jpl_method_spec_cache(_,_,_,_,_,_,_,_), yes).
jpl_assert_policy( jpl_class_tag_type_cache(_,_), yes).
jpl_assert_policy( jpl_classname_type_cache(_,_), yes).
jpl_assert_policy( jpl_iref_type_cache(_,_), no).   % must correspond to JPL_CACHE_TYPE_OF_REF in jpl.c

jpl_assert_policy( jpl_field_spec_is_cached(_), YN) :-
	jpl_assert_policy( jpl_field_spec_cache(_,_,_,_,_,_), YN).
jpl_assert_policy( jpl_method_spec_is_cached(_), YN) :-
	jpl_assert_policy( jpl_method_spec_cache(_,_,_,_,_,_,_,_), YN).

%------------------------------------------------------------------------------

% jpl_tidy_iref_type_cache( +Iref) :-
%   delete the cached type info, if any, under Iref;
%   called from jpl.c's jni_free_iref() via jni_tidy_iref_type_cache() 

jpl_tidy_iref_type_cache( Iref) :-
  % write( '[decaching types for iref='), write( Iref), write( ']'), nl,
	retractall( jpl_iref_type_cache(Iref,_)),
	true.

%------------------------------------------------------------------------------

% jpl_call(+X, +MethodSpec, +Params, -Result) :-
%   X should be:
%     an object reference
%       (for static or instance methods)
%     a classname, descriptor or type
%       (for static methods of the denoted class)
%
%   MethodSpec should be:
%     a method name (as an atom)
%       (may involve dynamic overload resolution based on inferred types of params)
%
%   Params should be:
%     a proper list (perhaps empty) of suitable actual parameters for the named method
%
%   finally, an attempt will be made to unify Result with the returned result

jpl_call(X, Mspec, Params, R) :-
	(   jpl_object_to_type(X, Type)         % the usual case (goal fails safely if X is var or rubbish)
	->  Obj = X,
	    Kind = instance
	;   var(X)
	->  throw(error(instantiation_error,
			context(jpl_call/4,
				'1st arg must be bound to an object, classname, descriptor or type')))
	;   atom(X)
	->  (   jpl_classname_to_type( X, Type)     % does this attempt to load the class?
	->  (   jpl_type_to_class( Type, ClassObj)
	    ->  Kind = static
	    ;   throw(error(existence_error(class,X),
			context(jpl_call/4,
				'the named class cannot be found')))
	    )
	;   throw(error(type_error(class_name_or_descriptor,X),
		    context(jpl_call/4, '1st arg must be an object, classname, descriptor or type')))
	)
	;   X = class(_,_)
	->  Type = X,
	    jpl_type_to_class( Type, ClassObj),
	    Kind = static
	;   X = array(_)
	->  throw(error(type_error(object_or_class,X),
		    context(jpl_call/4, 'cannot call a static method of an array type, as none exists')))
	;   throw(error(domain_error(object_or_class,X),
		    context(jpl_call/4,
			    '1st arg must be an object, classname, descriptor or type')))
	),
	(   atom(Mspec)                 % the usual case, i.e. a method name
	->  true
	;   var(Mspec)
	->  throw(error(instantiation_error,
		    context(jpl_call/4, '2nd arg must be an atom naming a public method of the class or object')))
	;   throw(error(type_error(method_name,Mspec),
		    context(jpl_call/4, '2nd arg must be an atom naming a public method of the class or object')))
	),
	(   is_list(Params)
	->  (   catch(
		jpl_datums_to_types(Params, Taps),
		error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
		throw(error(type_error(acyclic,Te),context(jpl_call/4,Msg)))
	    )   
	->  true
	;   throw(error(type_error(method_params,Params),
		    context(jpl_call/4, 'not all actual parameters are convertible to Java values or references')))
	),
	length( Params, A)
	;   var(Params)
	->  throw(error(instantiation_error,
		    context(jpl_call/4, '3rd arg must be a proper list of actual parameters for the named method')))
	;   throw(error(type_error(method_params,Params),
		    context(jpl_call/4, '3rd arg must be a proper list of actual parameters for the named method')))
	),
	(   Kind == instance
	->  jpl_call_instance(Type, Obj, Mspec, Params, Taps, A, Rx)
	;   jpl_call_static(Type, ClassObj, Mspec, Params, Taps, A, Rx)
	),
	(   nonvar(R),
	    R = {Term}  % yucky way of requesting Term->term conversion
	->  (   jni_jref_to_term( Rx, TermX)    % fails if Rx isn't a JRef to a jpl.Term
	->  Term = TermX
	;   throw(error(type_error,
			context(jpl_call/4, 'result is not a jpl.Term instance as required')))
	)
	;   R = Rx
	).

%------------------------------------------------------------------------------

%%	jpl_call_instance(+ObjectType, +Object, +MethodName, Params, 
%%			  ActualParamTypes, Arity, -Result)
%
%	call the MethodName-d method  (instance   or  static)  of Object
%	(which is of ObjectType),  which   most  specifically applies to
%	Params,  which  we  have   found    to   be   (respectively)  of
%	ActualParamTypes, and of which there are Arity, yielding Result

jpl_call_instance(Type, Obj, Mname, Params, Taps, A, Rx) :-
	findall(                    % get remaining details of all accessible methods of Obj's class (as denoted by Type)
	z5(I,Mods,MID,Tr,Tfps),
	jpl_method_spec(Type, I, Mname, A, Mods, MID, Tr, Tfps),
	Z5s
	),
	(   Z5s = []
	->  throw(error(existence_error(method,Mname/A),
		    context(jpl_call/4,
			    'the class or object has no public methods with the given name and quantity of parameters')))
	;   findall(
	    z5(I,Mods,MID,Tr,Tfps),             % those to which Params is assignable
	    (   member(z5(I,Mods,MID,Tr,Tfps), Z5s),
		jpl_types_fit_types(Taps, Tfps) % assignability test: actual param types "fit" formal param types
	    ),
	    Z5sA                                % Params-assignable methods
	),
	(   Z5sA == []
	->  throw(error(type_error(method_params,Params),
			context(jpl_call/4,
				'the actual parameters are not assignable to the formal parameters of any of the named methods')))

	;   Z5sA = [z5(I,Mods,MID,Tr,Tfps)]
	->  true                                % exactly one applicable method
	;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tfps))
	->  true                                % exactly one most-specific applicable method
	;   throw(error(existence_error(most_specific_method,Mname/Params),
			context(jpl_call/4,
				'more than one most-specific method is found for the actual parameters (this should not happen)')))
	)
	),
	(   member(static, Mods)                                        % if the chosen method is static
	->  jpl_object_to_class(Obj, ClassObj),                         % get a java.lang.Class instance which personifies Obj's class
	jpl_call_static_method(Tr, ClassObj, MID, Tfps, Params, Rx) % call static method w.r.t. associated Class object
	;   jpl_call_instance_method(Tr, Obj, MID, Tfps, Params, Rx)    % else call (non-static) method w.r.t. object itself
	).

%------------------------------------------------------------------------------

%%	jpl_call_static(+ClassType, +ClassObject, +MethodName, Params,
%%			ActualParamTypes, Arity, -Result)
%
%	call the MethodName-d static method of   the  class (which is of
%	ClassType, and which  is  represented   by  the  java.lang.Class
%	instance ClassObject) which most specifically applies to Params,
%	which we have found to   be  (respectively) of ActualParamTypes,
%	and of which there are Arity, yielding Result

jpl_call_static(Type, ClassObj, Mname, Params, Taps, A, Rx) :-
	findall(                    % get all accessible static methods of the class denoted by Type and ClassObj
	z5(I,Mods,MID,Tr,Tfps),
	(   jpl_method_spec(Type, I, Mname, A, Mods, MID, Tr, Tfps),
	    member(static, Mods)
	),
	Z5s
	),
	(   Z5s = []
	->  throw(error(existence_error(method,Mname/A),
		    context(jpl_call/4,
			    'the class has no public static methods with the given name and quantity of parameters')))
	;   findall(
	    z5(I,Mods,MID,Tr,Tfps),
	    (   member(z5(I,Mods,MID,Tr,Tfps), Z5s),
		jpl_types_fit_types(Taps, Tfps) % assignability test: actual param types "fit" formal param types
	    ),
	    Z5sA                                % Params-assignable methods
	),
	(   Z5sA == []
	->  throw(error(type_error(method_params,Params),
			context(jpl_call/4,
				'the actual parameters are not assignable to the formal parameters of any of the named methods')))
	;   Z5sA = [z5(I,Mods,MID,Tr,Tfps)]
	->  true                % exactly one applicable method
	;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tfps))
	->  true                % exactly one most-specific applicable method
	;   throw(error(existence_error(most_specific_method,Mname/Params),
			context(jpl_call/4,
				'more than one most-specific method is found for the actual parameters (this should not happen)')))
	)
	),
	jpl_call_static_method(Tr, ClassObj, MID, Tfps, Params, Rx).

%------------------------------------------------------------------------------

% jpl_call_instance_method(+Type, +ClassObject, +MethodID, +FormalParamTypes, +Params, -Result) :-

jpl_call_instance_method(void, Class, MID, Tfps, Ps, R) :-
	jCallVoidMethod(Class, MID, Tfps, Ps),
	jpl_void(R).

jpl_call_instance_method(boolean, Class, MID, Tfps, Ps, R) :-
	jCallBooleanMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(byte, Class, MID, Tfps, Ps, R) :-
	jCallByteMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(char, Class, MID, Tfps, Ps, R) :-
	jCallCharMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(short, Class, MID, Tfps, Ps, R) :-
	jCallShortMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(int, Class, MID, Tfps, Ps, R) :-
	jCallIntMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(long, Class, MID, Tfps, Ps, R) :-
	jCallLongMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(float, Class, MID, Tfps, Ps, R) :-
	jCallFloatMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(double, Class, MID, Tfps, Ps, R) :-
	jCallDoubleMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(array(_), Class, MID, Tfps, Ps, R) :-
	jCallObjectMethod(Class, MID, Tfps, Ps, R).

jpl_call_instance_method(class(_,_), Class, MID, Tfps, Ps, R) :-
	jCallObjectMethod(Class, MID, Tfps, Ps, R).

%------------------------------------------------------------------------------

% jpl_call_static_method(+Type, +ClassObject, +MethodID, +FormalParamTypes, +Params, -Result) :-

jpl_call_static_method(void, Class, MID, Tfps, Ps, R) :-
	jCallStaticVoidMethod(Class, MID, Tfps, Ps),
	jpl_void(R).

jpl_call_static_method(boolean, Class, MID, Tfps, Ps, R) :-
	jCallStaticBooleanMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(byte, Class, MID, Tfps, Ps, R) :-
	jCallStaticByteMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(char, Class, MID, Tfps, Ps, R) :-
	jCallStaticCharMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(short, Class, MID, Tfps, Ps, R) :-
	jCallStaticShortMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(int, Class, MID, Tfps, Ps, R) :-
	jCallStaticIntMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(long, Class, MID, Tfps, Ps, R) :-
	jCallStaticLongMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(float, Class, MID, Tfps, Ps, R) :-
	jCallStaticFloatMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(double, Class, MID, Tfps, Ps, R) :-
	jCallStaticDoubleMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(array(_), Class, MID, Tfps, Ps, R) :-
	jCallStaticObjectMethod(Class, MID, Tfps, Ps, R).

jpl_call_static_method(class(_,_), Class, MID, Tfps, Ps, R) :-
	jCallStaticObjectMethod(Class, MID, Tfps, Ps, R).

%------------------------------------------------------------------------------

%type   jpl_fergus_find_candidate(list(T), T, T, list(T))

jpl_fergus_find_candidate([], Candidate, Candidate, []).

jpl_fergus_find_candidate([X|Xs], Candidate0, Candidate, Rest) :-
	(   jpl_fergus_greater(X, Candidate0)
	->  Candidate1 = X,
	Rest = [Candidate0|Rest1]
	;   Candidate1 = Candidate0,
	Rest = [X|Rest1]
	),
	jpl_fergus_find_candidate(Xs, Candidate1, Candidate, Rest1).

%------------------------------------------------------------------------------

jpl_fergus_greater(z5(_,_,_,_,Tps1), z5(_,_,_,_,Tps2)) :-
	jpl_types_fit_types(Tps1, Tps2).
jpl_fergus_greater(z3(_,_,Tps1), z3(_,_,Tps2)) :-
	jpl_types_fit_types(Tps1, Tps2).

%------------------------------------------------------------------------------

%type   jpl_fergus_is_the_greatest(list(T), T)

%%	jpl_fergus_is_the_greatest(Xs, GreatestX)
%
%	Xs is a list of things  for which jpl_fergus_greater/2 defines a
%	partial ordering; GreatestX is one of  those, than which none is
%	greater; fails if there is more   than  one such; this algorithm
%	was contributed to c.l.p by Fergus   Henderson in response to my
%	"there must be a better way" challenge: there was, this is it

jpl_fergus_is_the_greatest([X|Xs], Greatest) :-
	jpl_fergus_find_candidate(Xs, X, Greatest, Rest),
	forall(
	member(R, Rest),
	jpl_fergus_greater(Greatest, R)
	).

%------------------------------------------------------------------------------

%%	jpl_get(+X, +Fspec, -V)
%	
%   X can be:
%     * a classname, a descriptor, or an (object or array) type
%       (for static fields);
%     * a non-array object
%       (for static and non-static fields)
%     * an array
%       (for 'length' pseudo field, or indexed element retrieval),
%   but not:
%     * a String
%       (clashes with class name; anyway, String has no fields to retrieve)
%
%   Fspec can be:
%       * an atomic field name,
%       * or an integral array index (to get an element from an array,
%	* or a pair I-J of integers (to get a subrange (slice?) of an
%	  array)
%
%   finally, an attempt will be made to unify V with the retrieved value

jpl_get(X, Fspec, V) :-
	(   jpl_object_to_type(X, Type)
	->  Obj = X,
	    jpl_get_instance( Type, Type, Obj, Fspec, Vx)   % pass Type twice for FAI
	;   var(X)
	->  throw(error(instantiation_error,
		    context(jpl_get/3,
			    '1st arg must be bound to an object, classname, descriptor or type')))
	;   jpl_is_type(X)          % e.g. class([java,lang],['String']), array(int)
	->  Type = X,
	    (   jpl_type_to_class(Type, ClassObj)
	    ->  jpl_get_static( Type, ClassObj, Fspec, Vx)
	    ;   jpl_type_to_classname( Type, Classname),
		throw(error(existence_error(class,Classname),
			    context(jpl_get/3,
				    'the named class cannot be found')))
	    )
	;   atom(X)
	->  (   jpl_classname_to_type( X, Type)     % does this attempt to load the class?
	    ->  (   jpl_type_to_class( Type, ClassObj)
		->  jpl_get_static( Type, ClassObj, Fspec, Vx)
		;   throw(error(existence_error(class,X),
				context(jpl_get/3,
					'the named class cannot be found')))
		)
	    ;   throw(error(type_error(class_name_or_descriptor,X),
			    context(jpl_get/3, '1st arg must be an object, classname, descriptor or type')))
	    )

	;   throw(error(domain_error(object_or_class,X),
		    context(jpl_get/3,
			    '1st arg must be bound to an object, classname, descriptor or type')))
	),
	(   nonvar(V),
	    V = {Term}  % yucky way of requesting Term->term conversion
	->  (   jni_jref_to_term( Vx, TermX)    % fails if Rx is not a JRef to a jpl.Term
	    ->  Term = TermX
	    ;   throw(error(type_error,
			    context(jpl_call/4, 'result is not a jpl.Term instance as required')))
	    )
	;   V = Vx
	).

%------------------------------------------------------------------------------

%%	jpl_get_static(+Type, +ClassObject, +FieldName, -Value)
%
%	ClassObject is an instance of   java.lang.Class which represents
%	the same class as Type; Value   (Vx below) is guaranteed unbound
%	on entry, and will, before exit,   be unified with the retrieved
%	value

jpl_get_static(Type, ClassObj, Fname, Vx) :-
	(   atom(Fname)             % assume it's a field name
	->  true
	;   var(Fname)
	->  throw(error(instantiation_error,
		    context(jpl_get/3, '2nd arg must be bound to an atom naming a public field of the class')))
	;   throw(error(type_error(field_name,Fname),
		    context(jpl_get/3, '2nd arg must be an atom naming a public field of the class')))
	),
  % get static fields of the denoted class
	findall(
	z4(I,Mods,FID,Tf),
	(   jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
	    member(static, Mods)
	),
	Z4s
	),
	(   Z4s = []
	->  throw(error(existence_error(field,Fname),
		    context(jpl_get/3,
			    'the class or object has no public static field with the given name')))
	;   Z4s = [z4(I,_Mods,FID,Tf)]
	->  jpl_get_static_field(Tf, ClassObj, FID, Vx)
	;   throw(error(existence_error(unique_field,Fname),
		    context(jpl_get/3,
			    'more than one field is found with the given name')))
	).

%------------------------------------------------------------------------------

% jpl_get_instance(+Type, +Type, +Object, +FieldSpecifier, -Value) :-

jpl_get_instance(class(_,_), Type, Obj, Fname, Vx) :-
	(   atom(Fname)                 % the usual case
	->  true
	;   var(Fname)
	->  throw(error(instantiation_error,
		    context(jpl_get/3, '2nd arg must be bound to an atom naming a public field of the class or object')))
	;   throw(error(type_error(field_name,Fname),
		    context(jpl_get/3, '2nd arg must be an atom naming a public field of the class or object')))
	),
	findall(z4(I,Mods,FID,Tf),
		jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
		Z4s),
	(   Z4s = []
	->  throw(error(existence_error(field,Fname),
			context(jpl_get/3,
				'the class or object has no public field with the given name')))
	;   Z4s = [z4(I,Mods,FID,Tf)]
	->  (   member(static, Mods)
	    ->  jpl_object_to_class(Obj, ClassObj),
		jpl_get_static_field(Tf, ClassObj, FID, Vx)
	    ;   jpl_get_instance_field(Tf, Obj, FID, Vx)
	    )   
	;   throw(error(existence_error(unique_field,Fname),
		    context(jpl_get/3,
			    'more than one field is found with the given name')))
	).

jpl_get_instance(array(ElementType), _, Array, Fspec, Vx) :-
	(   var(Fspec)
	->  throw(error(instantiation_error,
			context(jpl_get/3,
				'when 1st arg is an array, 2nd arg must be bound to an index, an index range, or ''length''')))
	;   integer(Fspec)
	->  (   Fspec < 0       % lo bound check
	    ->  throw(error(domain_error(array_index,Fspec),
			    context(jpl_get/3,
				    'when 1st arg is an array, integral 2nd arg must be non-negative')))
	    ;   jGetArrayLength(Array, Len),
		Fspec >= Len    % hi bound check
	    ->  throw(error(domain_error(array_index,Fspec),
			    context(jpl_get/3,
				    'when 1st arg is an array, integral 2nd arg must not exceed upper bound of array')))
	    ;   jpl_get_array_element(ElementType, Array, Fspec, Vx)
	    )
	;   Fspec = N-M     % NB should we support e.g. 3-2 -> [] ?
	->  (   integer(N),
	        integer(M)
	    ->  (   N >= 0,
		    M >= N
		->  jGetArrayLength(Array, Len),
		    (   N >= Len
		    ->  throw(error(domain_error(array_index_range,N-M),
				    context(jpl_get/3,
					    'lower bound of array index range must not exceed upper bound of array')))
		    ;   M >= Len
		    ->  throw(error(domain_error(array_index_range,N-M),
				    context(jpl_get/3,
					    'upper bound of array index range must not exceed upper bound of array')))
		    ;   jpl_get_array_elements(ElementType, Array, N, M, Vx)
		    )
		;   throw(error(domain_error(array_index_range,N-M),
				context(jpl_get/3,
					'array index range must be a non-decreasing pair of non-negative integers')))
		)
	    ;   throw(error(type_error(array_index_range,N-M),
			    context(jpl_get/3,
				    'array index range must be a non-decreasing pair of non-negative integers')))
	    )
	;   atom(Fspec)
	->  (   Fspec == length             % special-case for this solitary array "method"
	    ->  jGetArrayLength(Array, Vx)
	    ;   throw(error(domain_error(array_field_name,Fspec),
			    context(jpl_get/3,
				    'the array has no public field with the given name')))
	    )
	;   throw(error(type_error(array_lookup_spec,Fspec),
			context(jpl_get/3,
				'when 1st arg is an array, 2nd arg must be an index, an index range, or ''length''')))
	).

%------------------------------------------------------------------------------

%%	jpl_get_array_element(+ElementType, +Array, +Index, -Vc)
%
%	Array is (a  reference  to)  an   array  of  ElementType;  Vc is
%	(unified with a JPL repn  of)   its  Index-th  (numbered from 0)
%	element Java values are now  converted   to  Prolog terms within
%	foreign code
%	
%	@tbd	more of this could be done within foreign code ...

jpl_get_array_element(Type, Array, Index, Vc) :-
	(   (   Type = class(_,_)
	    ;   Type = array(_)
	    )
	->  jGetObjectArrayElement(Array, Index, Vr)
	;   jpl_primitive_type(Type)
	->  jni_type_to_xput_code(Type, Xc),
	    jni_alloc_buffer(Xc, 1, Bp),		% one-element buf for a Type
	    jpl_get_primitive_array_region(Type, Array, Index, 1, Bp),
	    jni_fetch_buffer_value(Bp, 0, Vr, Xc),	% zero-th element
	    jni_free_buffer(Bp)
	),
	Vr = Vc.    % redundant since Vc is always (?) unbound at call

%------------------------------------------------------------------------------

%%	jpl_get_array_elements(+ElementType, +Array, +N, +M, -Vs)
%
%	serves only jpl_get_instance Vs will always be unbound on entry

jpl_get_array_elements(ElementType, Array, N, M, Vs) :-
	(   (   ElementType = class(_,_)
	    ;   ElementType = array(_)
	    )
	->  jpl_get_object_array_elements(Array, N, M, Vs)
	;   jpl_get_primitive_array_elements(ElementType, Array, N, M, Vs)
	).

%------------------------------------------------------------------------------

jpl_get_instance_field(boolean, Obj, FieldID, V) :-
	jGetBooleanField(Obj, FieldID, V).
jpl_get_instance_field(byte, Obj, FieldID, V) :-
	jGetByteField(Obj, FieldID, V).
jpl_get_instance_field(char, Obj, FieldID, V) :-
	jGetCharField(Obj, FieldID, V).
jpl_get_instance_field(short, Obj, FieldID, V) :-
	jGetShortField(Obj, FieldID, V).
jpl_get_instance_field(int, Obj, FieldID, V) :-
	jGetIntField(Obj, FieldID, V).
jpl_get_instance_field(long, Obj, FieldID, V) :-
	jGetLongField(Obj, FieldID, V).
jpl_get_instance_field(float, Obj, FieldID, V) :-
	jGetFloatField(Obj, FieldID, V).
jpl_get_instance_field(double, Obj, FieldID, V) :-
	jGetDoubleField(Obj, FieldID, V).
jpl_get_instance_field(class(_,_), Obj, FieldID, V) :-
	jGetObjectField(Obj, FieldID, V).
jpl_get_instance_field(array(_), Obj, FieldID, V) :-
	jGetObjectField(Obj, FieldID, V).

%------------------------------------------------------------------------------

%%	jpl_get_object_array_elements(+Array, +LoIndex, +HiIndex, -Vcs)
%
%	Array should be a (zero-based) array   of  some object (array or
%	non-array)  type;  LoIndex  is  an  integer,   0  =<  LoIndex  <
%	length(Array); HiIndex is an  integer,   LoIndex-1  =< HiIndex <
%	length(Array); at call, Vcs will be   unbound; at exit, Vcs will
%	be  a  list   of   (references    to)   the   array's   elements
%	[LoIndex..HiIndex] inclusive

jpl_get_object_array_elements(Array, Lo, Hi, Vcs) :-
	(   Lo =< Hi
	->  Vcs = [Vc|Vcs2],
	    jGetObjectArrayElement(Array, Lo, Vc),
	    Next is Lo+1,
	    jpl_get_object_array_elements(Array, Next, Hi, Vcs2)
	;   Vcs = []
	).

%------------------------------------------------------------------------------

%%	jpl_get_primitive_array_elements(+ElementType, +Array, +LoIndex, +HiIndex, -Vcs)
%
%	Array  should  be  a  (zero-based)  Java  array  of  (primitive)
%	ElementType; Vcs should be unbound on entry, and on exit will be
%	a list of (JPL representations of   the  values of) the elements
%	[LoIndex..HiIndex] inclusive

jpl_get_primitive_array_elements(ElementType, Array, Lo, Hi, Vcs) :-
	Size is Hi-Lo+1,
	(   Size == 0
	->  Vcs = []
	;   jni_type_to_xput_code(ElementType, Xc),
	    jni_alloc_buffer(Xc, Size, Bp),
	    jpl_get_primitive_array_region(ElementType, Array, Lo, Size, Bp),
	    jpl_primitive_buffer_to_array(ElementType, Xc, Bp, 0, Size, Vcs),
	    jni_free_buffer(Bp)
	).

%------------------------------------------------------------------------------

jpl_get_primitive_array_region(boolean, Array, Lo, S, I) :-
	jGetBooleanArrayRegion(Array, Lo, S, jbuf(I,boolean)).
jpl_get_primitive_array_region(byte, Array, Lo, S, I) :-
	jGetByteArrayRegion(Array, Lo, S, jbuf(I,byte)).
jpl_get_primitive_array_region(char, Array, Lo, S, I) :-
	jGetCharArrayRegion(Array, Lo, S, jbuf(I,char)).
jpl_get_primitive_array_region(short, Array, Lo, S, I) :-
	jGetShortArrayRegion(Array, Lo, S, jbuf(I,short)).
jpl_get_primitive_array_region(int, Array, Lo, S, I) :-
	jGetIntArrayRegion(Array, Lo, S, jbuf(I,int)).
jpl_get_primitive_array_region(long, Array, Lo, S, I) :-
	jGetLongArrayRegion(Array, Lo, S, jbuf(I,long)).
jpl_get_primitive_array_region(float, Array, Lo, S, I) :-
	jGetFloatArrayRegion(Array, Lo, S, jbuf(I,float)).
jpl_get_primitive_array_region(double, Array, Lo, S, I) :-
	jGetDoubleArrayRegion(Array, Lo, S, jbuf(I,double)).

%------------------------------------------------------------------------------

jpl_get_static_field(boolean, Array, FieldID, V) :-
	jGetStaticBooleanField(Array, FieldID, V).
jpl_get_static_field(byte, Array, FieldID, V) :-
	jGetStaticByteField(Array, FieldID, V).
jpl_get_static_field(char, Array, FieldID, V) :-
	jGetStaticCharField(Array, FieldID, V).
jpl_get_static_field(short, Array, FieldID, V) :-
	jGetStaticShortField(Array, FieldID, V).
jpl_get_static_field(int, Array, FieldID, V) :-
	jGetStaticIntField(Array, FieldID, V).
jpl_get_static_field(long, Array, FieldID, V) :-
	jGetStaticLongField(Array, FieldID, V).
jpl_get_static_field(float, Array, FieldID, V) :-
	jGetStaticFloatField(Array, FieldID, V).
jpl_get_static_field(double, Array, FieldID, V) :-
	jGetStaticDoubleField(Array, FieldID, V).
jpl_get_static_field(class(_,_), Array, FieldID, V) :-
	jGetStaticObjectField(Array, FieldID, V).
jpl_get_static_field(array(_), Array, FieldID, V) :-
	jGetStaticObjectField(Array, FieldID, V).

%------------------------------------------------------------------------------

%%	jpl_new(+X, +Params, -V)
%
%   X can be:
%    * an atomic classname
%       e.g. 'java.lang.String'
%    * an atomic descriptor
%       e.g. '[I' or 'Ljava.lang.String;'
%    * a suitable type
%       i.e. any class(_,_) or array(_)
%
%   if X is an object (non-array)  type   or  descriptor and Params is a
%   list of values or references, then V  is the result of an invocation
%   of  that  type's  most  specifically-typed    constructor  to  whose
%   respective formal parameters the actual   Params are assignable (and
%   assigned)
%
%   if X is an array type or descriptor   and Params is a list of values
%   or references, each of which is   (independently)  assignable to the
%   array element type, then V is a  new   array  of as many elements as
%   Params has members,  initialised  with   the  respective  members of
%   Params;
%
%   if X is an array type  or   descriptor  and Params is a non-negative
%   integer N, then V is a new array of that type, with N elements, each
%   initialised to Java's appropriate default value for the type;
%
%   If V is {Term} then we attempt to convert a new jpl.Term instance to
%   a corresponding term; this is of  little   obvious  use here, but is
%   consistent with jpl_call/4 and jpl_get/3

jpl_new(X, Params, V) :-
	(   var(X)
	->  throw(error(instantiation_error,
		    context(jpl_new/3,
			    '1st arg must be bound to a classname, descriptor or object type')))
	;   jpl_is_type(X)                  % NB only class(_,_) or array(_)
	->  Type = X
	;   atom(X)                 % e.g. 'java.lang.String', '[L', 'boolean'
	->  (   jpl_classname_to_type(X, Type)
	    ->  true
	    ;   throw(error(domain_error(classname,X),
			    context(jpl_new/3,
				    'if 1st arg is an atom, it must be a classname or descriptor')))
	    )
	;   throw(error(type_error(instantiable,X),
			context(jpl_new/3,
				'1st arg must be a classname, descriptor or object type')))
	),
	jpl_new_1(Type, Params, Vx),
	(   nonvar(V),
	    V = {Term}  % yucky way of requesting Term->term conversion
	->  (   jni_jref_to_term( Vx, TermX)    % fails if Rx is not a JRef to a jpl.Term
	    ->  Term = TermX
	    ;   throw(error(type_error,
			    context(jpl_call/4, 'result is not a jpl.Term instance as required')))
	    )
	;   V = Vx
	).

%------------------------------------------------------------------------------

% jpl_new_1(+Tx, +Params, -Vx) :-
%   (serves only jpl_new/3)
%
%   Tx can be:
%     a class(_,_) or array(_) type;
%
%   Params must be:
%     a proper list of constructor parameters
%
%   at exit, Vx is bound to a JPL reference to a new, initialised instance of Tx

jpl_new_1(class(Ps,Cs), Params, Vx) :-
	!,                                      % green (see below)
	Tx = class(Ps,Cs),
	(   var(Params)
	->  throw(error(instantiation_error,
		    context(jpl_new/3,
			    '2nd arg must be a proper list of valid parameters for a constructor')))
	;   \+ is_list(Params)
	->  throw(error(type_error(list,Params),
		    context(jpl_new/3,
			    '2nd arg must be a proper list of valid parameters for a constructor')))
	;   true
	),
	length(Params, A),          % the "arity" of the required constructor
	jpl_type_to_class(Tx, Cx),  % throws Java exception if class is not found
	N = '<init>',               % JNI's constructor naming convention for GetMethodID()
	Tr = void,                  % all constructors have this return "type"
	findall(
	z3(I,MID,Tfps),
	jpl_method_spec(Tx, I, N, A, _Mods, MID, Tr, Tfps), % cached
	Z3s
	),
	(   Z3s == []               % no constructors which require the given qty of parameters?
	->  jpl_type_to_classname( Tx, Cn),
	(   jpl_call( Cx, isInterface, [], @(true))
	->  throw(error(type_error(concrete_class,Cn),
			context(jpl_new/3,
				'cannot create instance of an interface')))
	;   throw(error(existence_error(constructor,Cn/A),
			context(jpl_new/3,
				'no constructor found with the corresponding quantity of parameters')))
	)
	;   (   catch(
		jpl_datums_to_types(Params, Taps),  % infer actual parameter types
		error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
		throw(error(type_error(acyclic,Te),context(jpl_new/3,Msg)))
	    )
	->  true
	;   throw(error(domain_error(list(jpl_datum),Params),
			context(jpl_new/3,
				'one or more of the actual parameters is not a valid representation of any Java value or object')))
	),
	findall(
	    z3(I,MID,Tfps),                 % select constructors to which actual parameters are assignable
	    (   member(z3(I,MID,Tfps), Z3s),
		jpl_types_fit_types(Taps, Tfps) % assignability test: actual parameter types "fit" formal parameter types?
	    ),
	    Z3sA
	),
	(   Z3sA == []                      % no type-assignable constructors?
	->  (   Z3s = [_]
	    ->  throw(error(existence_error(constructor,Tx/A),
			context(jpl_new/3,
				'the actual parameters are not assignable to the formal parameter types of the only constructor which takes this qty of parameters')))
	    ;   throw(error(type_error(constructor_args,Params),
			context(jpl_new/3,
				'the actual parameters are not assignable to the formal parameter types of any of the constructors which take this qty of parameters')))
	    )
	;   Z3sA = [z3(I,MID,Tfps)]
	->  true
	;   jpl_z3s_to_most_specific_z3(Z3sA, z3(I,MID,Tfps))
	->  true
	;   throw(error(type_error(constructor_params,Params),
			context(jpl_new/3,
				'more than one most-specific matching constructor (shouldn''t happen)')))
	)
	),
	catch(
	jNewObject(Cx, MID, Tfps, Params, Vx),
	error(java_exception(@(_)), 'java.lang.InstantiationException'),
	(   jpl_type_to_classname( Tx, Cn),
	    throw(error(type_error(concrete_class,Cn),
			context(jpl_new/3,
				'cannot create instance of an abstract class')))
	)
	),
	jpl_cache_type_of_ref(Tx, Vx).          % since we know it

jpl_new_1(array(T), Params, Vx) :-
	!,
	(   var(Params)
	->  throw(error(instantiation_error,
		    context(jpl_new/3,
			    'when constructing a new array, 2nd arg must either be a non-negative integer (denoting the required array length) or a proper list of valid element values')))
	;   integer(Params)         % integer I -> array[0..I-1] of default values
	->  (   Params >= 0
	->  Len is Params
	;   throw(error(domain_error(array_length,Params),
		    context(jpl_new/3,
			    'when constructing a new array, if the 2nd arg is an integer (denoting the required array length) then it must be non-negative')))
	)
	;   is_list(Params)     % [V1,..VN] -> array[0..N-1] of respective values
	->  length(Params, Len)
	),
	jpl_new_array(T, Len, Vx), % NB may throw out-of-memory exception
	(   nth0(I, Params, Param),     % nmember fails silently when Params is integer
	jpl_set(Vx, I, Param),
	fail
	;   true
	),
	jpl_cache_type_of_ref(array(T), Vx).   % since we know it

jpl_new_1(T, _Params, _Vx) :-       % doomed attempt to create new primitive type instance (formerly a dubious completist feature :-)
	jpl_primitive_type(T),
	!,
	throw(error(domain_error(object_type,T),
	    context(jpl_new/3,
		    'cannot construct an instance of a primitive type'))).
  % (   var(Params)
  % ->  throw(error(instantiation_error,
  %                 context(jpl_new/3,
  %                         'when constructing a new instance of a primitive type, 2nd arg must be bound (to a representation of a suitable value)')))
  % ;   Params == []
  % ->  jpl_primitive_type_default_value(T, Vx)
  % ;   Params = [Param]
  % ->  jpl_primitive_type_term_to_value(T, Param, Vx)
  % ;   throw(error(domain_error(constructor_args,Params),
  %                 context(jpl_new/3,
  %                         'when constructing a new instance of a primitive type, 2nd arg must either be an empty list (indicating that the default value of that type is required) or a list containing exactly one representation of a suitable value)')))
  % ).

jpl_new_1( T, _, _) :-
	throw(error(domain_error(jpl_type,T),
		    context(jpl_new/3,
			    '1st arg must denote a known or plausible type'))).

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
	jNewObjectArray(Len, C, @(null), A).        % initialise each element to null

jpl_new_array(class(Ps,Cs), Len, A) :-
	jpl_type_to_class(class(Ps,Cs), C),
	jNewObjectArray(Len, C, @(null), A).

%------------------------------------------------------------------------------

% jpl_set(+X, +Fspec, +V) :-
%   basically, sets the Fspec-th field of class or object X to value V
%   iff it is assignable
%
%   X can be:
%     a class instance
%       (for static or non-static fields)
%     an array
%       (for indexed element or subrange assignment)
%     a classname, or a class/2 or array/1 type
%       (for static fields)
%   but not:
%     a String (no fields to retrieve)
%
%   Fspec can be:
%     an atomic field name
%       (overloading through shadowing has yet to be handled properly)
%     an array index I
%       (X must be an array object: V is assigned to X[I])
%     a pair I-J of integers
%       (X must be an array object, V must be a list of values: successive members of V are assigned to X[I..J])
%
%   V must be a suitable value or object

jpl_set(X, Fspec, V) :-
	(   jpl_object_to_type(X, Type)         % the usual case (test is safe if X is var or rubbish)
	->  Obj = X,
	catch(
	    jpl_set_instance(Type, Type, Obj, Fspec, V),    % first 'Type' is for FAI
	    error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
	    throw(error(type_error(acyclic,Te),context(jpl_set/3,Msg)))
	)
	;   var(X)
	->  throw(error(instantiation_error,
		    context(jpl_set/3,
			    '1st arg must be an object, classname, descriptor or type')))
	;   (   atom(X)
	->  (   jpl_classname_to_type(X, Type)          % it's a classname or descriptor...
	    ->  true
	    ;   throw(error(existence_error(class,X),
			context(jpl_set/3,
				'the named class cannot be found')))
	    )
	;   (   X = class(_,_)                          % it's a class type...
	    ;   X = array(_)                            % ...or an array type
	    )
	->  Type = X
	),
	(   jpl_type_to_class( Type, ClassObj)      % ...whose Class object is available
	->  true
	;   jpl_type_to_classname( Type, Classname),
	    throw(error(existence_error(class,Classname),
		    context(jpl_set/3,
			    'the class cannot be found')))
	)
	->  catch(
	    jpl_set_static(Type, ClassObj, Fspec, V),
	    error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
	    throw(error(type_error(acyclic,Te),context(jpl_set/3,Msg)))
	)
	;   throw(error(domain_error(object_or_class,X),
		    context(jpl_set/3,
			    '1st arg must be an object, classname, descriptor or type')))
	).

%------------------------------------------------------------------------------

% jpl_set_instance(+Type, +Type, +ObjectReference, +FieldName, +Value) :-
%   ObjectReference is a JPL reference to a Java object
%   of the class denoted by Type (which is passed twice for first agument indexing);
%   FieldName should name a public, non-final (static or non-static) field of this object,
%   but could be anything, and is validated here;
%   Value should be assignable to the named field, but could be anything, and is validated here

jpl_set_instance(class(_,_), Type, Obj, Fname, V) :-    % a non-array object
	(   atom(Fname)                 % the usual case
	->  true
	;   var(Fname)
	->  throw(error(instantiation_error,
		    context(jpl_set/3,
			    '2nd arg must be bound to the name of a public, non-final field')))
	;   throw(error(type_error(field_name,Fname),
		    context(jpl_set/3,
			    '2nd arg must be the name of a public, non-final field')))
	),
	findall(
	z4(I,Mods,FID,Tf),
	jpl_field_spec(Type, I, Fname, Mods, FID, Tf),  % public fields of class denoted by Type
	Z4s
	),
	(   Z4s = []
	->  throw(error(existence_error(field,Fname),
		    context(jpl_set/3,
			    'no public fields of the object have this name')))
	;   Z4s = [z4(I,Mods,FID,Tf)]
	->  (   member(final, Mods)
	->  throw(error(permission_error(modify,final_field,Fname),
		    context(jpl_set/3,
			    'cannot assign a value to a final field (actually you could but I''ve decided not to let you)')))
	;   jpl_datum_to_type( V, Tv)
	->  (   jpl_type_fits_type( Tv, Tf)
	    ->  (   member(static, Mods)
		->  jpl_object_to_class(Obj, ClassObj),
		    jpl_set_static_field(Tf, ClassObj, FID, V)
		;   jpl_set_instance_field(Tf, Obj, FID, V)         % oughta be jpl_set_instance_field?
		)
	    ;   jpl_type_to_nicename( Tf, NNf),
		throw(error(type_error(NNf,V),
		    context(jpl_set/3,
			    'the value is not assignable to the named field of the class')))
	    )
	;   throw(error(type_error(field_value,V),
		    context(jpl_set/3,
			    '3rd arg does not represent any Java value or object')))
	)
	;   throw(error(existence_error(field,Fname),   % 'existence'? or some other sort of error maybe?
		    context(jpl_set/3,
			    'more than one public field of the object has this name (this should not happen)')))
	).


jpl_set_instance(array(Type), _, Obj, Fspec, V) :-
	(   is_list(V)                  % a list of array element values
	->  Vs = V
	;   var(V)
	->  throw(error(instantiation_error,
		    context(jpl_set/3, 'when 1st arg is an array, 3rd arg must be bound to a suitable element value or list of values')))
	;   Vs = [V]                    % a single array element value
	),
	length(Vs, Iv),
	(   var(Fspec)
	->  throw(error(instantiation_error,
		    context(jpl_set/3,
			    'when 1st arg is an array, 2nd arg must be bound to an index or index range')))
	;   integer(Fspec)          % single-element assignment
	->  (   Fspec < 0
	->  throw(error(domain_error(array_index,Fspec),
		    context(jpl_set/3,
			    'when 1st arg is an array, an integral 2nd arg must be a non-negative index')))
	;   Iv is 1
	->  N is Fspec
	;   Iv is 0
	->  throw(error(domain_error(array_element(Fspec),Vs),
			context(jpl_set/3,
				'no values for array element assignment: needs one')))
	;   throw(error(domain_error(array_element(Fspec),Vs),
			context(jpl_set/3,
				'too many values for array element assignment: needs one')))
	)
	;   Fspec = N-M             % element-sequence assignment
	->  (   integer(N),
	    integer(M)
	->  (   N >= 0,
		Size is (M-N)+1,
		Size >= 0
	    ->  (   Size == Iv
		->  true
		;   Size < Iv
		->  throw(error(domain_error(array_elements(N-M),Vs),
				context(jpl_set/3,
					'too few values for array range assignment')))
		;   throw(error(domain_error(array_elements(N-M),Vs),
				context(jpl_set/3,
					'too many values for array range assignment')))
		)
	    ;   throw(error(domain_error(array_index_range,N-M),
		    context(jpl_set/3,
			    'array index range must be a non-decreasing pair of non-negative integers')))
	    )
	;   throw(error(type_error(array_index_range,N-M),
		    context(jpl_set/3,
			    'array index range must be a non-decreasing pair of non-negative integers')))
	)
	;   atom(Fspec)
	->  (   Fspec == length
	->  throw(error(permission_error(modify,final_field,length),
			    context(jpl_set/3,
				    'cannot assign a value to a final field')))
	;   throw(error(existence_error(field,Fspec),
			    context(jpl_set/3,
				    'array has no field with that name')))
	)
	;   throw(error(domain_error(array_index,Fspec),
		    context(jpl_set/3,
			    'when 1st arg is an array object, 2nd arg must be a non-negative index or index range')))
	),
	jpl_set_array(Type, Obj, N, Iv, Vs).

%------------------------------------------------------------------------------

% jpl_set_static(+Type, +ClassObj, +FieldName, +Value) :-
%   we can rely on:
%       Type being a class/2 type representing some accessible class
%       ClassObj being an instance of java.lang.Class which represents the same class as Type
%   but FieldName could be anything, so we validate it here,
%   look for a suitable (static) field of the target class,
%   then call jpl_set_static_field/4 to attempt to assign Value (which could be anything) to it
%
%   NB this does not yet handle shadowed fields correctly...

jpl_set_static(Type, ClassObj, Fname, V) :-
	(   atom(Fname)                     % the usual case
	->  true
	;   var(Fname)
	->  throw(error(instantiation_error,
		    context(jpl_set/3,
			    'when 1st arg denotes a class, 2nd arg must be bound to the name of a public, static, non-final field')))
	;   throw(error(type_error(field_name,Fname),
		    context(jpl_set/3,
			    'when 1st arg denotes a class, 2nd arg must be the name of a public, static, non-final field')))
	),
	findall(  % get all static fields of the denoted class
	z4(I,Mods,FID,Tf),
	(   jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
	    member(static, Mods)
	),
	Z4s
	),
	(   Z4s = []
	->  throw(error(existence_error(field,Fname),
		    context(jpl_set/3,
			    'class has no public static fields of this name')))
	;   Z4s = [z4(I,Mods,FID,Tf)]       % exactly one synonymous field?
	->  (   member(final, Mods)
	->  throw(error(permission_error(modify,final_field,Fname),
		    context(jpl_set/3,
			    'cannot assign a value to a final field')))
	;   jpl_datum_to_type(V, Tv)
	->  (   jpl_type_fits_type(Tv, Tf)
	    ->  jpl_set_static_field(Tf, ClassObj, FID, V)
	    ;   jpl_type_to_nicename(Tf, NNf),
		throw(error(type_error(NNf,V),
		    context(jpl_set/3,
			    'the value is not assignable to the named field of the class')))
	    )
	;   throw(error(type_error(field_value,V),
		    context(jpl_set/3,
			    '3rd arg does not represent any Java value or object')))
	)
	;   throw(error(existence_error(field,Fname),
		    context(jpl_set/3,
			    'more than one public static field of the class has this name (this should not happen)(?)')))
	).

%------------------------------------------------------------------------------

%%	jpl_set_array(+ElementType, +Array, +Offset, +DatumQty, +Datums)
%
%	Datums, of which there are DatumQty,   are stashed in successive
%	elements of Array which is an   array of ElementType starting at
%	the      Offset-th      (numbered      from       0)      throws
%	error(type_error(acyclic,_),context(jpl_datum_to_type/2,_))

jpl_set_array(T, A, N, I, Ds) :-
	(   jpl_datums_to_types(Ds, Tds)        % most specialised types of given values
	->  (   jpl_types_fit_type(Tds, T)      % all assignable to element type?
	    ->  true
	    ;   throw(error(type_error(array(T),Ds),
			    context(jpl_set/3,
				    'not all values are assignable to the array element type')))
	    )
	;   throw(error(type_error(array(T),Ds),
		    context(jpl_set/3,
			    'not all values are convertible to Java values or references')))
	),
	(   (   T = class(_,_)
	    ;   T = array(_)                    % array elements are objects
	    )
	->  (   nth0(J, Ds, D),                 % for each datum
	        Nd is N+J,                      % compute array index
		(   D = {Tq}                    % quoted term?
		->  jni_term_to_jref(Tq, D2)    % convert to a JPL reference to a corresponding jpl.Term object
		;   D = D2
		),
		jSetObjectArrayElement(A, Nd, D2),
		fail                            % iterate
	    ;   true
	    )
	;   jpl_primitive_type(T)               % array elements are primitive values
	->  jni_type_to_xput_code(T, Xc),
	    jni_alloc_buffer(Xc, I, Bp),        % I-element buf of required primitive type
	    jpl_set_array_1(Ds, T, 0, Bp),
	    jpl_set_elements(T, A, N, I, Bp),
	    jni_free_buffer(Bp)
	;   throw(error(system_error(array_element_type,T),
		    context(jpl_set/3,
			    'array element type is unknown (this should not happen)')))
	).

%------------------------------------------------------------------------------

%%	jpl_set_array_1(+Values, +Type, +BufferIndex, +BufferPointer)
%
%	successive members of Values  are   stashed  as (primitive) Type
%	from the BufferIndex-th element (numbered from 0) onwards of the
%	buffer indicated by BufferPointer NB  this   could  be done more
%	efficiently (?) within foreign code...

jpl_set_array_1([], _, _, _).
jpl_set_array_1([V|Vs], Tprim, Ib, Bp) :-
	jni_type_to_xput_code(Tprim, Xc),
	jni_stash_buffer_value(Bp, Ib, V, Xc),
	Ibnext is Ib+1,
	jpl_set_array_1(Vs, Tprim, Ibnext, Bp).

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

%%	jpl_set_instance_field(+Type, +Obj, +FieldID, +V)
%
%	we can rely on Type, Obj and FieldID being valid, and on V being
%	assignable (if V is a quoted term then it is converted here)

jpl_set_instance_field(boolean, Obj, FieldID, V) :-
	jSetBooleanField(Obj, FieldID, V).
jpl_set_instance_field(byte, Obj, FieldID, V) :-
	jSetByteField(Obj, FieldID, V).
jpl_set_instance_field(char, Obj, FieldID, V) :-
	jSetCharField(Obj, FieldID, V).
jpl_set_instance_field(short, Obj, FieldID, V) :-
	jSetShortField(Obj, FieldID, V).
jpl_set_instance_field(int, Obj, FieldID, V) :-
	jSetIntField(Obj, FieldID, V).
jpl_set_instance_field(long, Obj, FieldID, V) :-
	jSetLongField(Obj, FieldID, V).
jpl_set_instance_field(float, Obj, FieldID, V) :-
	jSetFloatField(Obj, FieldID, V).
jpl_set_instance_field(double, Obj, FieldID, V) :-
	jSetDoubleField(Obj, FieldID, V).
jpl_set_instance_field(class(_,_), Obj, FieldID, V) :-  % also handles byval term assignments
	(   V = {T}                     % quoted term?
	->  jni_term_to_jref(T, V2)     % convert to a JPL reference to a corresponding jpl.Term object
	;   V = V2
	),
	jSetObjectField(Obj, FieldID, V2).
jpl_set_instance_field(array(_), Obj, FieldID, V) :-
	jSetObjectField(Obj, FieldID, V).

%------------------------------------------------------------------------------

% jpl_set_static_field(+Type, +ClassObj, +FieldID, +V) :-
%   we can rely on Type, ClassObj and FieldID being valid,
%   and on V being assignable (if V is a quoted term then it is converted here)

jpl_set_static_field(boolean, Obj, FieldID, V) :-
	jSetStaticBooleanField(Obj, FieldID, V).

jpl_set_static_field(byte, Obj, FieldID, V) :-
	jSetStaticByteField(Obj, FieldID, V).

jpl_set_static_field(char, Obj, FieldID, V) :-
	jSetStaticCharField(Obj, FieldID, V).

jpl_set_static_field(short, Obj, FieldID, V) :-
	jSetStaticShortField(Obj, FieldID, V).

jpl_set_static_field(int, Obj, FieldID, V) :-
	jSetStaticIntField(Obj, FieldID, V).

jpl_set_static_field(long, Obj, FieldID, V) :-
	jSetStaticLongField(Obj, FieldID, V).

jpl_set_static_field(float, Obj, FieldID, V) :-
	jSetStaticFloatField(Obj, FieldID, V).

jpl_set_static_field(double, Obj, FieldID, V) :-
	jSetStaticDoubleField(Obj, FieldID, V).

jpl_set_static_field(class(_,_), Obj, FieldID, V) :-    % also handles byval term assignments
	(   V = {T}                         % quoted term?
	->  jni_term_to_jref(T, V2)         % convert to a JPL reference to a corresponding jpl.Term object
	;   V = V2
	),
	jSetStaticObjectField(Obj, FieldID, V2).

jpl_set_static_field(array(_), Obj, FieldID, V) :-
	jSetStaticObjectField(Obj, FieldID, V).

%------------------------------------------------------------------------------

% jpl_z3s_to_most_specific_z3(+Zs, -Z) :-
%   Zs is a list of arity-matching, type-suitable z3(I,MID,Tfps)
%   Z is the single most specific element of Zs,
%   i.e. that than which no other z3/3 has a more specialised signature;
%   fails if there is more than one such

jpl_z3s_to_most_specific_z3(Zs, Z) :-
	jpl_fergus_is_the_greatest(Zs, Z).

%------------------------------------------------------------------------------

% jpl_z5s_to_most_specific_z5(+Zs, -Z) :-
%   Zs is a list of arity-matching, type-suitable z5(I,Mods,MID,Tr,Tfps)
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


jpl_pl_lib_version(3, 1, 3, alpha).

%------------------------------------------------------------------------------

% jpl_type_alfa(0'$) -->        % presumably not allowed
%   "$".                        % given the "inner class" syntax?

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

jpl_type_id_v2(A) -->                   % inner class name parts (empirically)
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

%type   jCallBooleanMethod(object, method_id, types, datums, boolean)

% jCallBooleanMethod(+Obj, +MethodID, +Types, +Params, -Rbool) :-

jCallBooleanMethod(Obj, MethodID, Types, Params, Rbool) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(39, Obj, MethodID, ParamBuf, Rbool).

%------------------------------------------------------------------------------

%type   jCallByteMethod(object, method_id, types, datums, byte)

% jCallByteMethod(+Obj, +MethodID, +Types, +Params, -Rbyte) :-

jCallByteMethod(Obj, MethodID, Types, Params, Rbyte) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(42, Obj, MethodID, ParamBuf, Rbyte).

%------------------------------------------------------------------------------

%type   jCallCharMethod(object, method_id, types, datums, char)

% jCallCharMethod(+Obj, +MethodID, +Types, +Params, -Rchar) :-

jCallCharMethod(Obj, MethodID, Types, Params, Rchar) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(45, Obj, MethodID, ParamBuf, Rchar).

%------------------------------------------------------------------------------

%type   jCallDoubleMethod(object, method_id, types, datums, double)

% jCallDoubleMethod(+Obj, +MethodID, +Types, +Params, -Rdouble) :-

jCallDoubleMethod(Obj, MethodID, Types, Params, Rdouble) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(60, Obj, MethodID, ParamBuf, Rdouble).

%------------------------------------------------------------------------------

%type   jCallFloatMethod(object, method_id, types, datums, float)

% jCallFloatMethod(+Obj, +MethodID, +Types, +Params, -Rfloat) :-

jCallFloatMethod(Obj, MethodID, Types, Params, Rfloat) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(57, Obj, MethodID, ParamBuf, Rfloat).

%------------------------------------------------------------------------------

%type   jCallIntMethod(object, method_id, types, datums, int)

% jCallIntMethod(+Obj, +MethodID, +Types, +Params, -Rint) :-

jCallIntMethod(Obj, MethodID, Types, Params, Rint) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(51, Obj, MethodID, ParamBuf, Rint).

%------------------------------------------------------------------------------

%type   jCallLongMethod(object, method_id, types, datums, long)

% jCallLongMethod(+Obj, +MethodID, +Types, +Params, -Rlong) :-

jCallLongMethod(Obj, MethodID, Types, Params, Rlong) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(54, Obj, MethodID, ParamBuf, Rlong).

%------------------------------------------------------------------------------

%type   jCallObjectMethod(object, method_id, types, datums, object)

% jCallObjectMethod(+Obj, +MethodID, +Types, +Params, -Robj) :-

jCallObjectMethod(Obj, MethodID, Types, Params, Robj) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(36, Obj, MethodID, ParamBuf, Robj).

%------------------------------------------------------------------------------

%type   jCallShortMethod(object, method_id, types, datums, short)

% jCallShortMethod(+Obj, +MethodID, +Types, +Params, -Rshort) :-

jCallShortMethod(Obj, MethodID, Types, Params, Rshort) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(48, Obj, MethodID, ParamBuf, Rshort).

%------------------------------------------------------------------------------

%type   jCallStaticBooleanMethod(class, types, datums, boolean)

% jCallStaticBooleanMethod(+Class, +MethodID, +Types, +Params, -Rbool) :-

jCallStaticBooleanMethod(Class, MethodID, Types, Params, Rbool) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(119, Class, MethodID, ParamBuf, Rbool).

%------------------------------------------------------------------------------

%type   jCallStaticByteMethod(class, method_id, types, datums, byte)

% jCallStaticByteMethod(+Class, +MethodID, +Types, +Params, -Rbyte) :-

jCallStaticByteMethod(Class, MethodID, Types, Params, Rbyte) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(122, Class, MethodID, ParamBuf, Rbyte).

%------------------------------------------------------------------------------

%type   jCallStaticCharMethod(class, method_id, types, datums, char)

% jCallStaticCharMethod(+Class, +MethodID, +Types, +Params, -Rchar) :-

jCallStaticCharMethod(Class, MethodID, Types, Params, Rchar) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(125, Class, MethodID, ParamBuf, Rchar).

%------------------------------------------------------------------------------

%type   jCallStaticDoubleMethod(class, method_id, types, datums, double)

% jCallStaticDoubleMethod(+Class, +MethodID, +Types, +Params, -Rdouble) :-

jCallStaticDoubleMethod(Class, MethodID, Types, Params, Rdouble) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(140, Class, MethodID, ParamBuf, Rdouble).

%------------------------------------------------------------------------------

%type   jCallStaticFloatMethod(class, method_id, types, datums, float)

% jCallStaticFloatMethod(+Class, +MethodID, +Types, +Params, -Rfloat) :-

jCallStaticFloatMethod(Class, MethodID, Types, Params, Rfloat) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(137, Class, MethodID, ParamBuf, Rfloat).

%------------------------------------------------------------------------------

%type   jCallStaticIntMethod(class, method_id, types, datums, int)

% jCallStaticIntMethod(+Class, +MethodID, +Types, +Params, -Rint) :-

jCallStaticIntMethod(Class, MethodID, Types, Params, Rint) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(131, Class, MethodID, ParamBuf, Rint).

%------------------------------------------------------------------------------

%type   jCallStaticLongMethod(class, method_id, types, datums, long)

% jCallStaticLongMethod(+Class, +MethodID, +Types, +Params, -Rlong) :-

jCallStaticLongMethod(Class, MethodID, Types, Params, Rlong) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(134, Class, MethodID, ParamBuf, Rlong).

%------------------------------------------------------------------------------

%type   jCallStaticObjectMethod(class, method_id, types, datums, object)

% jCallStaticObjectMethod(+Class, +MethodID, +Types, +Params, -Robj) :-

jCallStaticObjectMethod(Class, MethodID, Types, Params, Robj) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(116, Class, MethodID, ParamBuf, Robj).

%------------------------------------------------------------------------------

%type   jCallStaticShortMethod(class, method_id, types, datums, short)

% jCallStaticShortMethod(+Class, +MethodID, +Types, +Params, -Rshort) :-

jCallStaticShortMethod(Class, MethodID, Types, Params, Rshort) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(128, Class, MethodID, ParamBuf, Rshort).

%------------------------------------------------------------------------------

%type   jCallStaticVoidMethod(class, method_id, types, datums)

% jCallStaticVoidMethod(+Class, +MethodID, +Types, +Params) :-

jCallStaticVoidMethod(Class, MethodID, Types, Params) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_void(143, Class, MethodID, ParamBuf).

%------------------------------------------------------------------------------

%type   jCallVoidMethod(object, method_id, types, datums)

% jCallVoidMethod(+Obj, +MethodID, +Types, +Params) :-

jCallVoidMethod(Obj, MethodID, Types, Params) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_void(63, Obj, MethodID, ParamBuf).

%------------------------------------------------------------------------------

%type   jFindClass(findclassname, class)

% jFindClass(+ClassName, -Class) :-

jFindClass(ClassName, Class) :-
	jni_func(6, ClassName, Class).

%------------------------------------------------------------------------------

%type   jGetArrayLength(array, int)

% jGetArrayLength(+Array, -Size) :-

jGetArrayLength(Array, Size) :-
	jni_func(171, Array, Size).

%------------------------------------------------------------------------------

%type   jGetBooleanArrayRegion(boolean_array, int, int, boolean_buf)

% jGetBooleanArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetBooleanArrayRegion(Array, Start, Len, Buf) :-
	jni_void(199, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetBooleanField(object, field_id, boolean)

% jGetBooleanField(+Obj, +FieldID, -Rbool) :-

jGetBooleanField(Obj, FieldID, Rbool) :-
	jni_func(96, Obj, FieldID, Rbool).

%------------------------------------------------------------------------------

%type   jGetByteArrayRegion(byte_array, int, int, byte_buf)

% jGetByteArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetByteArrayRegion(Array, Start, Len, Buf) :-
	jni_void(200, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetByteField(object, field_id, byte)

% jGetByteField(+Obj, +FieldID, -Rbyte) :-

jGetByteField(Obj, FieldID, Rbyte) :-
	jni_func(97, Obj, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type   jGetCharArrayRegion(char_array, int, int, char_buf)

% jGetCharArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetCharArrayRegion(Array, Start, Len, Buf) :-
	jni_void(201, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetCharField(object, field_id, char)

% jGetCharField(+Obj, +FieldID, -Rchar) :-

jGetCharField(Obj, FieldID, Rchar) :-
	jni_func(98, Obj, FieldID, Rchar).

%------------------------------------------------------------------------------

%type   jGetDoubleArrayRegion(double_array, int, int, double_buf)

% jGetDoubleArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetDoubleArrayRegion(Array, Start, Len, Buf) :-
	jni_void(206, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetDoubleField(object, field_id, double)

% jGetDoubleField(+Obj, +FieldID, -Rdouble) :-

jGetDoubleField(Obj, FieldID, Rdouble) :-
	jni_func(103, Obj, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type   jGetFieldID(class, descriptor, field_id)

% jGetFieldID(+Class, +Name, +Typedescriptor, -FieldID) :-

jGetFieldID(Class, Name, Type, FieldID) :-
	jpl_type_to_descriptor(Type, TD),
	jni_func(94, Class, Name, TD, FieldID).

%------------------------------------------------------------------------------

%type   jGetFloatArrayRegion(float_array, int, int, float_buf)

% jGetFloatArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetFloatArrayRegion(Array, Start, Len, Buf) :-
	jni_void(205, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetFloatField(object, field_id, float)

% jGetFloatField(+Obj, +FieldID, -Rfloat) :-

jGetFloatField(Obj, FieldID, Rfloat) :-
	jni_func(102, Obj, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type   jGetIntArrayRegion(int_array, int, int, int_buf)

% jGetIntArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetIntArrayRegion(Array, Start, Len, Buf) :-
	jni_void(203, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetIntField(object, field_id, int)

% jGetIntField(+Obj, +FieldID, -Rint) :-

jGetIntField(Obj, FieldID, Rint) :-
	jni_func(100, Obj, FieldID, Rint).

%------------------------------------------------------------------------------

%type   jGetLongArrayRegion(long_array, int, int, long_buf)

% jGetLongArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetLongArrayRegion(Array, Start, Len, Buf) :-
	jni_void(204, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetLongField(object, field_id, long)

% jGetLongField(+Obj, +FieldID, -Rlong) :-

jGetLongField(Obj, FieldID, Rlong) :-
	jni_func(101, Obj, FieldID, Rlong).

%------------------------------------------------------------------------------

%type   jGetMethodID(class, name, descriptor, method_id)

% jGetMethodID(+Class, +Name, +TypeDescriptor, -MethodID) :-

jGetMethodID(Class, Name, Type, MethodID) :-
	jpl_type_to_descriptor(Type, TD),
	jni_func(33, Class, Name, TD, MethodID).

%------------------------------------------------------------------------------

%type   jGetObjectArrayElement(object_array, int, object)

% jGetObjectArrayElement(+Array, +Index, -Obj) :-

jGetObjectArrayElement(Array, Index, Obj) :-
	jni_func(173, Array, Index, Obj).

%------------------------------------------------------------------------------

%type   jGetObjectClass(object, class)

% jGetObjectClass(+Object, -Class) :-

jGetObjectClass(Object, Class) :-
	jni_func(31, Object, Class).

%------------------------------------------------------------------------------

%type   jGetObjectField(object, field_id, object)

% jGetObjectField(+Obj, +FieldID, -RObj) :-

jGetObjectField(Obj, FieldID, Robj) :-
	jni_func(95, Obj, FieldID, Robj).

%------------------------------------------------------------------------------

%type   jGetShortArrayRegion(short_array, int, int, short_buf)

% jGetShortArrayRegion(+Array, +Start, +Len, +Buf) :-

jGetShortArrayRegion(Array, Start, Len, Buf) :-
	jni_void(202, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jGetShortField(object, field_id, short)

% jGetShortField(+Obj, +FieldID, -Rshort) :-

jGetShortField(Obj, FieldID, Rshort) :-
	jni_func(99, Obj, FieldID, Rshort).

%------------------------------------------------------------------------------

%type   jGetStaticBooleanField(class, field_id, boolean)

% jGetStaticBooleanField(+Class, +FieldID, -Rbool) :-

jGetStaticBooleanField(Class, FieldID, Rbool) :-
	jni_func(146, Class, FieldID, Rbool).

%------------------------------------------------------------------------------

%type   jGetStaticByteField(class, field_id, byte)

% jGetStaticByteField(+Class, +FieldID, -Rbyte) :-

jGetStaticByteField(Class, FieldID, Rbyte) :-
	jni_func(147, Class, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type   jGetStaticCharField(class, field_id, char)

% jGetStaticCharField(+Class, +FieldID, -Rchar) :-

jGetStaticCharField(Class, FieldID, Rchar) :-
	jni_func(148, Class, FieldID, Rchar).

%------------------------------------------------------------------------------

%type   jGetStaticDoubleField(class, field_id, double)

% jGetStaticDoubleField(+Class, +FieldID, -Rdouble) :-

jGetStaticDoubleField(Class, FieldID, Rdouble) :-
	jni_func(153, Class, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type   jGetStaticFieldID(class, name, field_id)

% jGetStaticFieldID(+Class, +Name, +TypeDescriptor, -FieldID) :-

jGetStaticFieldID(Class, Name, Type, FieldID) :-
	jpl_type_to_descriptor(Type, TD),               % cache this?
	jni_func(144, Class, Name, TD, FieldID).

%------------------------------------------------------------------------------

%type   jGetStaticFloatField(class, field_id, float)

% jGetStaticFloatField(+Class, +FieldID, -Rfloat) :-

jGetStaticFloatField(Class, FieldID, Rfloat) :-
	jni_func(152, Class, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type   jGetStaticIntField(class, field_id, int)

% jGetStaticIntField(+Class, +FieldID, -Rint) :-

jGetStaticIntField(Class, FieldID, Rint) :-
	jni_func(150, Class, FieldID, Rint).

%------------------------------------------------------------------------------

%type   jGetStaticLongField(class, field_id, long)

% jGetStaticLongField(+Class, +FieldID, -Rlong) :-

jGetStaticLongField(Class, FieldID, Rlong) :-
	jni_func(151, Class, FieldID, Rlong).

%------------------------------------------------------------------------------

%type   jGetStaticMethodID(class, name, method_id)

% jGetStaticMethodID(+Class, +Name, +TypeDescriptor, -MethodID) :-

jGetStaticMethodID(Class, Name, Type, MethodID) :-
	jpl_type_to_descriptor(Type, TD),
	jni_func(113, Class, Name, TD, MethodID).

%------------------------------------------------------------------------------

%type   jGetStaticObjectField(class, field_id, object)

% jGetStaticObjectField(+Class, +FieldID, -RObj) :-

jGetStaticObjectField(Class, FieldID, Robj) :-
	jni_func(145, Class, FieldID, Robj).

%------------------------------------------------------------------------------

%type   jGetStaticShortField(class, field_id, short)

% jGetStaticShortField(+Class, +FieldID, -Rshort) :-

jGetStaticShortField(Class, FieldID, Rshort) :-
	jni_func(149, Class, FieldID, Rshort).

%------------------------------------------------------------------------------

%type   jGetSuperclass(object, object)

% jGetSuperclass(+Class1, -Class2) :-

jGetSuperclass(Class1, Class2) :-
	jni_func(10, Class1, Class2).

%------------------------------------------------------------------------------

%type   jIsAssignableFrom(object, object)

% jIsAssignableFrom(+Class1, +Class2) :-

jIsAssignableFrom(Class1, Class2) :-
	jni_func(11, Class1, Class2, @(true)).

%------------------------------------------------------------------------------

%type   jNewBooleanArray(int, boolean_array)

% jNewBooleanArray(+Length, -Array) :-

jNewBooleanArray(Length, Array) :-
	jni_func(175, Length, Array).

%------------------------------------------------------------------------------

%type   jNewByteArray(int, byte_array)

% jNewByteArray(+Length, -Array) :-

jNewByteArray(Length, Array) :-
	jni_func(176, Length, Array).

%------------------------------------------------------------------------------

%type   jNewCharArray(int, char_array)

% jNewCharArray(+Length, -Array) :-

jNewCharArray(Length, Array) :-
	jni_func(177, Length, Array).

%------------------------------------------------------------------------------

%type   jNewDoubleArray(int, double_array)

% jNewDoubleArray(+Length, -Array) :-

jNewDoubleArray(Length, Array) :-
	jni_func(182, Length, Array).

%------------------------------------------------------------------------------

%type   jNewFloatArray(int, float_array)

% jNewFloatArray(+Length, -Array) :-

jNewFloatArray(Length, Array) :-
	jni_func(181, Length, Array).

%------------------------------------------------------------------------------

%type   jNewIntArray(int, int_array)

% jNewIntArray(+Length, -Array) :-

jNewIntArray(Length, Array) :-
	jni_func(179, Length, Array).

%------------------------------------------------------------------------------

%type   jNewLongArray(int, long_array)

% jNewLongArray(+Length, -Array) :-

jNewLongArray(Length, Array) :-
	jni_func(180, Length, Array).

%------------------------------------------------------------------------------

%type   jNewObject(class, method_id, types, datums, object)

% jNewObject(+Class, +MethodID, +Types, +Params, -Obj) :-

jNewObject(Class, MethodID, Types, Params, Obj) :-
	jni_params_put(Params, Types, ParamBuf),
	jni_func(30, Class, MethodID, ParamBuf, Obj).

%------------------------------------------------------------------------------

%type   jNewObjectArray(int, class, object, object_array)

% jNewObjectArray(+Len, +Class, +InitVal, -Array) :-

jNewObjectArray(Len, Class, InitVal, Array) :-
	jni_func(172, Len, Class, InitVal, Array).

%------------------------------------------------------------------------------

%type   jNewShortArray(int, short_array)

% jNewShortArray(+Length, -Array) :-

jNewShortArray(Length, Array) :-
	jni_func(178, Length, Array).

%------------------------------------------------------------------------------

%type   jSetBooleanArrayRegion(boolean_array, int, int, boolean_buf)

% jSetBooleanArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetBooleanArrayRegion(Array, Start, Len, Buf) :-
	jni_void(207, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetBooleanField(object, field_id, boolean)

% jSetBooleanField(+Obj, +FieldID, +Rbool) :-

jSetBooleanField(Obj, FieldID, Rbool) :-
	jni_void(105, Obj, FieldID, Rbool).

%------------------------------------------------------------------------------

%type   jSetByteArrayRegion(byte_array, int, int, byte_buf)

% jSetByteArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetByteArrayRegion(Array, Start, Len, Buf) :-
	jni_void(208, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetByteField(object, field_id, byte)

% jSetByteField(+Obj, +FieldID, +Rbyte) :-

jSetByteField(Obj, FieldID, Rbyte) :-
	jni_void(106, Obj, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type   jSetCharArrayRegion(char_array, int, int, char_buf)

% jSetCharArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetCharArrayRegion(Array, Start, Len, Buf) :-
	jni_void(209, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetCharField(object, field_id, char)

% jSetCharField(+Obj, +FieldID, +Rchar) :-

jSetCharField(Obj, FieldID, Rchar) :-
	jni_void(107, Obj, FieldID, Rchar).

%------------------------------------------------------------------------------

%type   jSetDoubleArrayRegion(double_array, int, int, double_buf)

% jSetDoubleArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetDoubleArrayRegion(Array, Start, Len, Buf) :-
	jni_void(214, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetDoubleField(object, field_id, double)

% jSetDoubleField(+Obj, +FieldID, +Rdouble) :-

jSetDoubleField(Obj, FieldID, Rdouble) :-
	jni_void(112, Obj, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type   jSetFloatArrayRegion(float_array, int, int, float_buf)

% jSetFloatArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetFloatArrayRegion(Array, Start, Len, Buf) :-
	jni_void(213, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetFloatField(object, field_id, float)

% jSetFloatField(+Obj, +FieldID, +Rfloat) :-

jSetFloatField(Obj, FieldID, Rfloat) :-
	jni_void(111, Obj, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type   jSetIntArrayRegion(int_array, int, int, int_buf)

% jSetIntArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetIntArrayRegion(Array, Start, Len, Buf) :-
	jni_void(211, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetIntField(object, field_id, int)

% jSetIntField(+Obj, +FieldID, +Rint) :-

jSetIntField(Obj, FieldID, Rint) :-
	jni_void(109, Obj, FieldID, Rint).

%------------------------------------------------------------------------------

%type   jSetLongArrayRegion(long_array, int, int, long_buf)

% jSetLongArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetLongArrayRegion(Array, Start, Len, Buf) :-
	jni_void(212, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetLongField(object, field_id, long)

% jSetLongField(+Obj, +FieldID, +Rlong) :-

jSetLongField(Obj, FieldID, Rlong) :-
	jni_void(110, Obj, FieldID, Rlong).

%------------------------------------------------------------------------------

%type   jSetObjectArrayElement(object_array, int, object)

% jSetObjectArrayElement(+Array, +Index, +Obj) :-

jSetObjectArrayElement(Array, Index, Obj) :-
	jni_void(174, Array, Index, Obj).

%------------------------------------------------------------------------------

%type   jSetObjectField(object, field_id, object)

% jSetObjectField(+Obj, +FieldID, +RObj) :-

jSetObjectField(Obj, FieldID, Robj) :-
	jni_void(104, Obj, FieldID, Robj).

%------------------------------------------------------------------------------

%type   jSetShortArrayRegion(short_array, int, int, short_buf)

% jSetShortArrayRegion(+Array, +Start, +Len, +Buf) :-

jSetShortArrayRegion(Array, Start, Len, Buf) :-
	jni_void(210, Array, Start, Len, Buf).

%------------------------------------------------------------------------------

%type   jSetShortField(object, field_id, short)

% jSetShortField(+Obj, +FieldID, +Rshort) :-

jSetShortField(Obj, FieldID, Rshort) :-
	jni_void(108, Obj, FieldID, Rshort).

%------------------------------------------------------------------------------

%type   jSetStaticBooleanField(class, field_id, boolean)

% jSetStaticBooleanField(+Class, +FieldID, +Rbool) :-

jSetStaticBooleanField(Class, FieldID, Rbool) :-
	jni_void(155, Class, FieldID, Rbool).

%------------------------------------------------------------------------------

%type   jSetStaticByteField(class, field_id, byte)

% jSetStaticByteField(+Class, +FieldID, +Rbyte) :-

jSetStaticByteField(Class, FieldID, Rbyte) :-
	jni_void(156, Class, FieldID, Rbyte).

%------------------------------------------------------------------------------

%type   jSetStaticCharField(class, field_id, char)

% jSetStaticCharField(+Class, +FieldID, +Rchar) :-

jSetStaticCharField(Class, FieldID, Rchar) :-
	jni_void(157, Class, FieldID, Rchar).

%------------------------------------------------------------------------------

%type   jSetStaticDoubleField(class, field_id, double)

% jSetStaticDoubleField(+Class, +FieldID, +Rdouble) :-

jSetStaticDoubleField(Class, FieldID, Rdouble) :-
	jni_void(162, Class, FieldID, Rdouble).

%------------------------------------------------------------------------------

%type   jSetStaticFloatField(class, field_id, float)

% jSetStaticFloatField(+Class, +FieldID, +Rfloat) :-

jSetStaticFloatField(Class, FieldID, Rfloat) :-
	jni_void(161, Class, FieldID, Rfloat).

%------------------------------------------------------------------------------

%type   jSetStaticIntField(class, field_id, int)

% jSetStaticIntField(+Class, +FieldID, +Rint) :-

jSetStaticIntField(Class, FieldID, Rint) :-
	jni_void(159, Class, FieldID, Rint).

%------------------------------------------------------------------------------

%type   jSetStaticLongField(class, field_id, long)

% jSetStaticLongField(+Class, +FieldID, +Rlong) :-

jSetStaticLongField(Class, FieldID, Rlong) :-
	jni_void(160, Class, FieldID, Rlong).

%------------------------------------------------------------------------------

%type   jSetStaticObjectField(class, field_id, object)

% jSetStaticObjectField(+Class, +FieldID, +Robj) :-

jSetStaticObjectField(Class, FieldID, Robj) :-
	jni_void(154, Class, FieldID, Robj).

%------------------------------------------------------------------------------

%type   jSetStaticShortField(class, field_id, short)

% jSetStaticShortField(+Class, +FieldID, +Rshort) :-

jSetStaticShortField(Class, FieldID, Rshort) :-
	jni_void(158, Class, FieldID, Rshort).

%------------------------------------------------------------------------------

% jni_params_put(+Params, +Types, -ParamBuf)  :-
%   the old form used a static buffer, hence was not re-entrant;
%   the new form allocates a buffer of one jvalue per arg,
%   puts the (converted) args into respective elements, then returns it
%   (the caller is responsible for freeing it)

jni_params_put(As, Ts, ParamBuf)     :-
	jni_ensure_jvm,                     % in case e.g. NewStringUTF() is called
	length(As, N),
	jni_type_to_xput_code(jvalue, Xc), % Xc will be 15
	jni_alloc_buffer(Xc, N, ParamBuf),
	jni_params_put_1(As, 0, Ts, ParamBuf).

%------------------------------------------------------------------------------

% jni_params_put_1(+Params, +N, +JPLTypes, +ParamBuf) :-
%   Params is a (full or partial) list of args-not-yet-stashed,
%   and Types are their (JPL) types (e.g. 'boolean');
%   N is the arg and buffer index (0+) at which the head of Params is to be stashed;
%   the old form used a static buffer and hence was non-reentrant;
%   the new form uses a dynamically allocated buffer (which oughta be freed after use)
%
%NB if the (user-provided) actual params were to be unsuitable for conversion
%NB to the method-required types, this would fail silently (without freeing the buffer);
%NB it's not clear whether the overloaded-method-resolution ensures that all args
%NB are convertible

jni_params_put_1([], _, [], _).

jni_params_put_1([A|As], N, [Tjni|Ts], ParamBuf) :-     % type checking?
	(   jni_type_to_xput_code(Tjni, Xc)
	->  (       A = {Term}                              % a quoted general term?
	->      jni_term_to_jref( Term, Ax)             % convert it to a @(Tag) ref to a new Term instance
	;       A = Ax
	),
	jni_param_put(N, Xc, Ax, ParamBuf)              % foreign
	;   fail                                            % oughta raise an exception?
	),
	N2 is N+1,
	jni_params_put_1(As, N2, Ts, ParamBuf).             % stash remaining params (if any)

%------------------------------------------------------------------------------

% jni_type_to_xput_code(+JspType, -JniXputCode) :-
%   NB JniXputCode determines widening and casting in foreign code
%   NB the codes could be compiled into jni_method_spec_cache etc.
%   instead of, or as well as, types (for - small - efficiency gain)

jni_type_to_xput_code(boolean,      1).     % JNI_XPUT_BOOLEAN

jni_type_to_xput_code(byte,         2).     % JNI_XPUT_BYTE

jni_type_to_xput_code(char,         3).     % JNI_XPUT_CHAR

jni_type_to_xput_code(short,        4).     % JNI_XPUT_SHORT

jni_type_to_xput_code(int,          5).     % JNI_XPUT_INT

jni_type_to_xput_code(long,         6).     % JNI_XPUT_LONG

jni_type_to_xput_code(float,        7).     % JNI_XPUT_FLOAT

jni_type_to_xput_code(double,       8).     % JNI_XPUT_DOUBLE

jni_type_to_xput_code(class(_,_),   12).    % JNI_XPUT_REF

jni_type_to_xput_code(array(_),     12).    % JNI_XPUT_REF

jni_type_to_xput_code(jvalue,       15).    % JNI_XPUT_JVALUE

%------------------------------------------------------------------------------

% jpl_class_to_constructor_array(+Class, -MethodArray) :-
%   might this be done more efficiently in foreign code? or in Java?

jpl_class_to_constructor_array(Cx, Ma) :-
	jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
	jGetMethodID(
	CC,
	getConstructors,
	method([],array(class([java,lang,reflect],['Constructor']))),
	MID
	),                                                  % cacheable?
	jCallObjectMethod(Cx, MID, [], [], Ma).

%------------------------------------------------------------------------------

% jpl_class_to_constructors(+Class, -Methods) :-

jpl_class_to_constructors(Cx, Ms) :-
	jpl_class_to_constructor_array(Cx, Ma),
	jpl_object_array_to_list(Ma, Ms).

%------------------------------------------------------------------------------

% jpl_class_to_field_array(+Class, -FieldArray) :-

jpl_class_to_field_array(Cx, Fa) :-
	jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
	jGetMethodID(
	CC,
	getFields,
	method([],array(class([java,lang,reflect],['Field']))),
	MID
	),                                                  % cacheable?
	jCallObjectMethod(Cx, MID, [], [], Fa).

%------------------------------------------------------------------------------

% jpl_class_to_fields(+Class, -Fields) :-
%   do this in Java (ditto for methods)?

jpl_class_to_fields(C, Fs) :-
	jpl_class_to_field_array(C, Fa),
	jpl_object_array_to_list(Fa, Fs).

%------------------------------------------------------------------------------

% jpl_class_to_method_array(+Class, -MethodArray) :-
%   migrate into foreign code for efficiency?

jpl_class_to_method_array(Cx, Ma) :-
	jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
	jGetMethodID(
	CC,
	getMethods,
	method([],array(class([java,lang,reflect],['Method']))),
	MID
	),                                                  % cacheable?
	jCallObjectMethod(Cx, MID, [], [], Ma).

%------------------------------------------------------------------------------

% jpl_class_to_methods(+Class, -Methods) :-
%   also used for constructors
%   do this in Java (ditto for fields)?

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

jpl_constructor_to_parameter_types(X, Tfps) :-
	jpl_classname_to_class('java.lang.reflect.Constructor', Cx),   % cached?
	jpl_method_to_parameter_types_1(X, Cx, Tfps).

%------------------------------------------------------------------------------

% jpl_constructor_to_return_type(+Method, -Type) :-
%   it is a JNI convention that, for the purposes of retrieving a MethodID,
%   a constructor has a return type of 'void'

jpl_constructor_to_return_type(_X, void).

%------------------------------------------------------------------------------

% jpl_field_spec(+Type, -Index, -Name, -Modifiers, -MID, -FieldType) :-
%   I'm unsure whether arrays have fields, but if they do, this will handle them correctly

jpl_field_spec(T, I, N, Mods, MID, Tf) :-
	(   jpl_field_spec_is_cached(T)
	->  jpl_field_spec_cache(T, I, N, Mods, MID, Tf)
	;   jpl_type_to_class(T, C),
	jpl_class_to_fields(C, Fs),
	(   T = array(_BaseType)    % regardless of base type...
	->  Tci = array(_)          % ...the "cache index" type is this
	;   Tci = T
	),
	jpl_field_spec_1(C, Tci, Fs),
	jpl_assert(jpl_field_spec_is_cached(Tci)),
	jpl_field_spec_cache(Tci, I, N, Mods, MID, Tf)
	).

%------------------------------------------------------------------------------

jpl_field_spec_1(C, Tci, Fs) :-
	(   nth1(I, Fs, F),
	jpl_field_to_name(F, N),
	jpl_field_to_modifiers(F, Mods),
	jpl_field_to_type(F, Tf),
	(   member(static, Mods)
	->  jGetStaticFieldID(C, N, Tf, MID)
	;   jGetFieldID(C, N, Tf, MID)
	),
	jpl_assert(jpl_field_spec_cache(Tci,I,N,Mods,MID,Tf)),
	fail
	;   true
	).

%------------------------------------------------------------------------------

:- dynamic jpl_field_spec_cache/6.      % document this...

%------------------------------------------------------------------------------

:- dynamic jpl_field_spec_is_cached/1.  % document this...

%------------------------------------------------------------------------------

%type   jpl_field_to_modifiers(object, ordset(modifier))

% jpl_field_to_modifiers(+Field, -Modifiers) :-

jpl_field_to_modifiers(F, Ms) :-
	jpl_classname_to_class('java.lang.reflect.Field', Cf),
	jpl_method_to_modifiers_1(F, Cf, Ms).

%------------------------------------------------------------------------------

% jpl_field_to_name(+Field, -Name) :-

jpl_field_to_name(F, N) :-
	jpl_classname_to_class('java.lang.reflect.Field', Cf),
	jpl_member_to_name_1(F, Cf, N).

%------------------------------------------------------------------------------

%type   jpl_field_to_type(object, type)

% jpl_field_to_type(+Field, -Type) :-

jpl_field_to_type(F, Tf) :-
	jpl_classname_to_class('java.lang.reflect.Field', Cf),
	jGetMethodID(Cf, getType, method([],class([java,lang],['Class'])), MID),
	jCallObjectMethod(F, MID, [], [], Cr),
	jpl_class_to_type(Cr, Tf).

%------------------------------------------------------------------------------

%type   jpl_method_spec(type, integer, name, arity, ordset(modifier), method_id, type, list(type))

% jpl_method_spec(+Type, -Index, -Name, -Arity, -Modifiers, -MID, -ReturnType, -ParameterTypes) :-
%   generates pertinent details of all accessible methods of Type (class/2 or array/1),
%   populating or using the cache as appropriate

jpl_method_spec(T, I, N, A, Mods, MID, Tr, Tfps) :-
	(   jpl_method_spec_is_cached(T)
	->  jpl_method_spec_cache(T, I, N, A, Mods, MID, Tr, Tfps)
	;   jpl_type_to_class(T, C),
	jpl_class_to_constructors(C, Xs),
	jpl_class_to_methods(C, Ms),
	(   T = array(_BaseType)    % regardless of base type...
	->  Tci = array(_)          % ...the "cache index" type is this
	;   Tci = T
	),
	jpl_method_spec_1(C, Tci, Xs, Ms),
	jpl_assert(jpl_method_spec_is_cached(Tci)),
	jpl_method_spec_cache(Tci, I, N, A, Mods, MID, Tr, Tfps)
	).

%------------------------------------------------------------------------------

%type   jpl_method_spec_1(class, partial_type, list(method), list(method))

% jpl_method_spec_1(+ClassObject, +CacheIndexType, +Constructors, +Methods) :-
%   if the original type is e.g. array(byte) then CacheIndexType is array(_) else it is that type;

jpl_method_spec_1(C, Tci, Xs, Ms) :-
	(   (   nth1(I, Xs, X),     % generate constructors, numbered from 1
	    jpl_constructor_to_name(X, N),
	    jpl_constructor_to_modifiers(X, Mods),
	    jpl_constructor_to_return_type(X, Tr),
	    jpl_constructor_to_parameter_types(X, Tfps)
	;   length(Xs, J0),
	    nth1(J, Ms, M),     % generate members, continuing numbering
	    I is J0+J,
	    jpl_method_to_name(M, N),
	    jpl_method_to_modifiers(M, Mods),
	    jpl_method_to_return_type(M, Tr),
	    jpl_method_to_parameter_types(M, Tfps)
	),
	length(Tfps, A), % arity
	(   member(static, Mods)
	->  jGetStaticMethodID(C, N, method(Tfps,Tr), MID)
	;   jGetMethodID(C, N, method(Tfps,Tr), MID)
	),
	jpl_assert(jpl_method_spec_cache(Tci,I,N,A,Mods,MID,Tr,Tfps)),
	fail
	;   true
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

%type   jpl_method_to_modifiers_1(object, object, ordset(modifier))

% jpl_method_to_modifiers_1(+Method, +ConstructorClass, -ModifierSet) :-

jpl_method_to_modifiers_1(XM, Cxm, Ms) :-
	jGetMethodID(Cxm, getModifiers, method([],int), MID),
	jCallIntMethod(XM, MID, [], [], I),
	jpl_modifier_int_to_modifiers(I, Ms).

%------------------------------------------------------------------------------

% jpl_method_to_name(+Method, -Name) :-

jpl_method_to_name(M, N) :-
	jpl_classname_to_class('java.lang.reflect.Method', CM),
	jpl_member_to_name_1(M, CM, N).

%------------------------------------------------------------------------------

jpl_member_to_name_1(M, CM, N) :-
	jGetMethodID(CM, getName, method([],class([java,lang],['String'])), MID),
	jCallObjectMethod(M, MID, [], [], N).

%------------------------------------------------------------------------------

% jpl_method_to_parameter_types(+Method, -Types) :-

jpl_method_to_parameter_types(M, Tfps) :-
	jpl_classname_to_class('java.lang.reflect.Method', Cm),
	jpl_method_to_parameter_types_1(M, Cm, Tfps).

%------------------------------------------------------------------------------

% jpl_method_to_parameter_types_1(+XM, +Cxm, -Tfps) :-
%   XM is (a JPL ref to) an instance of java.lang.reflect.[Constructor|Method]

jpl_method_to_parameter_types_1(XM, Cxm, Tfps) :-
	jGetMethodID(Cxm, getParameterTypes, method([],array(class([java,lang],['Class']))), MID),
	jCallObjectMethod(XM, MID, [], [], Atp),
	jpl_object_array_to_list(Atp, Ctps),
	jpl_classes_to_types(Ctps, Tfps).

%------------------------------------------------------------------------------

% jpl_method_to_return_type(+Method, -Type) :-

jpl_method_to_return_type(M, Tr) :-
	jpl_classname_to_class('java.lang.reflect.Method', Cm),
	jGetMethodID(Cm, getReturnType, method([],class([java,lang],['Class'])), MID),
	jCallObjectMethod(M, MID, [], [], Cr),
	jpl_class_to_type(Cr, Tr).

%------------------------------------------------------------------------------

jpl_modifier_bit(public,        0x001).
jpl_modifier_bit(private,       0x002).
jpl_modifier_bit(protected,     0x004).
jpl_modifier_bit(static,        0x008).
jpl_modifier_bit(final,         0x010).
jpl_modifier_bit(synchronized,  0x020).
jpl_modifier_bit(volatile,      0x040).
jpl_modifier_bit(transient,     0x080).
jpl_modifier_bit(native,        0x100).
jpl_modifier_bit(interface,     0x200).
jpl_modifier_bit(abstract,      0x400).

%------------------------------------------------------------------------------

%type   jpl_modifier_int_to_modifiers(integer, ordset(modifier))

% jpl_modifier_int_to_modifiers(+Int, -ModifierSet) :-
%   ModifierSet is an ordered (hence canonical) list,
%   possibly empty (although I suspect never in practice?),
%   of modifier atoms, e.g. [public,static]

jpl_modifier_int_to_modifiers(I, Ms) :-
	setof(
	M,                                  %  should use e.g. set_of_all/3
	B^(jpl_modifier_bit(M, B),
	    (B /\ I) =\= 0
	),
	Ms
	).

%------------------------------------------------------------------------------

% jpl_servlet_byref(+Config, +Request, +Response) :-
%   this serves the "byref" servlet demo,
%   exemplifying one tactic for implementing a servlet in Prolog
%   by accepting the Request and Response objects as JPL references
%   and accessing their members via JPL as required;
%   see also jpl_servlet_byval/3

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
	(   member(ContextInitParameterName, ContextInitParameterNames),
	jpl_call(Context, getInitParameter, [ContextInitParameterName], ContextInitParameter),
	concat_atom(['\t\tContext.InitParameter[',ContextInitParameterName,'] = ',ContextInitParameter], ContextInitParameterMsg),
	jpl_call(W, println, [ContextInitParameterMsg], _),
	fail
	;   true
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
	(   ServletName == @(null)
	->  ServletNameAtom = null
	;   ServletNameAtom = ServletName
	),
	concat_atom(['\tConfig.ServletName = ',ServletNameAtom], ServletNameMsg),
	jpl_call(W, println, [ServletNameMsg], _),

	jpl_call(Config, getInitParameterNames, [], ConfigInitParameterNameEnum),
	jpl_enumeration_to_list(ConfigInitParameterNameEnum, ConfigInitParameterNames),
	length(ConfigInitParameterNames, NConfigInitParameterNames),
	concat_atom(['\tConfig.InitParameters = ',NConfigInitParameterNames], NConfigInitParameterNamesMsg),
	jpl_call(W, println, [NConfigInitParameterNamesMsg], _),
	(   member(ConfigInitParameterName, ConfigInitParameterNames),
	jpl_call(Config, getInitParameter, [ConfigInitParameterName], ConfigInitParameter),
	concat_atom(['\t\tConfig.InitParameter[',ConfigInitParameterName,'] = ',ConfigInitParameter], ConfigInitParameterMsg),
	jpl_call(W, println, [ConfigInitParameterMsg], _),
	fail
	;   true
	),

	jpl_call(W, println, ['\nrequest stuff:'], _),

	jpl_call(Request, getAttributeNames, [], AttributeNameEnum),
	jpl_enumeration_to_list(AttributeNameEnum, AttributeNames),
	length(AttributeNames, NAttributeNames),
	concat_atom(['\tRequest.Attributes = ',NAttributeNames], NAttributeNamesMsg),
	jpl_call(W, println, [NAttributeNamesMsg], _),
	(   member(AttributeName, AttributeNames),
	jpl_call(Request, getAttribute, [AttributeName], Attribute),
	jpl_call(Attribute, toString, [], AttributeString),
	concat_atom(['\t\tRequest.Attribute[',AttributeName,'] = ',AttributeString], AttributeMsg),
	jpl_call(W, println, [AttributeMsg], _),
	fail
	;   true
	),

	jpl_call(Request, getCharacterEncoding, [], CharacterEncoding),
	(   CharacterEncoding == @(null)
	->  CharacterEncodingAtom = ''
	;   CharacterEncodingAtom = CharacterEncoding
	),
	concat_atom(['\tRequest.CharacterEncoding',' = ',CharacterEncodingAtom], CharacterEncodingMsg),
	jpl_call(W, println, [CharacterEncodingMsg], _),

	jpl_call(Request, getContentLength, [], ContentLength),
	concat_atom(['\tRequest.ContentLength',' = ',ContentLength], ContentLengthMsg),
	jpl_call(W, println, [ContentLengthMsg], _),

	jpl_call(Request, getContentType, [], ContentType),
	(   ContentType == @(null)
	->  ContentTypeAtom = ''
	;   ContentTypeAtom = ContentType
	),
	concat_atom(['\tRequest.ContentType',' = ',ContentTypeAtom], ContentTypeMsg),
	jpl_call(W, println, [ContentTypeMsg], _),

	jpl_call(Request, getParameterNames, [], ParameterNameEnum),
	jpl_enumeration_to_list(ParameterNameEnum, ParameterNames),
	length(ParameterNames, NParameterNames),
	concat_atom(['\tRequest.Parameters = ',NParameterNames], NParameterNamesMsg),
	jpl_call(W, println, [NParameterNamesMsg], _),
	(   member(ParameterName, ParameterNames),
	jpl_call(Request, getParameter, [ParameterName], Parameter),
	concat_atom(['\t\tRequest.Parameter[',ParameterName,'] = ',Parameter], ParameterMsg),
	jpl_call(W, println, [ParameterMsg], _),
	fail
	;   true
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
	(   AuthType == @(null)
	->  AuthTypeAtom = ''
	;   AuthTypeAtom = AuthType
	),
	concat_atom(['\tRequest.AuthType',' = ',AuthTypeAtom], AuthTypeMsg),
	jpl_call(W, println, [AuthTypeMsg], _),

	jpl_call(Request, getContextPath, [], ContextPath),
	(   ContextPath == @(null)
	->  ContextPathAtom = ''
	;   ContextPathAtom = ContextPath
	),
	concat_atom(['\tRequest.ContextPath',' = ',ContextPathAtom], ContextPathMsg),
	jpl_call(W, println, [ContextPathMsg], _),

	jpl_call(Request, getCookies, [], CookieArray),
	(   CookieArray == @(null)
	->  Cookies = []
	;   jpl_array_to_list(CookieArray, Cookies)
	),
	length(Cookies, NCookies),
	concat_atom(['\tRequest.Cookies',' = ',NCookies], NCookiesMsg),
	jpl_call(W, println, [NCookiesMsg], _),
	(   nth0(NCookie, Cookies, Cookie),
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
	;   true
	),

	jpl_call(W, println, ['</pre></body></html>'], _),

	true.

%------------------------------------------------------------------------------

% jpl_servlet_byval(+MultiMap, -ContentType, -BodyAtom) :-
%   this exemplifies an alternative (to jpl_servlet_byref) tactic
%   for implementing a servlet in Prolog;
%   most Request fields are extracted in Java before this is called,
%   and passed in as a multimap (a map, some of whose values are maps)

jpl_servlet_byval(MM, CT, Ba) :-
	CT = 'text/html',
	multimap_to_atom(MM, MMa),
	concat_atom(['<html><head></head><body>',
		     '<h2>jpl_servlet_byval/3 says:</h2><pre>',
		     MMa,
		     '</pre></body></html>'
		    ], Ba).

%------------------------------------------------------------------------------

%type   jpl_cache_type_of_ref(jpl_type, ref)

% jpl_cache_type_of_ref(+Type, +Ref) :-
%   Type must be a proper (concrete) JPL type;
%   Ref must be a proper JPL reference (not void);
%   Type is memoed (if policy so dictates) as the type of the referenced object (unless it's null)
%   by iref (so as not to disable atom-based GC)
%   NB obsolete lemmas must be watched-out-for and removed

jpl_cache_type_of_ref(T, @(Tag)) :-
	(   jpl_assert_policy( jpl_iref_type_cache(_,_), no)
	->  true
	;   \+ ground(T)                            % shouldn't happen (implementation error)
	->  write('[jpl_cache_type_of_ref/2: arg 1 is not ground]'), nl,    % oughta throw an exception
	fail
	;   \+ atom(Tag)                            % shouldn't happen (implementation error)
	->  write('[jpl_cache_type_of_ref/2: arg 2 is not an atomic-tag ref]'), nl, % oughta throw an exception
	fail
	;   Tag == null                             % a null ref? (this is valid)
	->  true                                    % silently ignore it
	;   jni_tag_to_iref(Tag, Iref)
	->  (   jpl_iref_type_cache(Iref, TC)       % we expect TC == T
	->  (   T == TC
	    ->  true
	    ; % write('[JPL: found obsolete tag-type lemma...]'), nl,   % or keep statistics? (why?)
		retractall(jpl_iref_type_cache(Iref,_)),
		jpl_assert(jpl_iref_type_cache(Iref,T))
	    )
	;   jpl_assert(jpl_iref_type_cache(Iref,T))
	)
	;   write('[jpl_cache_type_of_ref/2: jni_tagatom_to_iref(Tag,_) failed]'), nl,  % oughta throw an exception
	fail
	).

%------------------------------------------------------------------------------

% jpl_class_tag_type_cache(-Tag, -ClassType) :-
%   Tag is the tag part of an @(Tag) reference
%   to a JVM instance of java.lang.Class
%   which denotes ClassType;
%   we index on Tag rather than on Iref so as to keep these objects around
%   even after an atom garbage collection
%   (if needed once, they are likely to be needed again)

:- dynamic jpl_class_tag_type_cache/2.

%------------------------------------------------------------------------------

% jpl_class_to_ancestor_classes(+Class, -AncestorClasses) :-
%   AncestorClasses will be a list of (JPL references to) instances of java.lang.Class
%   denoting the "implements" lineage (?), nearest first
%   (the first member denotes the class which Class directly implements,
%   the next (if any) denotes the class which *that* class implements,
%   and so on to java.lang.Object)

jpl_class_to_ancestor_classes(C, Cas) :-
	(   jpl_class_to_super_class(C, Ca)
	->  Cas = [Ca|Cas2],
	jpl_class_to_ancestor_classes(Ca, Cas2)
	;   Cas = []
	).

%------------------------------------------------------------------------------

% jpl_class_to_classname(+Class, -ClassName) :-
%   Class is a reference to a class object;
%   ClassName is its canonical (?) source-syntax (dotted) name,
%   e.g. 'java.util.Date'
%   not used outside jni_junk and jpl_test (is this (still) true?);
%   oughta use the available caches (but their indexing doesn't suit)

jpl_class_to_classname(C, CN) :-
	jpl_call(C, getName, [], CN).

%------------------------------------------------------------------------------

% jpl_class_to_raw_classname(+Class, -ClassName) :-
%   hmm, I forget exactly what a "raw" classname is...

jpl_class_to_raw_classname(Cobj, CN) :-
	jpl_classname_to_class('java.lang.Class', CC),      % cached?
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
	Cx \== @(null),         % as returned when C is java.lang.Object, i.e. no superclass
	jpl_cache_type_of_ref(class([java,lang],['Class']), Cx).    

%------------------------------------------------------------------------------

% jpl_class_to_type(+ClassObject, -Type) :-
%   ClassObject is a reference to a class object of Type
%   NB should ensure that, if not found in cache, then cache is updated;
%   intriguingly (?), getParameterTypes returns class objects with names
%   'boolean', 'byte' etc. and even 'void' (?!)

jpl_class_to_type(@(Tag), Type) :-
	(   jpl_class_tag_type_cache(Tag, Tx)
	->  true
	;   jpl_class_to_raw_classname_chars(@(Tag), Cs),   % uncached
	jpl_classname_chars_to_type(Cs, Tr),
	jpl_type_to_canonical_type(Tr, Tx),             % map e.g. class([],[byte]) -> byte
	jpl_assert(jpl_class_tag_type_cache(Tag,Tx))
	->  true    % the elseif goal should be determinate, but just in case...
	),
	Type = Tx.

%------------------------------------------------------------------------------

jpl_classes_to_types([], []).

jpl_classes_to_types([C|Cs], [T|Ts]) :-
	jpl_class_to_type(C, T),
	jpl_classes_to_types(Cs, Ts).

%------------------------------------------------------------------------------

jpl_classname_chars_to_type(Cs, Type) :-
	(   phrase(jpl_type_classname_1(Type), Cs)
	->  true
	).

%------------------------------------------------------------------------------

% jpl_classname_to_class(+ClassName, -Class) :-
%   ClassName unambiguously represents a class,
%   e.g. 'java.lang.String'
%   Class is a (canonical) reference to the corresponding class object;
%   uses caches where the class is already encountered

jpl_classname_to_class(N, C) :-
	jpl_classname_to_type(N, T),    % cached
	jpl_type_to_class(T, C).        % cached

%------------------------------------------------------------------------------

% jpl_classname_to_type(+Classname, -Type) :-
%   Classname is a source-syntax (dotted) class name,
%   e.g. 'java.util.Date', '[java.util.Date' or '[L'
%   Type is its corresponding JPL type structure,
%   e.g. class([java,util],['Date']), array(class([java,util],['Date'])), array(long)
%
%thinks
%   by "classname" do I mean "typename"?
%   should this throw an exception for unbound CN? is this public API?

jpl_classname_to_type(CN, T) :-
	(   jpl_classname_type_cache(CN, Tx)
	->  Tx = T
	;   atom_codes(CN, CsCN),
	phrase(jpl_type_classname_1(T), CsCN)
	->  jpl_assert(jpl_classname_type_cache(CN,T)),
	true
	).

%------------------------------------------------------------------------------

% jpl_classname_type_cache( -Classname, -Type) :-
%   Classname is the atomic name of Type;
%   NB may denote a class which cannot be found

:- dynamic jpl_classname_type_cache/2.

%------------------------------------------------------------------------------

% jpl_datum_to_type(+Datum, -Type) :-
%   Datum must be a proper JPL representation
%   of an instance of one (or more) Java types;
%   Type is the unique most specialised type of which Datum denotes an instance;
%   N.B. 3 is an instance of byte, char, short, int and long,
%   of which byte and char are the joint, overlapping most specialised types,
%   so this relates 3 to the pseudo subtype 'char_byte';
%   see jpl_type_to_preferred_concrete_type/2 for converting inferred types
%   to instantiable types

jpl_datum_to_type(D, T) :-
	(   jpl_value_to_type(D, T)
	->  true
	;   jpl_ref_to_type(D, T)
	->  true
	;   nonvar( D),
	D = {Term}
	->  (   cyclic_term(Term)
	->  throw(error(type_error(acyclic,Term),
			context(jpl_datum_to_type/2,'must be acyclic')))
	;   atom( Term)
	->  T = class([jpl],['Atom'])
	;   integer( Term)
	->  T = class([jpl],['Integer'])
	;   float( Term)
	->  T = class([jpl],['Float'])
	;   var( Term)
	->  T = class([jpl],['Variable'])
	;   T = class([jpl],['Compound'])
	)
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
%   X is (by unification) the proper JPL datum which represents the Java boolean value 'false'
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
%   X is the proper JPL datum which represents the Java boolean value 'false';
%   whatever, no further instantiation of X occurs

jpl_is_false(X) :-
	X == @(false).

%------------------------------------------------------------------------------

% jpl_is_fieldID(?X) :-
%   X is a proper JPL field ID structure (jfieldID/1);
%   applications should not be messing with these (?);
%   whatever, no further instantiation of X occurs

jpl_is_fieldID(jfieldID(X)) :-      % NB a var arg may get bound...
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
	jpl_is_ref(X),      % (syntactically, at least...)
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
	atom(Y),        % presumably a (garbage-collectable) tag
	Y \== void,     % not a ref
	Y \== false,    % not a ref
	Y \== true.     % not a ref

%------------------------------------------------------------------------------

% jpl_is_true(?X) :-
%   X is a proper JPL datum, representing the Java boolean value 'true';
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
	(   append(_, [Tx|Ts2], Ts)
	->  [Tx|Ts2] = Ts0
	;   jpl_type_to_super_type(Tx, Tx2)
	->  jpl_lineage_types_type_to_common_lineage_types(Ts, Tx2, Ts0)
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
	(   I == N
	->  Xs = []
	;   jGetObjectArrayElement(A, I, X),
	Xs = [X|Xs2],
	J is I+1,
	jpl_object_array_to_list_1(A, J, N, Xs2)
	).

%------------------------------------------------------------------------------

% jpl_object_to_class(+Object, -Class) :-
%   Object must be a valid object (should this throw an exception otherwise?);
%   Class is a (canonical) reference to the (canonical) class object
%   which represents the class of Object;
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
	(   (   T = class(_,_)
	;   T = array(_)
	)
	->  jpl_type_to_class(T, C),
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
	jni_fetch_buffer_value(Bp, I, Vc, Xc),
	Ix is I+1,
	(   Ix < Size
	->  jpl_primitive_buffer_to_array(T, Xc, Bp, Ix, Size, Vcs)
	;   Vcs = []
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
jpl_primitive_type_default_value(char,    0).
jpl_primitive_type_default_value(byte,    0).
jpl_primitive_type_default_value(short,   0).
jpl_primitive_type_default_value(int,     0).
jpl_primitive_type_default_value(long,    0).
jpl_primitive_type_default_value(float,   0.0).
jpl_primitive_type_default_value(double,  0.0).

%------------------------------------------------------------------------------

jpl_primitive_type_super_type(T, Tx) :-
	(   jpl_type_fits_type_direct_prim(T, Tx)
	;   jpl_type_fits_type_direct_xtra(T, Tx)
	).

%------------------------------------------------------------------------------

% jpl_primitive_type_term_to_value(+Type, +Term, -Val) :-
%   Term, after widening iff appropriate, represents an instance of Type;
%   Val is the instance of Type which it represents (often the same thing);
%   currently used only by jpl_new_1 when creating an "instance"
%   of a primitive type (which may be misguided completism - you can't
%   do that in Java)

jpl_primitive_type_term_to_value(Type, Term, Val) :-
	(   jpl_primitive_type_term_to_value_1(Type, Term, Val)
	->  true
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
	I =< 65535.         %  (2**16)-1.

jpl_primitive_type_term_to_value_1(byte, I, I) :-
	integer(I),
	I >= 128,           % -(2**7)
	I =< 127.           %  (2**7)-1

jpl_primitive_type_term_to_value_1(short, I, I) :-
	integer(I),
	I >= -32768,        % -(2**15)
	I =<  32767.        %  (2**15)-1

jpl_primitive_type_term_to_value_1(int, I, I) :-
	integer(I),
	I >= -2147483648,   % -(2**31)
	I =<  2147483647.   %  (2**31)-1

jpl_primitive_type_term_to_value_1(long, I, I) :-
	integer(I),
	I >= -9223372036854775808,  % -(2**63)
	I =<  9223372036854775807.  %  (2**63)-1

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
	(   jpl_primitive_type_super_type(T, Ta)
	->  Ts = [Ta|Tas],
	jpl_primitive_type_to_ancestor_types(Ta, Tas)
	;   Ts = []
	).

%------------------------------------------------------------------------------

jpl_primitive_type_to_super_type(T, Tx) :-
	jpl_primitive_type_super_type(T, Tx).

%------------------------------------------------------------------------------

% jpl_ref_to_type(+Ref, -Type) :-
%   Ref must be a proper JPL reference (to an object, null or void);
%   Type is its type

jpl_ref_to_type(@(X), T) :-
	(   X == null
	->  T = null
	;   X == void
	->  T = void
	;   jpl_tag_to_type(X, T)
	).

%------------------------------------------------------------------------------

% jpl_tag_to_type(+Tag, -Type) :-
%   Tag must be an (atomic) object tag;
%   Type is its type (either from the cache or by reflection);

jpl_tag_to_type(Tag, Type) :-
	jni_tag_to_iref(Tag, Iref),
	(   jpl_iref_type_cache(Iref, T)
	->  true                                % T is Tag's type
	;   jpl_object_to_class(@(Tag), Cobj), % else get ref to class obj
	jpl_class_to_type(Cobj, T),         % get type of class it denotes
	jpl_assert(jpl_iref_type_cache(Iref,T))
	),
	Type = T.

%------------------------------------------------------------------------------

% jpl_true(-X) :-
%   X is (by unification) the proper JPL datum which represents the Java boolean value 'true';
%cf jpl_is_true/1

jpl_true(@(true)).

%------------------------------------------------------------------------------

% jpl_type_fits_type(+TypeX, +TypeY) :-
%   TypeX and TypeY must each be proper JPL types;
%   this succeeds iff TypeX is assignable to TypeY

jpl_type_fits_type(Tx, Ty) :-
	(   jpl_type_fits_type_1(Tx, Ty)
	->  true
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

% jpl_type_fits_type_direct_xtra(-PseudoType, -ConcreteType) :-
%   this predicate defines the direct subtype-supertype relationships
%   which involve the intersection pseudo types char_int, char_short and char_byte

jpl_type_fits_type_direct_xtra(char_int,   int).    % char_int is a direct subtype of int
jpl_type_fits_type_direct_xtra(char_int,   char).   % etc.
jpl_type_fits_type_direct_xtra(char_short, short).
jpl_type_fits_type_direct_xtra(char_short, char).
jpl_type_fits_type_direct_xtra(char_byte,  byte).
jpl_type_fits_type_direct_xtra(char_byte,  char).

jpl_type_fits_type_direct_xtra(overlong,   float).  % 6/Oct/2006 experiment

%------------------------------------------------------------------------------

% jpl_type_fits_type_xprim(-Tp, -T) :-
%   indeterminate;
%   serves only jpl_type_fits_type_1/2

jpl_type_fits_type_xprim(Tp, T) :-
	jpl_type_fits_type_direct_xprim(Tp, Tq),
	(   Tq = T
	;   jpl_type_fits_type_xprim(Tq, T)
	).

%------------------------------------------------------------------------------

% jpl_type_to_ancestor_types(+T, -Tas) :-
%   this does not accommodate the assignability of null,
%   but that's OK (?) since "type assignability" and "type ancestry" are not equivalent

jpl_type_to_ancestor_types(T, Tas) :-
	(   (   T = class(_,_)
	;   T = array(_)
	)
	->  jpl_type_to_class(T, C),
	jpl_class_to_ancestor_classes(C, Cas),
	jpl_classes_to_types(Cas, Tas)
	;   jpl_primitive_type_to_ancestor_types(T, Tas)
	->  true
	).

%------------------------------------------------------------------------------

% jpl_type_to_canonical_type(+Type, -CanonicalType) :-
%   Type must be a type, not necessarily canonical;
%   CanonicalType will be equivalent and canonical

%eg jpl_type_to_canonical_type(class([],[byte]), byte)

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
%   incomplete types are now never cached (or otherwise passed around);
%   jFindClass throws an exception if FCN can't be found

%nb if this is public API maybe oughta restore the ground(T) check and throw exception

jpl_type_to_class(T, @(Tag)) :-
  % ground(T),  % 9/Nov/2004 removed this spurious (?) check
	(   jpl_class_tag_type_cache(ClassTag,T)
	->  Tag = ClassTag
	;   (   jpl_type_to_findclassname(T, FCN)   % peculiar syntax for FindClass()
	->  jFindClass(FCN, @(ClassTag)),       % which caches type of @ClassTag
	  % jpl_cache_type_of_ref(T, @(ClassTag))
	    jpl_cache_type_of_ref(class([java,lang],['Class']), @(ClassTag))    % 9/Nov/2004 bugfix (?)
	),
	jpl_assert(jpl_class_tag_type_cache(ClassTag,T))
	),
	Tag = ClassTag.

%------------------------------------------------------------------------------

% jpl_type_to_nicename(+Type, -NiceName) :-
%   Type, which is a class or array type (not sure about the others...),
%   is denoted by ClassName in dotted syntax

%nb is this used? is "nicename" well defined and necessary?
%nb this could use caching if indexing were amenable

%eg jpl_type_to_nicename(class([java,util],['Date']), 'java.util.Date')
%eg jpl_type_to_nicename(boolean, boolean)

%cf jpl_type_to_classname/2

jpl_type_to_nicename(T, NN) :-
	(   jpl_primitive_type( T)
	->  NN = T
	;   (   phrase(jpl_type_classname_1(T), Cs)
	->  atom_codes(CNx, Cs),                                % green commit to first solution
	    NN = CNx
	)
	).

%------------------------------------------------------------------------------

% jpl_type_to_classname(+Type, -ClassName) :-
%   Type, which is a class or array type (not sure about the others...),
%   is denoted by ClassName in dotted syntax

%eg jpl_type_to_classname(class([java,util],['Date']), 'java.util.Date')

%cf jpl_type_to_nicename/2

jpl_type_to_classname(T, CN) :-
	(   phrase(jpl_type_classname_1(T), Cs)
	->  atom_codes(CNx, Cs),                                % green commit to first solution
	CN = CNx
	).

%------------------------------------------------------------------------------

% jpl_type_to_descriptor(+Type, -Descriptor) :-
%   Type (denoting any Java type)
%   (can also be a JPL method/2 structure (?!))
%   is represented by Descriptor (JVM internal syntax)
%   I'd cache this, but I'd prefer more efficient indexing on types (hashed?)

jpl_type_to_descriptor(T, D) :-
	(   phrase(jpl_type_descriptor_1(T), Cs)
	->  atom_codes(Dx, Cs),
	D = Dx
	).

%------------------------------------------------------------------------------

% jpl_type_to_findclassname(+Type, -FindClassName) :-
%   FindClassName denotes Type (class or array only)
%   in the syntax required peculiarly by FindClass()

jpl_type_to_findclassname(T, FCN) :-
	(   phrase(jpl_type_findclassname(T), Cs)
	->  atom_codes(FCNx, Cs),
	FCN = FCNx
	).

%------------------------------------------------------------------------------

% jpl_type_to_super_type(+Type, -SuperType) :-
%   Type oughta be a proper JPL type;
%   SuperType is the (at most one) type which it directly implements (if it's a class);
%   if Type denotes a class, this works only if that class can be found;
%   if Type = array(Type) then I dunno what happens...

jpl_type_to_super_type(T, Tx) :-
	(   jpl_object_type_to_super_type(T, Tx)
	->  true
	;   jpl_primitive_type_to_super_type(T, Tx)
	->  true
	).

%------------------------------------------------------------------------------

% jpl_type_to_preferred_concrete_type( +Type, -ConcreteType) :-
%   Type must be a canonical JPL type,
%   possibly a pseudo (inferred) type such as char_int or array(char_byte);
%   ConcreteType is the preferred concrete (Java-instantiable) type;
%   introduced 16/Apr/2005 to fix bug whereby jpl_list_to_array([1,2,3],A) failed 
%   because the lists's inferred type of array(char_byte) is not Java-instantiable

jpl_type_to_preferred_concrete_type( T, Tc) :-
	(   jpl_type_to_preferred_concrete_type_1( T, TcX)
	->  Tc = TcX
	).

%------------------------------------------------------------------------------

jpl_type_to_preferred_concrete_type_1( char_int, int).

jpl_type_to_preferred_concrete_type_1( char_short, short).

jpl_type_to_preferred_concrete_type_1( char_byte, byte).

jpl_type_to_preferred_concrete_type_1( array(T), array(Tc)) :-
	jpl_type_to_preferred_concrete_type_1( T, Tc).

jpl_type_to_preferred_concrete_type_1( T, T).

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

% jpl_types_fit_types(+Types1, +Types2) :-
%   each member type of Types1 "fits" the respective member type of Types2

jpl_types_fit_types([], []).

jpl_types_fit_types([T1|T1s], [T2|T2s]) :-
	jpl_type_fits_type(T1, T2),
	jpl_types_fit_types(T1s, T2s).

%------------------------------------------------------------------------------

% jpl_value_to_type(+Value, -Type) :-
%   Value must be a proper JPL datum other than a ref
%   i.e. primitive, String or void;
%   it is of (unique most specific) Type,
%   which may be one of the pseudo types char_byte, char_short or char_int

jpl_value_to_type(V, T) :-
	ground(V),                          % critically assumed by jpl_value_to_type_1/2
	(   jpl_value_to_type_1(V, Tv)      % 2nd arg must be unbound
	->  T = Tv
	).

%------------------------------------------------------------------------------

% jpl_value_to_type_1(+Value, -Type) :-
%   Type is the unique most specific JPL type of which Value represents an instance;
%   called solely by jpl_value_to_type/2, which commits to first solution;
%
%   NB some integer values are of JPL-peculiar uniquely most specific subtypes,
%   i.e. char_byte, char_short, char_int
%   but all are understood by JPL's internal utilities which call this proc
%
%   NB we regard float as subtype of double
%
%   NB objects and refs always have straightforward types

jpl_value_to_type_1(@(false), boolean).

jpl_value_to_type_1(@(true), boolean).

jpl_value_to_type_1(A, class([java,lang],['String'])) :-   % yes it's a "value"
	atom(A).

jpl_value_to_type_1(I, T) :-
	integer(I),
	(   I >= 0  ->  (   I < 2**7    ->  T = char_byte
		    ;   I < 2**15   ->  T = char_short
		    ;   I < 2**16   ->  T = char_int
		    ;   I < 2**31   ->  T = int
		    ;   I < 2**63   ->  T = long
				    ;   T = overlong
		    )
	;   I >= -(2**7)    ->  T = byte
	;   I >= -(2**15)   ->  T = short
	;   I >= -(2**31)   ->  T = int
	;   I >= -(2**63)   ->  T = long
			;   T = overlong 
	).

jpl_value_to_type_1(F, float) :-
	float(F).

%------------------------------------------------------------------------------

% jpl_void(-X) :-
%   X is (by unification) the proper JPL datum which represents the pseudo Java value 'void';
%   c.f. jpl_is_void/1

jpl_void(@(void)).

%------------------------------------------------------------------------------

%type   jpl_array_to_length(array, integer)

% jpl_array_to_length(+ArrayObject, -Length) :-
%   must validate ArrayObject before making the JNI call...

jpl_array_to_length(A, N) :-
	(   jpl_ref_to_type(A, array(_))    % can this be done cheaper e.g. in foreign code?
	->  jGetArrayLength(A, N)           % *must* be array, else undefined (crash?)
	).

%------------------------------------------------------------------------------

%type   jpl_array_to_list(array, list(datum))

% jpl_array_to_list(+Array, -Elements) :-

jpl_array_to_list(A, Es) :-
	jpl_array_to_length(A, Len),
	(   Len > 0
	->  LoBound is 0,
	HiBound is Len-1,
	jpl_get(A, LoBound-HiBound, Es)
	;   Es = []
	).

%------------------------------------------------------------------------------

%type   jpl_datums_to_array(list(datum), array)

% jpl_datums_to_array(+Ds, -A) :-
%   A will be a ref to a new JVM array,
%   whose base type is the most specific Java type
%   of which each member of Datums is (directly or indirectly) an instance;
%   NB this fails (without warning, currently) if:
%       Ds is an empty list (no base type can be inferred)
%       Ds contains a primitive value and an object or array ref (no common supertype)

jpl_datums_to_array(Ds, A) :-
	ground(Ds),
	jpl_datums_to_most_specific_common_ancestor_type(Ds, T),    % T may be pseudo e.g. char_byte
	jpl_type_to_preferred_concrete_type( T, Tc),    % bugfix added 16/Apr/2005
	jpl_new(array(Tc), Ds, A).

%------------------------------------------------------------------------------

%type   jpl_enumeration_element(object, datum)

% jpl_enumeration_element(+Enumeration, -Element) :-
%   generates each Element from the Enumeration;
%   if the element is a java.lang.String then Element will be an atom;
%   if the element is null then Element will (oughta) be null;
%   otherwise I reckon it has to be an object ref

jpl_enumeration_element(En, E) :-
	(   jpl_call(En, hasMoreElements, [], @(true))
	->  jpl_call(En, nextElement, [], Ex),
	(   E = Ex
	;   jpl_enumeration_element(En, E)
	)
	).

%------------------------------------------------------------------------------

%type   jpl_enumeration_to_list(object, list(datum))

% jpl_enumeration_to_list(+Enumeration, -Elements) :-

jpl_enumeration_to_list(EN, Es) :-
	(   jpl_call(EN, hasMoreElements, [], @(true))
	->  jpl_call(EN, nextElement, [], E),
	Es = [E|Es1],
	jpl_enumeration_to_list(EN, Es1)
	;   Es = []
	).

%------------------------------------------------------------------------------

%type   jpl_hashtable_pair(object, pair(datum,datum))

% jpl_hashtable_pair(+HashTable, -KeyValuePair) :-
%   generates Key-Value pairs from the given HashTable
%   NB String is converted to atom but Integer is presumably returned as an object ref
%   (i.e. as elsewhere, no auto unboxing);
%nb this is anachronistic (oughta use the Map interface?)

jpl_hashtable_pair(HT, K-V) :-
	jpl_call(HT, keys, [], Ek),
	jpl_enumeration_to_list(Ek, Ks),
	member(K, Ks),
	jpl_call(HT, get, [K], V).

%------------------------------------------------------------------------------

%type   jpl_iterator_element(object, datum)

% jpl_iterator_element(+Iterator, -Element) :-

jpl_iterator_element(I, E) :-
	(   jpl_call(I, hasNext, [], @(true))
	->  (   jpl_call(I, next, [], E)        % surely it's steadfast...
	;   jpl_iterator_element(I, E)
	)
	).

%------------------------------------------------------------------------------

%type   jpl_list_to_array(list(datum), array)

% jpl_list_to_array(+Datums, -Array) :-
%   Datums is a proper list of JPL datums (values or refs);
%   if they have a most specific common supertype,
%   Array is an array, of that base type,
%   whose respective elements are Datums

jpl_list_to_array(Ds, A) :-
	jpl_datums_to_array(Ds, A).

%------------------------------------------------------------------------------

%type   jpl_terms_to_array(list(term), array)

% jpl_terms_to_array(+Terms, -Array) :-
%   Terms is a proper list of arbitrary terms;
%   Array is an array of jpl.Term,
%   whose elements represent the respective members of the list

jpl_terms_to_array(Ts, A) :-
	jpl_terms_to_array_1(Ts, Ts2),
	jpl_new( array(class([jpl],['Term'])), Ts2, A).

%------------------------------------------------------------------------------

jpl_terms_to_array_1([], []).

jpl_terms_to_array_1([T|Ts], [{T}|Ts2]) :-
	jpl_terms_to_array_1(Ts, Ts2).

%------------------------------------------------------------------------------

%type   jpl_map_element(object, pair(datum,datum))

% jpl_map_element(+Map, -KeyValue) :-
%   Map must be an instance of any implementation of the java.util.Map interface;
%   this generates each Key-Value pair from the Map

jpl_map_element(M, K-V) :-
	jpl_call(M, entrySet, [], ES),
	jpl_set_element(ES, E),
	jpl_call(E, getKey, [], K),
	jpl_call(E, getValue, [], V).

%------------------------------------------------------------------------------

%type   jpl_set_element(object, datum)

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

multimap_to_atom(KVs, A) :-
	multimap_to_atom_1(KVs, "", Cz, []),
	flatten(Cz, Cs),
	atom_codes(A, Cs).

%------------------------------------------------------------------------------

multimap_to_atom_1([], _, Cs, Cs).
multimap_to_atom_1([K-V|KVs], T, Cs1, Cs0) :-
	atom_codes(K, CsK),
	Cs1 = [T,CsK," = "|Cs2],
	(   is_list(V)
	->  (   is_pairs(V)
	    ->  V = V2
	    ;   findall(N-Ve, nth1(N, V, Ve), V2)
	    ),
	    T2 = ["    ",T],
	    Cs2 = [10|Cs2a],
	    multimap_to_atom_1(V2, T2, Cs2a, Cs3)
	;   term_to_codes(V, CsV),
	    Cs2 = [CsV,10|Cs3]
	),
	multimap_to_atom_1(KVs, T, Cs3, Cs0).

%------------------------------------------------------------------------------

%%	term_to_codes(+Term, ?Codes)
%
%	unifies Codes with a printed representation of Term.
%	
%	@tbd Sort of quoting requirements and use format(codes(Codes),
%	...)

term_to_codes(Term, Codes) :-
	(   atom(Term)
	->  Term = A                % avoid superfluous quotes
	;   system:term_to_atom(Term, A)
	),
	atom_codes(A, Codes).

%------------------------------------------------------------------------------

		 /*******************************
		 *            MESSAGES          *
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
		 *             PATHS            *
		 *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(jar, swi(lib)).

%%	add_search_path(+Var, +Value) is det.
%
%	Add value to search-path Var.  Value is normally a directory.
%	
%	@param Value	Path to add in OS notation.

add_search_path(Path, Dir) :-
	(   getenv(Path, Old)
	->  (   current_prolog_flag(windows, true)
	    ->	Sep = (;)
	    ;	Sep = (:)
	    ),
	    concat_atom([Old, Sep, Dir], New),
	    setenv(Path, New)
	;   setenv(Path, Dir)
	).

%%	search_path_separator(-Sep:atom)
%
%	Separator  used  the  the  OS    in  =PATH=,  =LD_LIBRARY_PATH=,
%	=CLASSPATH=, etc.

search_path_separator((;)) :-
	current_prolog_flag(windows, true), !.
search_path_separator(:).

		 /*******************************
		 *         LOAD THE JVM         *
		 *******************************/

%%      check_java_environment
%       
%       Verify the Java environment.  Preferably   we  would create, but
%       most Unix systems do not   allow putenv("LD_LIBRARY_PATH=..." in
%       the current process. A suggesting found on  the net is to modify
%       LD_LIBRARY_PATH right at startup and  next execv() yourself, but
%       this doesn't work if we want to load Java on demand or if Prolog
%       itself is embedded in another application.
%       
%       So, after reading lots of pages on   the web, I decided checking
%       the environment and producing a sensible   error  message is the
%       best we can do.
%       
%       Please not that Java2 doesn't require   $CLASSPATH to be set, so
%       we do not check for that.

check_java_environment :-
	check_lib(java),
	check_lib(jvm).

check_lib(Name) :-
	check_shared_object(Name, File, EnvVar, Absolute),
	(   Absolute == (-)
	->  (   current_prolog_flag(windows, true)
	    ->  A = '%', Z = '%'
	    ;   A = '$', Z = ''
	    ),
	    format(string(Msg), 'Please add directory holding ~w to ~w~w~w',
		   [ File, A, EnvVar, Z ]),
	    throw(error(existence_error(library, Name),
			context(_, Msg)))
	;   true
	).

%%	check_shared_object(+Lib, -File, -EnvVar, -AbsFile) is semidet.
%
%	True if AbsFile is existing .so/.dll file for Lib.
%	
%	@param File	Full name of Lib (i.e. libjpl.so or jpl.dll)
%	@param EnvVar	Search-path for shared objects.

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


%%	library_search_path(-Dirs:list, -EnvVar) is det.
%
%	Dirs  is  the  list   of    directories   searched   for  shared
%	objects/DLLs. EnvVar is the variable in which the search path os
%	stored.

library_search_path(Path, EnvVar) :-
	current_prolog_flag(shared_object_search_path, EnvVar),
	search_path_separator(Sep),
	(   getenv(EnvVar, Env),
	    concat_atom(Path, Sep, Env)
	->  true
	;   Path = []
	).


%%      add_jpl_to_classpath
%       
%       Add jpl.jar to =CLASSPATH= to facilitate callbacks

add_jpl_to_classpath :-
	absolute_file_name(jar('jpl.jar'),
			   [ access(read)
			   ], JplJAR), !,
	(   getenv('CLASSPATH', Old)
	->  true
	;   Old = '.'
	),
	(       current_prolog_flag(windows, true)
	->      Separator = ';'
	;       Separator = ':'
	),
	concat_atom([JplJAR, Old], Separator, New),
	setenv('CLASSPATH', New).


%%      libjpl(-Spec) is det.
%       
%       Return the spec for loading the   JPL shared object. This shared
%       object must be called libjpl.so as the Java System.loadLibrary()
%       call used by jpl.jar adds the lib* prefix.

libjpl(File) :-
	(   current_prolog_flag(unix, true)
	->  File = foreign(libjpl)
	;   File = foreign(jpl)
	).

%%	add_jpl_to_ldpath(+JPL) is det.
%
%	Add the directory holding jpl.so  to   search  path  for dynamic
%	libraries. This is needed for callback   from Java. Java appears
%	to use its own search  and  the   new  value  of the variable is
%	picked up correctly.

add_jpl_to_ldpath(JPL) :-
	absolute_file_name(JPL, File, [file_type(executable), file_errors(fail)]), !,
	file_directory_name(File, Dir),
	prolog_to_os_filename(Dir, OsDir),
	current_prolog_flag(shared_object_search_path, PathVar),
	add_search_path(PathVar, OsDir).
add_jpl_to_ldpath(_).

%%	add_java_to_ldpath is det.
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

add_java_dir(DLL, SubPath, Dirs) :-
	(   check_shared_object(DLL, _, _Var, Abs),
	    Abs \== (-)
	->  Dirs = []
	;   java_home(JavaHome)
	->  atom_concat(JavaHome, SubPath, ClientDir),
	    Dirs = [ClientDir]
	;   Dirs = []
	).
	    
%%	java_home(-Home) is semidet
%
%	Find the home location of Java.
%
%	@param Home	JAVA home in OS notation

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
	add_jpl_to_ldpath(JPL),
	catch(load_foreign_library(JPL), E, report_java_setup_problem(E)),
	assert(jvm_ready).

report_java_setup_problem(E) :-
	print_message(error, E),
	check_java_environment.

:- initialization
   setup_jvm.
