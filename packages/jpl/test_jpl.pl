:- module(test_jpl,
	  [ run_tests/0,
	    run_tests/1
	  ]).
% ensure we get the local copies

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(jpl_examples, 'examples/prolog')).
:- asserta(user:file_search_path(jar, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(jpl)).
:- use_module(library(plunit)).

:- jpl:add_search_path('CLASSPATH', 'jpltest.jar').

:- begin_tests(jpl).

test(
	ancestor_types_1,
	[	true(
			Ts == [class([jpl],['Compound']),class([jpl],['Term']),class([java,lang],['Object'])]
		)
	]
) :-
	jpl:jpl_type_to_ancestor_types( class([jpl],['Atom']), Ts).

test(
	call_array_equals_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1),
			jpl_new( array(byte), [4,5,6], A2)
		))
	]
) :-
	jpl_call( A1, equals, [A2], @(false)).

test(
	call_array_equals_2,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1)
		))
	]
) :-
	jpl_call( A1, equals, [A1], @(true)).

test(
	call_array_hashcode_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A)
		)),
		true((
			integer( H)
		))
	]
) :-
	jpl_call( A, hashCode, [], H).

test(
	call_array_hashcode_2,
	[	setup((
			jpl_new( array(byte), [4,5,6], A1),
			jpl_new( array(byte), [4,5,6], A2)
		)),
		true((
			H1 \== H2
		))
	]
) :-
	jpl_call( A1, hashCode, [], H1),
	jpl_call( A2, hashCode, [], H2).

test(
	call_array_to_string_1,
	[	setup((
			jpl_new( array(byte), [4,5,6], A)
		)),
		true((
			atom_codes( S, [0'[, 0'B | _])
		))
	]
) :-
	jpl_call( A, toString, [], S).

test(
	call_instance_param_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acyclic,T),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( Test, methodInstanceTerm, [{T}], @(true)).

testX(
	call_instance_param_cyclic_term_2,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(type_error(acyclic,_),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( Test, methodInstanceTerm, [{T}], @(true)).

test(
	call_method_static_array_1,
	[	setup((
			jpl_new( array(int), [3,4,5], IntArray)
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticArray, [IntArray], 'int[]').

test(
	call_method_static_array_2,
	[	setup((
			jpl_new( array(byte), [3,4,5], ByteArray)
		)),
		throws(
			error(
				type_error(method_params,[ByteArray]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticArray, [ByteArray], _).

test(
	call_static_param_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(type_error(acyclic,T),context(jpl_call/4,_))
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticTerm, [{T}], @(true)).

test(
	call_class_get_name_1,
	[	setup((
			ClassName = 'java.lang.Integer',
			jpl_classname_to_class( ClassName, ClassObject)
		)),
		true((
			ClassName == ClassName2
		))
	]
) :-
	jpl_call( ClassObject, getName, [], ClassName2).

test(
	call_get_array_bad_field_name_1,
	[	setup((
			jpl_new( array(byte), 5, A),
			FieldName = colour
		)),
		throws(
			error(domain_error(array_field_name,FieldName),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, FieldName, _).

test(
	call_get_array_bad_fspec_1,
	[	setup((
			jpl_new( array(byte), 5, A),
			Fspec = poo(77)
		)),
		throws(
			error(type_error(array_lookup_spec,Fspec),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, Fspec, _).

test(
	call_get_array_bad_index_range_1,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,(-1)-2),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, (-1)-2, _).

test(
	call_get_array_bad_index_range_2,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,10-12),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, 10-12, _).

test(
	call_get_array_bad_index_range_3,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index_range,3-33),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, 3-33, _).

test(
	call_get_array_bad_index_range_4,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(type_error(array_index_range,this-that),context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, this-that, _).

test(
	get_array_element_1,
	[	setup((
			jpl_new( array(byte), [4,5,6,7,8], A)
		)),
		true((
			7 == V
		))
	]
) :-
	jpl_get( A, 3, V). % should bind V = 7 i.e. a[3] i.e. the fourth array element counting from zero 

test(
	get_array_elements_1,
	[	setup((
			jpl_new( array(byte), [4,5,6,7,8], A)
		)),
		true((
			[5,6] == V
		))
	]
) :-
	jpl_get( A, 1-2, V). % should bind V = [5,6] i.e. a[1-2] i.e. the 2nd to 3rd array elements counting from zero 

test(
	get_array_length_1,
	[	setup((
			Len1 is 5,
			jpl_new( array(byte), Len1, A)
		)),
		true((
			Len1 == Len2
		))
	]
) :-
	jpl_get( A, length, Len2). % should bind Len2 to the (integer) value of Len1

test(
	get_array_negative_index_1,
	[	setup((
			BadIndex is -1,
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(domain_error(array_index,BadIndex), context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, BadIndex, _).

test(
	get_array_unbound_fspec_1,
	[	setup((
			jpl_new( array(byte), 5, A)
		)),
		throws(
			error(instantiation_error,context(jpl_get/3,_))
		)
	]
) :-
	jpl_get( A, _, _).

test(
	get_field_static_boolean_1,
	[	true((
			V == @(false)
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticBoolean1, V).

test(
	get_field_static_boolean_2,
	[	true((
			V == @(true)
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticBoolean2, V).

test(
	get_field_static_char_1,
	[	true((
			V == 0
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticChar1, V).

test(
	get_field_static_char_2,
	[	true((
			V == 65535
		))
	]
) :-
	jpl_get( 'jpl.test.Test', fieldStaticChar2, V).

test(
	get_field_instance_byte_2,
	[	setup((
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		true((
			V == -1
		))
	]
) :-
	jpl_get( Test, fieldInstanceByte2, V).

test(
	list_to_array_1,
	[	true((
			Type == array(byte)
		))
	]
) :-
	jpl_list_to_array( [1,2,3], A),
	jpl_object_to_type( A, Type).

test(
	method_static_byte_1,
	[	throws(
			error(
				type_error(method_params,[-129]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoByte, [-129], _).

test(
	method_static_echo_boolean_1,
	[	setup((
			jpl_false( V1)
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoBoolean, [V1], V2).

test(
	method_static_echo_boolean_2,
	[	setup((
			jpl_true( V1)
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoBoolean, [V1], V2).

test(
	method_static_echo_char_1,
	[	setup((
			V1 = 0
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], V2).

test(
	method_static_echo_char_2,
	[	setup((
			V1 = 65535
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], V2).

test(
	method_static_char_3,
	[	setup((
			V1 = -1
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_char_4,
	[	setup((
			V1 = 1.0
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_char_5,
	[	setup((
			V1 = a
		)),
		throws(
			error(
				type_error(method_params,[V1]),
				context(jpl_call/4,_)
			)
		)
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoChar, [V1], _).

test(
	method_static_echo_double_1,
	[	setup((
			V1 = 1.5
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_double_2,
	[	setup((
			V1 = 2
		)),
		true((
			V2 =:= float(V1)
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_double_3,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**63-1
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoDouble, [V1], V2).

test(
	method_static_echo_float_1,
	[	setup((
			V1 = 1.5
		)),
		true((
			V1 == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_2,
	[	setup((
			V1 is 2,
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_3,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**63-1 % was 2**99
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	method_static_echo_float_4,
	[	blocked('we do not yet widen unbounded integers to floats or doubles'),
		setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V1)
		    ;   V1 is 2**99		% an unbounded integer
		    ),
			V2b is float(V1)
		)),
		true((
			V2 == V2b
		))
	]
) :-
	jpl_call( 'jpl.test.Test', methodStaticEchoFloat, [V1], V2).

test(
	new_abstract_class_1,
	[	setup((
			Classname = 'java.util.Dictionary'
		)),
		throws(
			error(
				type_error(concrete_class,Classname),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( Classname, [], _).

test(
	new_array_boolean_from_val_1,
	[	setup((
			jpl_false( V)
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayBooleanFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_array_double_from_val_1,
	[	setup((
			V is 1.5
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayDoubleFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_array_float_from_val_1,
	[	setup((
			V is 1.5
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_call( 'jpl.test.Test', newArrayFloatFromValue, [V], A),
	jpl_get( A, 0, V2).

test(
	new_interface_1,
	[	setup((
			Classname = 'java.util.Enumeration'
		)),
		throws(
			error(
				type_error(concrete_class,Classname),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( Classname, [], _).

test(
	new_param_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_new/3,_)
			)
		)
	]
) :-
	jpl_new( 'jpl.test.Test', [{T}], _).

test(
	prolog_calls_java_calls_prolog_1,
	[	true((
			V == @(true)
		))
	]
) :-
	jpl_new( 'jpl.Query', ['4 is 2+2'], Q),
	jpl_call( Q, hasSolution, [], V).

test(
	set_array_element_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( array(class([jpl,test],['Test'])), 5, A)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 0, {T}).

test(
	set_array_elements_bad_type_1,
	[	setup((
			jpl_new( array(byte), 3, A)
		)),
		throws(
			error(
				type_error(array(byte),[128]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 0, 128).

test(
	set_array_length_1,
	[	setup((
			jpl_new( array(byte), 6, A)
		)),
		throws(
			error(
				permission_error(modify,final_field,length),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, length, 13).

test(
	set_field_bad_field_spec_1,
	[	setup((
			BadFieldName = 3.7
		)),
		throws(
			error(
				type_error(field_name,BadFieldName),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', BadFieldName, a).

test(
	set_field_instance_cyclic_term_1,
	[	setup((
			T = f(T),
			jpl_new( 'jpl.test.Test', [], Test)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( Test, instanceTerm, {T}).

test(
	set_field_long_array_1,
	[	setup((
			jpl_new( array(long), [1,2,3], LongArray)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLongArray, LongArray).

test(
	set_field_long_array_2,
	[	setup((
			jpl_new( array(int), [1,2,3], IntArray)
		)),
		throws(
			error(
				type_error('[J',IntArray),	% NB '[J' is *not* how the type was specified in the failing goal
				context(
					jpl_set/3,
					'the value is not assignable to the named field of the class'
				)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLongArray, IntArray).

test(
	set_field_object_array_1,
	[	setup((
			jpl_new( 'java.util.Date', [], Date),
			jpl_new( array(class([java,lang],['Object'])), [Date,Date], ObjArray)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticObjectArray, ObjArray).

test(
	set_field_static_bad_type_1,
	[	setup((
			BadVal = 27
		)),
		throws(
			error(
				type_error(boolean,BadVal),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, BadVal).

test(
	set_field_static_boolean_1,
	[	setup((
			jpl_true( V)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, V).

test(
	set_field_static_boolean_2,
	[	setup((
			jpl_false( V)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, V).

test(
	set_field_static_boolean_bad_1,
	[	setup((
			BadVal = foo(bar)
		)),
		throws(
			error(
				type_error(field_value,BadVal),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticBoolean, BadVal).

test(
	set_field_static_cyclic_term_1,
	[	setup((
			T = f(T)
		)),
		throws(
			error(
				type_error(acyclic,T),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', staticTerm, {T}).

test(
	set_field_static_final_int_1,
	[	setup((
			FieldName = fieldStaticFinalInt,
			Value = 6
		)),
		throws(
			error(
				permission_error(modify,final_field,FieldName),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', FieldName, Value).

test(
	set_field_static_shadow_1,
	[	blocked('we do not yet resolve same-named shadowed fields')
	]
) :-
	jpl_set( 'jpl.test.ShadowB', fieldStaticInt, 3).

test(
	set_field_static_term_1,
	[	setup((
			T1 = foo(bar,33),
			T2 = bar(77,bing)
		)),
		true((
			T1 == T1a,
			T2 == T2a
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T1a}),
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T2}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T2a}).

test(
	set_field_static_term_2,
	[	setup((
			T1 = foo(bar,33),
			T2 = bar(77,bing)
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T1}),
	jpl_set( 'jpl.test.Test', fieldStaticTerm, {T2}),
	jpl_get( 'jpl.test.Test', fieldStaticTerm, {T2}).

test(
	set_get_array_element_boolean_1,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = @(false)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_boolean_2,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = @(true)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_boolean_3,
	[	setup((
			jpl_new( array(boolean), 3, A),
			V = bogus
		)),
		throws(
			error(
				type_error(array(boolean),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_element_byte_1,
	[	setup((
			jpl_new( array(byte), 3, A),
			V = 33
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_byte_2,
	[	setup((
			jpl_new( array(byte), 3, A),
			V = 128
		)),
		throws(
			error(
				type_error(array(byte),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_element_char_1,
	[	setup((
			jpl_new( array(char), 3, A),
			V = 65535
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_double_1,
	[	setup((
			jpl_new( array(double), 3, A),
			V = 2.5
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_1,
	[	setup((
			jpl_new( array(float), 3, A),
			V = 7.5
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_2,
	[	setup((
			jpl_new( array(float), 3, A),
			V is 2,
			VrX is float(V)
		)),
		true((
			VrX == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_float_3,
	[	setup((
			jpl_new( array(float), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, Imax)
			;	Imax is 2**63-1
			),
			VrX is float(Imax)
		)),
		true((
			VrX == Vr
		))
	]
) :-
	jpl_set( A, 2, Imax),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_long_1,
	[	setup((
			jpl_new( array(long), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, V)
			;	V is 2**63-1
			)
		)),
		true((
			V == Vr
		))
	]
) :-
	jpl_set( A, 2, V),
	jpl_get( A, 2, Vr).

test(
	set_get_array_element_long_2,
	[	setup((
			jpl_new( array(long), 3, A),
			(	current_prolog_flag( bounded, true)
			->	current_prolog_flag( max_integer, V)
			;	V is 2**63
			)
		)),
		throws(
			error(
				type_error(array(long),[V]),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( A, 2, V).

test(
	set_get_array_elements_boolean_1,
	[	setup((
			jpl_new( array(boolean), 3, A),
			Vf = @(false),
			Vt = @(true)
		)),
		true((
			Vf+Vt+Vf == Vr0+Vr1+Vr2
		))
	]
) :-
	jpl_set( A, 0, Vf),
	jpl_set( A, 1, Vt),
	jpl_set( A, 2, Vf),
	jpl_get( A, 0, Vr0),
	jpl_get( A, 1, Vr1),
	jpl_get( A, 2, Vr2).

test(
	set_get_field_static_long_1,
	[	setup((
			(   current_prolog_flag( bounded, true)
		    ->  current_prolog_flag( max_integer, V)
		    ;   V is 2**63-1
		    )
		)),
		true((
			V == V2
		))
	]
) :-
	jpl_set( 'jpl.test.Test', fieldStaticLong, V),
	jpl_get( 'jpl.test.Test', fieldStaticLong, V2).

test(
	set_non_accessible_field_1,
	[	throws(
			error(
				existence_error(field,gagaga),
				context(jpl_set/3,_)
			)
		)
	]
) :-
	jpl_set( 'jpl.test.Test', gagaga, 4).

test(
	terms_to_array_1,
	[]
) :-
	jpl_terms_to_array( [foo(bar)], A),
	jpl_object_to_type( A, array(class([jpl],['Term']))),
	jpl_get( A, length, 1),
	jpl_get( A, 0, T),
	jpl_call( T, toString, [], 'foo(bar)').

test(
	throw_java_exception_1,
	[	blocked('part of the error term is nondeterministic: we need to match with _'),
		throws(
			error(
				java_exception(@(_)),
				'java.lang.NumberFormatException'
			)
		)
	]
) :-
	jpl_call( 'java.lang.Integer', decode, [q], _).

test(
	versions_1,
	[	true((
			Vpl == Vc,
			Vc == Vjava
		))
	]
) :-
	jpl_pl_lib_version(Vpl),
	jpl_c_lib_version(Vc),
	jpl_call( 'jpl.JPL', version_string, [], Vjava).

%	JW: Mutual recursion check.  Moved from jpl.pl to here.  As the
%	callback is in module user, we define it there.

user:jpl_test_fac( N, F) :-
	(	N == 1
	->	F = 1
	;	N > 1
	->	N2 is N-1,
		jpl_call( 'jpl.test.Test', fac, [N2], F2),	% call its Java counterpart, which does vice versa
		F is N*F2
	;	F = 0
	).

test(fac10,
     [ true(N==3628800)
     ]) :-
     jpl_test_fac(10, N).

test(threads1,
	[	true((
			thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
			thread_join(ThreadId, true)
		))
	]
) :-
	jpl_call('java.lang.System', currentTimeMillis, [], _).

test(threads2, true(X==true)) :-
	jpl_call('java.lang.System', currentTimeMillis, [], _),
	thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
	thread_join(ThreadId, X).

test(jref1,
	[	true((
			Term1 \== Term2,
			Term1 =@= Term2
		))
	]
) :-
	length(Term1, 5),
	jpl:jni_term_to_jref(Term1, JRef),
	jpl:jni_jref_to_term(JRef, Term2).

:- end_tests(jpl).
