/* Copyright */

:- module(test_iEEE, [test_iEEE/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_iEEE :-
    run_tests([ iEEE
	      ]).

:- begin_tests(iEEE,
	       [ condition(current_prolog_flag(bounded, false)),
                 setup(set_float_flags(Old, [
					flag(float_overflow,infinity),
					flag(float_zero_div,infinity),
					flag(float_undefined,nan),
					flag(float_underflow,ignore),
					flag(float_rounding,nearest)           %*** Eclipse is "to_nearest"
				 ])),
                 cleanup(set_float_flags(_, Old))
	       ]).

test(iEEE_flags) :-
	assertion((
		get_set_flag(float_overflow,Old_o,error),
		get_set_flag(float_overflow,error,infinity),
		get_set_flag(float_overflow,infinity,Old_o),
		get_set_flag(float_zero_div,Old_z,error),
		get_set_flag(float_zero_div,error,infinity),
		get_set_flag(float_zero_div,infinity,Old_z),
		get_set_flag(float_undefined,Old_n,error),
		get_set_flag(float_undefined,error,nan),
		get_set_flag(float_undefined,nan,Old_n),
		get_set_flag(float_underflow,Old_u,error),
		get_set_flag(float_underflow,error,ignore),
		get_set_flag(float_underflow,ignore,Old_u),
		get_set_flag(float_rounding,Old_r,nearest),        %*** Eclipse is "to_nearest"
		get_set_flag(float_rounding,nearest,to_positive),  %*** Eclipse is "to_nearest"
		get_set_flag(float_rounding,to_positive,to_negative),
		get_set_flag(float_rounding,to_negative,to_zero),
		get_set_flag(float_rounding,to_zero,Old_r)
		)).
		
%% Basic Arithmetic Tests (from Eclipse paper) %%%%%%%%%%%%

test(iEEE_cmp) :-
	assertion( 0.0 =:= -0.0),
	assertion( 0.0 \== -0.0),
	assertion( inf  >  -inf),
	assertion( nan =\=  nan),
	assertion( nan ==   nan),
	assertion(not(nan  <  nan)),
	assertion(not(nan =<  nan)),
	assertion(not(nan >=  nan)),
	assertion(not(nan  >  nan)),
	assertion(not(nan =:= nan)),
	assertion(1.5NaN is nan),
	assertion( nan =\=  0.0),
	assertion(not(nan =:= 0.0)),
	assertion(not(nan  <  0.0)),
	assertion(not(nan =<  0.0)),
	assertion(not(nan >=  0.0)),
	assertion(not(nan  >  0.0)),
	assertion(-inf  <   -0.0),
	assertion( 0.0  <   inf),
	assertion(sqrt(-3) =\= sqrt(-2)).

test(iEEE_tcmp ) :-  % placement in standard term order
	assertion(( 1.5NaN @< -1.0Inf,
			   -1.0Inf @< -0.0,
			   -0.0    @<  0.0,
			    0.0    @<  1.0Inf)),
	assertion(( 1.0Inf @>  0.0,
			    0.0    @> -0.0,
			   -0.0    @> -1.0Inf,
			   -1.0Inf @>  1.5NaN)),
	assertion(( 1.5NaN @< -1.0Inf,
			   -1.0Inf @<  0,
			    0      @<  1.0Inf)),
	assertion(( 1.0Inf @>  0,
			    0      @> -1.0Inf,
			   -1.0Inf @>  1.5NaN)),
	assertion((compare(O,1.5NaN,0.0),compare(O,1.5NaN,0))),
	assertion(sort([1.0Inf,    0.0, -0.0, -1.0Inf, 1.5NaN],
				   [1.5NaN,-1.0Inf, -0.0,    0.0,  1.0Inf])).

test(iEEE_minus) :-
	assertion(    0.0 is - -0.0),
	assertion(   -0.0 is -  0.0),
	assertion( 1.0Inf is - -inf),
	assertion(-1.0Inf is -  inf),
	assertion( 1.5NaN is -  nan).

test(iEEE_add) :-
	assertion(    0.0 =:= -0.0+  0.0),
	assertion(    0.0 =:=  0.0+ -0.0),
	assertion(   -0.0 =:= -0.0+ -0.0),
	assertion( 1.0Inf =:= -0.0+  inf),
	assertion( 1.5NaN is   inf+ -inf),
	assertion( 1.5NaN is  -inf+  inf),
	assertion(-1.0Inf is  -inf+  0.0),
	assertion( 1.5NaN is   nan+  inf).

test(iEEE_sub) :-
	assertion(    0.0 =:=  0.0-  0.0),
	assertion(   -0.0 =:= -0.0-  0.0),
	assertion(    0.0 =:=  0.0- -0.0),
	assertion(    0.0 =:= -0.0- -0.0),
	assertion( 1.0Inf =:=  inf- -0.0),
	assertion( 1.5NaN is  -inf- -inf),
	assertion( 1.5NaN is   inf-  inf),
	assertion( 1.0Inf is   inf- -inf),
	assertion(-1.0Inf is  -inf-  inf),
	assertion(-1.0Inf is   1.0-  inf),
	assertion(-1.0Inf is  -inf-  1.0),
	assertion( 1.0Inf is   1.0- -inf),
	assertion( 1.5NaN is   inf-  nan).

test(iEEE_mul) :-
	assertion(   -0.0 =:= -0.0*  1.0),
	assertion(    0.0 =:= -0.0* -1.0),
	assertion( 1.0Inf is   inf*  inf),
	assertion(-1.0Inf is   inf* -inf),
	assertion( 1.0Inf is  -inf* -inf),
	assertion( 1.0Inf is   inf*  1.0),
	assertion(-1.0Inf is   1.0* -inf),
	assertion( 1.5NaN is   0.0* inf),
	assertion( 1.5NaN is   nan* inf).
	
test(iEEE_div) :-
	assertion( 1.5NaN is -0.0/  0.0),
	assertion( 1.5NaN is -0.0/ -0.0),
	assertion( 1.5NaN is  0.0/  0.0),
	assertion( 1.5NaN is  0.0/ -0.0),
	assertion(   -0.0 is -0.0/  1.0),
	assertion(    0.0 is -0.0/ -1.0),
	assertion(-1.0Inf is  1.0/ -0.0),
	assertion( 1.0Inf is -1.0/ -0.0),
	assertion( 1.5NaN is -inf/ -inf),
	assertion( 1.5NaN is  inf/  inf),
	assertion( 1.5NaN is  inf/ -inf),
	assertion( 1.5NaN is -inf/  inf),
	assertion( 1.0Inf is  inf/  1.0),
	assertion(    0.0 is  1.0/  inf),
	assertion(-1.0Inf is -inf/  1.0),
	assertion(   -0.0 is  1.0/ -inf),
	assertion(-1.0Inf is  inf/ -1.0),
	assertion(   -0.0 is -1.0/  inf),
	assertion( 1.0Inf is -inf/ -1.0),
	assertion(    0.0 is -1.0/ -inf),
	assertion( 1.5NaN is -1.0/ nan),
	assertion( 1.5NaN is  nan/ 1.0).

test(iEEE_sign) :-
	assertion( 0.0    is sign(-0.0)),
	assertion( 0.0    is sign( 0.0)),
	assertion( 1.0    is sign( inf)),
	assertion(-1.0    is sign(-inf)),
	assertion( 1.5NaN is sign( nan)).

test(iEEE_parts) :-
	assertion(float_parts(-0.5,   -0.0,   -0.5)),
	assertion(float_parts( 0.5,    0.0,    0.5)),
	assertion(float_parts(-0.0,   -0.0,   -0.0)),
	assertion(float_parts( 0.0,    0.0,    0.0)),
	assertion(float_parts( inf, 1.0Inf,    0.0)),
	assertion(float_parts(-inf,-1.0Inf,   -0.0)),
	assertion(float_parts( nan, 1.5NaN, 1.5NaN)).

%% End of Basic Arithmetic Tests %%%%%%%%%%%%%%%%%%


%% Annex F Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(iEEE_acos) :-						% C11 - F.10.1.1
	assertion(0.0 is acos(1)),
	assertion(1.5NaN is acos(2)),
	assertion(1.5NaN is acos(-2)),
	assertion(1.5NaN is acos(inf)),
	assertion(1.5NaN is acos(-inf)),
	assertion(1.5NaN is acos(nan)).

test(iEEE_asin) :-						% C11 - F.10.1.2
	assertion(0.0 is asin(0.0)),
	assertion(-0.0 is asin(-0.0)),
	assertion(1.5NaN is asin(2)),
	assertion(1.5NaN is asin(-2)),
	assertion(1.5NaN is asin(inf)),
	assertion(1.5NaN is asin(-inf)),
	assertion(1.5NaN is asin(nan)).

test(iEEE_atan) :-						% C11 - F.10.1.3
	assertion( 0.0  is  atan(0.0)),
	assertion(-0.0  is  atan(-0.0)),
	assertion( pi/2 =:= atan(inf)),
	assertion(-pi/2 =:= atan(-inf)),
	assertion(1.5NaN is atan(nan)).

test(iEEE_atan2) :-						% C11 - F.10.1.4
	assertion( pi =:= atan2( 0.0,-0.0)),
	assertion(-pi =:= atan2(-0.0,-0.0)),
	assertion( 0.0 is atan2( 0.0, 0.0)),
	assertion(-0.0 is atan2(-0.0, 0.0)),
	assertion( pi =:= atan2( 0.0,-1.0)),
	assertion(-pi =:= atan2(-0.0,-1.0)),
	assertion( 0.0 is atan2( 0.0, 1.0)),
	assertion(-0.0 is atan2(-0.0, 1.0)),
	assertion(-pi/2 =:= atan2(-1.0, 0.0)),
	assertion(-pi/2 =:= atan2(-1.0,-0.0)),
	assertion( pi/2 =:= atan2( 1.0, 0.0)),
	assertion( pi/2 =:= atan2( 1.0,-0.0)),
	assertion( pi   =:= atan2( 1.0,-inf)),
	assertion(-pi   =:= atan2(-1.0,-inf)),
	assertion( 0.0  is  atan2( 1.0, inf)),
	assertion(-0.0  is  atan2(-1.0, inf)),
	assertion( pi/2 =:= atan2( inf, 1.0)),
	assertion(-pi/2 =:= atan2(-inf,-1.0)),
	assertion( 3*pi/4 =:= atan2( inf,-inf)),
	assertion(-3*pi/4 =:= atan2(-inf,-inf)),
	assertion( pi/4 =:= atan2( inf, inf)),
	assertion(-pi/4 =:= atan2(-inf, inf)).
	
test(iEEE_cos) :-						% C11 - F.10.1.5
	assertion(   1.0 is cos( 0.0)),
	assertion(   1.0 is cos(-0.0)),
	assertion( 1.5NaN is  cos( inf)),
	assertion( 1.5NaN is  cos(-inf)),
	assertion( 1.5NaN is  cos( nan)).

test(iEEE_sin) :-						% C11 - F.10.1.6
	assertion(   0.0 is sin( 0.0)),
	assertion(  -0.0 is sin(-0.0)),
	assertion( 1.5NaN is sin( inf)),
	assertion( 1.5NaN is sin(-inf)),
	assertion( 1.5NaN is sin( nan)).

test(iEEE_tan) :-						% C11 - F.10.1.7
	assertion(   0.0 is tan( 0.0)),
	assertion(  -0.0 is tan(-0.0)),
	assertion( 1.5NaN is tan( inf)),
	assertion( 1.5NaN is tan(-inf)),
	assertion( 1.5NaN is tan( nan)).


test(iEEE_acosh) :-						% C11 - F.10.2.1
	assertion(   0.0 is acosh( 1.0)),
	assertion(1.0Inf is acosh( inf)),
	assertion(1.5NaN is acosh( 0.5)),
	assertion(1.5NaN is acosh( nan)).

test(iEEE_asinh) :-						% C11 - F.10.2.2
	assertion(    0.0 is asinh( 0.0)),
	assertion(   -0.0 is asinh(-0.0)),
	assertion( 1.0Inf is asinh( inf)),
	assertion(-1.0Inf is asinh(-inf)),
	assertion( 1.5NaN is asinh( nan)).

test(iEEE_atanh) :-						% C11 - F.10.2.3
	assertion(    0.0 is atanh( 0.0)),
	assertion(   -0.0 is atanh(-0.0)),
	assertion( 1.0Inf is atanh( 1.0)),
	assertion(-1.0Inf is atanh(-1.0)),
	assertion( 1.5NaN is atanh( 2.0)),
	assertion( 1.5NaN is atanh(-2.0)),
	assertion( 1.5NaN is atanh( nan)).
	
test(iEEE_cosh) :-						% C11 - F.10.2.4
	assertion(    1.0 is cosh( 0.0)),
	assertion(    1.0 is cosh(-0.0)),
	assertion( 1.0Inf is cosh( inf)),
	assertion( 1.0Inf is cosh(-inf)),
	assertion( 1.5NaN is cosh( nan)).

test(iEEE_sinh) :-						% C11 - F.10.2.5
	assertion(    0.0 is sinh( 0.0)),
	assertion(   -0.0 is sinh(-0.0)),
	assertion( 1.0Inf is sinh( inf)),
	assertion(-1.0Inf is sinh(-inf)),
	assertion( 1.5NaN is sinh( nan)).

test(iEEE_tanh) :-						% C11 - F.10.2.6
	assertion(    0.0 is tanh( 0.0)),
	assertion(   -0.0 is tanh(-0.0)),
	assertion(    1.0 is tanh( inf)),
	assertion(   -1.0 is tanh(-inf)),
	assertion( 1.5NaN is tanh( nan)).


test(iEEE_exp) :-						% C11 - F.10.3.1
	assertion(    1.0 is exp( 0.0)),
	assertion(    1.0 is exp(-0.0)),
	assertion( 1.0Inf is exp( inf)),
	assertion(    0.0 is exp(-inf)),
	assertion( 1.5NaN is exp( nan)).

test(iEEE_log) :-						% C11 - F.10.3.7
	assertion(-1.0Inf is log( 0.0)),
	assertion(-1.0Inf is log(-0.0)),
	assertion( 1.5NaN is log(-1.0)),
	assertion( 1.0Inf is log( inf)),
	assertion( 1.5NaN is log( nan)).

test(iEEE_log10) :-						% C11 - F.10.3.8
	assertion(-1.0Inf is log10( 0.0)),
	assertion(-1.0Inf is log10(-0.0)),
	assertion( 1.5NaN is log10(-1.0)),
	assertion( 1.0Inf is log10( inf)),
	assertion( 1.5NaN is log10( nan)).


test(iEEE_abs) :-						% C11 - F.10.4.2
	assertion(    0.0 is abs( 0.0)),
	assertion(    0.0 is abs(-0.0)),
	assertion( 1.0Inf is abs( inf)),
	assertion( 1.0Inf is abs(-inf)),
	assertion( 1.5NaN is abs( nan)).

test(iEEE_pow) :-						% C11 - F.10.4.4
	assertion( 1.0Inf is  0.0** -1),
	assertion(-1.0Inf is -0.0** -1),
	assertion( 1.0Inf is  0.0** -2),
	assertion( 1.0Inf is -0.0** -2),
	assertion( 1.0Inf is  0.0** (-inf)),
	assertion( 1.0Inf is -0.0** (-inf)),
	assertion(    0.0 is  0.0**  1),
	assertion(   -0.0 is -0.0**  1),
	assertion(    0.0 is  0.0**  2),
	assertion(    0.0 is -0.0**  2),
	
	assertion(    1.0 is  1.0**  inf),
	assertion(    1.0 is  1.0** (-inf)),
	assertion(    1.0 is  1.0**  nan),
	assertion(    1.0 is  inf**  0.0),
	assertion(    1.0 is -inf**  0.0),
	assertion(    1.0 is  nan**  0.0),
	assertion(    1.0 is  inf** -0.0),
	assertion(    1.0 is -inf** -0.0),
	assertion(    1.0 is  nan** -0.0),
	assertion( 1.5NaN is -1.0**  1.5),
	
	assertion( 1.0Inf is -0.5** (-inf)),
	assertion(    0.0 is  1.5** (-inf)),
	assertion(    0.0 is -0.5**  inf),
	assertion( 1.0Inf is  1.5**  inf),

	assertion(   -0.0 is -inf** -1),
	assertion(    0.0 is -inf** -2),
	assertion(-1.0Inf is -inf**  3),
	assertion( 1.0Inf is -inf**  2),
	assertion(    0.0 is  inf** -1),
	assertion( 1.0Inf is  inf**  2).

test(iEEE_sqrt) :-						% C11 - F.10.4.4 (see opengroup.org)
	assertion(    0.0 is sqrt( 0.0)),
	assertion(   -0.0 is sqrt(-0.0)),
	assertion( 1.0Inf is sqrt( inf)),
	assertion( 1.5NaN is sqrt(-inf)),
	assertion( 1.5NaN is sqrt(-1.0)),
	assertion( 1.5NaN is sqrt( nan)).


test(iEEE_erf) :-						% C11 - F.10.5.1
	assertion(    0.0 is erf( 0.0)),
	assertion(   -0.0 is erf(-0.0)),
	assertion(    1.0 is erf( inf)),
	assertion(   -1.0 is erf(-inf)),
	assertion( 1.5NaN is erf( nan)).

test(iEEE_erfc) :-						% C11 - F.10.5.2
	assertion(    0.0 is erfc( inf)),
	assertion(    2.0 is erfc(-inf)),
	assertion( 1.5NaN is erfc( nan)).

test(iEEE_lgamma) :-					% C11 - F.10.5.3
	assertion(    0.0 is lgamma( 1.0)),
	assertion(    0.0 is lgamma( 2.0)),
	assertion( 1.0Inf is lgamma( 0.0)),
	assertion( 1.0Inf is lgamma(-0.0)),
	assertion( 1.0Inf is lgamma(-1)),
	assertion( 1.0Inf is lgamma( inf)),
	assertion( 1.0Inf is lgamma(-inf)),
	assertion( 1.5NaN is lgamma( nan)).


test(iEEE_ceil) :-						% C11 - F.10.6.1
	assertion(      0 is ceiling( 0.0)),
	assertion(      0 is ceiling(-0.0)),
	assertion( 1.0Inf is ceiling( inf)),
	assertion(-1.0Inf is ceiling(-inf)),
	assertion( 1.5NaN is ceiling( nan)).

test(iEEE_floor) :-						% C11 - F.10.6.2
	assertion(      0 is floor( 0.0)),
	assertion(      0 is floor(-0.0)),
	assertion( 1.0Inf is floor( inf)),
	assertion(-1.0Inf is floor(-inf)),
	assertion( 1.5NaN is floor( nan)).


test(iEEE_round) :-						% C11 - F.10.6.6
	assertion(      0 is round( 0.0)),
	assertion(      0 is round(-0.0)),
	assertion( 1.0Inf is round( inf)),
	assertion(-1.0Inf is round(-inf)),
	assertion( 1.5NaN is round( nan)).

test(iEEE_trunc) :-						% C11 - F.10.6.8
	assertion(      0 is truncate( 0.0)),
	assertion(      0 is truncate(-0.0)),
	assertion( 1.0Inf is truncate( inf)),
	assertion(-1.0Inf is truncate(-inf)),
	assertion( 1.5NaN is truncate( nan)).


test(iEEE_copysign) :-					% C11 - F.10.8.1
	assertion(-1.0Inf is copysign( inf, -0.0)),
	assertion( 1.0Inf is copysign(-inf,  0.0)),
	assertion(    0.0 is copysign(-0.0,  inf)),
	assertion(   -0.0 is copysign( 0.0, -inf)),
	assertion(    0.0 is copysign( 0.0,  nan)),
	assertion( 1.5NaN is copysign( nan, -0.0)).

test(iEEE_nexttoward) :-				% C11 - F.10.8.4
	assertion(-1.0Inf is nexttoward(-inf, -inf)),
	assertion( 1.0Inf is nexttoward( inf,  inf)),
	assertion(-1.0Inf <  nexttoward(-inf,  inf)),
	assertion( 1.0Inf >  nexttoward( inf, -inf)),
	assertion(    0.0 is nexttoward(-0.0,  0.0)),
	assertion(   -0.0 is nexttoward( 0.0, -0.0)),
	assertion(    0.0 <  nexttoward(-0.0,  inf)),
	assertion(   -0.0 >  nexttoward( 0.0, -inf)),
	assertion( 1.5NaN is nexttoward( 0.0,  nan)),
	assertion( 1.5NaN is nexttoward( nan, -0.0)).


test(iEEE_max) :-				% C11 - F.10.9.2
	assertion(-1.0Inf is max(-inf, -inf)),
	assertion( 1.0Inf is max( inf,  inf)),
	assertion( 1.0Inf is max( inf, -inf)),
	assertion(    0.0 is max(-0.0,  0.0)),
	assertion(-1.0Inf is max( nan, -inf)),
	assertion( 1.5NaN is max( nan,  nan)).

test(iEEE_min) :-				% C11 - F.10.9.3
	assertion(-1.0Inf is min(-inf, -inf)),
	assertion( 1.0Inf is min( inf,  inf)),
	assertion(-1.0Inf is min( inf, -inf)),
	assertion(   -0.0 is min(-0.0,  0.0)),
	assertion( 1.0Inf is min( inf,  nan)),
	assertion( 1.5NaN is min( nan,  nan)).

%% End of Annex F Tests %%%%%%%%%%%%%%%%%%%%%%%%% 

test(iEEE_rmode) :-
	assertion((
		RExp = exp(log(1.1)),
		evalR(RExp,nearest,Rc),        %*** Eclipse is "to_nearest"
		evalR(RExp,to_positive,Rp),
		evalR(RExp,to_negative,Rn),
		evalR(RExp,to_zero,Rz),
		Rn =< Rc, Rc =< Rp, Rn < Rp,
		Rz =< Rc, Rz < Rp
	)),
	assertion((
		PExp = cos(-pi),
		evalR(PExp,nearest,Pc),        %*** Eclipse is "to_nearest"
		evalR(PExp,to_positive,Pp),
		evalR(PExp,to_negative,Pn),
		evalR(PExp,to_zero,Pz),
		Pn =< Pc, Pc =< Pp, Pn < Pp,
		Pz >= Pc, Pz > Pn
	)).

:- end_tests(iEEE).

%% Auxiliary predicates %%%%%%%%%%%%%%%%%%%%%%%%%

get_set_flag(Flag,Old,New) :-
	current_prolog_flag(Flag,Old),
	set_prolog_flag(Flag,New).

set_float_flags([], []).
set_float_flags([flag(Name,Old)|Olds], [flag(Name,New)|News]) :-
	get_set_flag(Name,Old,New),
	set_float_flags(Olds,News).
	
float_parts(F,Ip,Fp) :-
	Ip is float_integer_part(F),
	Fp is float_fractional_part(F).

evalR(Exp,Mode,Res) :-
	get_set_flag(float_rounding,Save,Mode),
	Res is Exp,
	get_set_flag(float_rounding,Mode,Save).

