/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2016, University of Amsterdam
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

#ifndef PL_LDPASS_H_INCLUDED
#define PL_LDPASS_H_INCLUDED

		 /*******************************
		 *	LD-USING FUNCTIONS	*
		 *******************************/

#define allocGlobal(n)		allocGlobal__LD(n PASS_LD)
#define getInputStream(t, k, s)		getInputStream__LD(t, k, s PASS_LD)
#define getTextInputStream(t, s)	getTextInputStream__LD(t, s PASS_LD)
#define getBinaryInputStream(t, s)	getBinaryInputStream__LD(t, s PASS_LD)
#define getOutputStream(t, k, s)	getOutputStream__LD(t, k, s PASS_LD)
#define getTextOutputStream(t, s)	getTextOutputStream__LD(t, s PASS_LD)
#define getBinaryOutputStream(t, s)	getBinaryOutputStream__LD(t, s PASS_LD)
#define getCharsString(s, l)	getCharsString__LD(s, l PASS_LD)
#define getCharsWString(s, l)	getCharsWString__LD(s, l PASS_LD)
#define compileTermToHeap(t, f)	\
	compileTermToHeap__LD(t, NULL, NULL, f PASS_LD)
#define linkVal(p)		linkVal__LD(p PASS_LD)
#define TrailAssignment(p)	TrailAssignment__LD(p PASS_LD)
#define bindConst(p, c)		bindConst__LD(p, c PASS_LD)
#define consPtr(p, ts)		consPtr__LD(p, ts PASS_LD)
#define allocGlobalNoShift(n)	allocGlobalNoShift__LD(n PASS_LD)
#define getProcDefinition(proc)	getProcDefinition__LD(proc->definition PASS_LD)
#define popTermRef()		popTermRef__LD(PASS_LD1)
#define pushWordAsTermRef(p)	pushWordAsTermRef__LD(p PASS_LD)
#define lookupHTable(ht, n)	lookupHTable__LD(ht, n PASS_LD)
#define pushPredicateAccess(def, gen) pushPredicateAccess__LD(def, gen PASS_LD)
#define popPredicateAccess(def) popPredicateAccess__LD(def PASS_LD)
#define popNPredicateAccess(cnt) popNPredicateAccess__LD(cnt PASS_LD)
#define nextClause(chp, argv, fr, def) nextClause__LD(chp, argv, fr, def PASS_LD)
#define lookupModule(name)	lookupModule__LD(name PASS_LD)
#define isCurrentModule(name)	isCurrentModule__LD(name PASS_LD)
#define isCurrentProcedure(f,m) isCurrentProcedure__LD(f, m PASS_LD)
#define resolveProcedure(f,m)	resolveProcedure__LD(f, m PASS_LD)
#define ensureLocalSpace(n)	likely(ensureLocalSpace__LD(n PASS_LD))

#define _PL_get_arg(n, t, a)	_PL_get_arg__LD(n, t, a PASS_LD)
#define _PL_put_number(t, n)	_PL_put_number__LD(t, n PASS_LD)
#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_ref_noshift()	PL_new_term_ref_noshift__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)
#define PL_reset_term_refs(t)	PL_reset_term_refs__LD(t PASS_LD)
#define PL_copy_term_ref(t)	PL_copy_term_ref__LD(t PASS_LD)
#define PL_unify(t1, t2)	PL_unify__LD(t1, t2 PASS_LD)
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)
#define PL_unify_int64(t, i)	PL_unify_int64__LD(t, i PASS_LD)
#define PL_unify_functor(t, f)	PL_unify_functor__LD(t, f PASS_LD)
#define PL_unify_term(t, ...)	PL_unify_term__LD(t PASS_LD, __VA_ARGS__)
#define PL_unify_output(t1,t2)	PL_unify_output__LD(t1, t2 PASS_LD)
#define PL_get_atom(t, a)	PL_get_atom__LD(t, a PASS_LD)
#define PL_put_atom(t, a)	PL_put_atom__LD(t, a PASS_LD)
#define PL_put_variable(t)	PL_put_variable__LD(t PASS_LD)
#define PL_is_functor(t, f)	PL_is_functor__LD(t, f PASS_LD)
#define PL_put_integer(t, i)	PL_put_integer__LD(t, i PASS_LD)
#define PL_put_intptr(t, i)	PL_put_intptr__LD(t, i PASS_LD)
#define PL_strip_module(q, m, t) PL_strip_module__LD(q, m, t, 0 PASS_LD)
#define PL_get_integer(t, i)	PL_get_integer__LD(t, i PASS_LD)
#define PL_get_long(t, i)	PL_get_long__LD(t, i PASS_LD)
#define PL_get_int64(t, i)	PL_get_int64__LD(t, i PASS_LD)
#define PL_get_size_ex(t,i)	PL_get_size_ex__LD(t,i PASS_LD)
#define PL_get_number(t, n)	PL_get_number__LD(t, n PASS_LD)
#define PL_unify_number(t, n)	PL_unify_number__LD(t, n PASS_LD)
#define PL_put_number(t, n)	PL_put_number__LD(t, n PASS_LD)
#define PL_get_pointer(t, ptr)	PL_get_pointer__LD(t, ptr PASS_LD)
#define PL_put_term(t1, t2)	PL_put_term__LD(t1, t2 PASS_LD)
#define PL_get_functor(t, f)	PL_get_functor__LD(t, f PASS_LD)
#define PL_unify_atom(t, a)	PL_unify_atom__LD(t, a PASS_LD)
#define PL_unify_pointer(t, p)	PL_unify_pointer__LD(t, p PASS_LD)
#define PL_is_variable(t)	PL_is_variable__LD(t PASS_LD)
#define PL_is_atomic(t)		PL_is_atomic__LD(t PASS_LD)
#define PL_get_list(l, h, t)	PL_get_list__LD(l, h, t PASS_LD)
#define PL_is_atom(t)		PL_is_atom__LD(t PASS_LD)
#define PL_unify_list(l, h, t)	PL_unify_list__LD(l, h, t PASS_LD)
#define PL_cons_list(l, h, t)	PL_cons_list__LD(l, h, t PASS_LD)
#define PL_get_text(l, t, f)	PL_get_text__LD(l, t, f PASS_LD)
#define PL_unify_int64_ex(t, i)	PL_unify_int64_ex__LD(t, i PASS_LD)
#define PL_pending(sig)	        PL_pending__LD(sig PASS_LD)
#define PL_clearsig(sig)        PL_clearsig__LD(sig PASS_LD)
#define PL_same_term(t1, t2)	PL_same_term__LD(t1, t2 PASS_LD)
#define PL_get_name_arity_sz(t,n,a) PL_get_name_arity_sz__LD(t,n,a PASS_LD)

#define PL_get_atom_ex(t, a)	PL_get_atom_ex__LD(t, a PASS_LD)
#define PL_open_foreign_frame() PL_open_foreign_frame__LD(PASS_LD1)
#define PL_close_foreign_frame(id) PL_close_foreign_frame__LD(id PASS_LD)
#define PL_strip_module_ex(t,m,p) PL_strip_module_ex__LD(t,m,p PASS_LD)
#define var_name_ptr(p, n)	var_name_ptr__LD(p, n PASS_LD)
#define classify_exception(ex)  classify_exception__LD(ex PASS_LD)
#define classify_exception_p(p) classify_exception_p__LD(p PASS_LD)

#endif /*PL_LDPASS_H_INCLUDED*/
