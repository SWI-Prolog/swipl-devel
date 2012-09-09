/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
#define valFloat(w)		valFloat__LD(w PASS_LD)
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

#define _PL_get_arg(n, t, a)	_PL_get_arg__LD(n, t, a PASS_LD)
#define _PL_put_number(t, n)	_PL_put_number__LD(t, n PASS_LD)
#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_ref_noshift()	PL_new_term_ref_noshift__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)
#define PL_unify(t1, t2)	PL_unify__LD(t1, t2 PASS_LD)
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)
#define PL_unify_int64(t, i)	PL_unify_int64__LD(t, i PASS_LD)
#define PL_get_atom(t, a)	PL_get_atom__LD(t, a PASS_LD)
#define PL_put_atom(t, a)	PL_put_atom__LD(t, a PASS_LD)
#define PL_put_variable(t)	PL_put_variable__LD(t PASS_LD)
#define PL_is_functor(t, f)	PL_is_functor__LD(t, f PASS_LD)
#define PL_put_integer(t, i)	PL_put_integer__LD(t, i PASS_LD)
#define PL_put_intptr(t, i)	PL_put_intptr__LD(t, i PASS_LD)
#define PL_strip_module(q, m, t) PL_strip_module__LD(q, m, t PASS_LD)
#define PL_get_integer(t, i)	PL_get_integer__LD(t, i PASS_LD)
#define PL_get_long(t, i)	PL_get_long__LD(t, i PASS_LD)
#define PL_get_int64(t, i)	PL_get_int64__LD(t, i PASS_LD)
#define PL_unify_number(t, n)	PL_unify_number__LD(t, n PASS_LD)
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

#define PL_get_atom_ex(t, a)	PL_get_atom_ex__LD(t, a PASS_LD)
#define PL_open_foreign_frame() PL_open_foreign_frame__LD(PASS_LD1)
#define PL_close_foreign_frame(id) PL_close_foreign_frame__LD(id PASS_LD)
#define PL_strip_module_ex(t,m,p) PL_strip_module_ex__LD(t,m,p PASS_LD)

#endif /*PL_LDPASS_H_INCLUDED*/
