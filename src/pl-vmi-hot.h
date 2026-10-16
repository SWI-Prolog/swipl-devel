/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The VMI functions that use most of the time, used by pl-wam.c to group
them in the .text.hot section when using tail-calling VMI functions.
Generated from a profile of bench/ and the tabling benchmarks using
doc/tabling/tools/vmi-hot.sh.  Each line is VMI_HOT(function).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI_HOT(helper_t_const)
VMI_HOT(helper_I_FEXITNDET)
VMI_HOT(helper_foreign_redo)
VMI_HOT(helper_shallow_backtrack)
VMI_HOT(instr_S_STATIC)
VMI_HOT(helper_bvar_cont)
VMI_HOT(helper_deep_backtrack)
VMI_HOT(helper_depart_or_retry_continue)
VMI_HOT(instr_S_TRUSTME)
VMI_HOT(helper_exit_continue)
VMI_HOT(instr_I_DEPART)
VMI_HOT(instr_I_CALL)
VMI_HOT(instr_L_VAR)
VMI_HOT(instr_S_LIST)
VMI_HOT(helper_h_const)
VMI_HOT(instr_H_VAR)
VMI_HOT(instr_I_CUT)
VMI_HOT(helper_normal_call)
VMI_HOT(instr_I_EXITRESET)
VMI_HOT(helper_i_metacall_common)
VMI_HOT(instr_B_FIRSTVAR)
VMI_HOT(helper_a_var_n)
VMI_HOT(instr_H_ATOM)
VMI_HOT(instr_H_LIST_FF)
VMI_HOT(instr_I_FCALLDETVA)
VMI_HOT(instr_H_RLIST)
VMI_HOT(instr_I_RESET)
VMI_HOT(instr_I_LCALL)
VMI_HOT(instr_I_EXIT)
VMI_HOT(instr_I_ENTER)
VMI_HOT(helper_exit_checking_wakeup)
VMI_HOT(helper_a_cmp_out)
VMI_HOT(instr_H_FIRSTVAR)
VMI_HOT(instr_H_POP)
VMI_HOT(instr_B_ARGVAR)
VMI_HOT(instr_B_RFUNCTOR)
VMI_HOT(instr_A_LE)
VMI_HOT(instr_T_FUNCTOR)
VMI_HOT(instr_C_OR)
VMI_HOT(instr_I_FREDO)
VMI_HOT(instr_T_TRY_SMALLINT)
VMI_HOT(instr_B_VAR0)
VMI_HOT(instr_B_VAR)
VMI_HOT(instr_I_TCALL)
VMI_HOT(instr_S_TABLED)
VMI_HOT(helper_common_an)
VMI_HOT(instr_A_INTEGER)
VMI_HOT(instr_H_RFUNCTOR)
VMI_HOT(instr_I_EXITFACT)
VMI_HOT(instr_L_NOLCO)
VMI_HOT(helper_unify_backtrack)
VMI_HOT(instr_A_ENTER)
VMI_HOT(instr_H_LIST)
VMI_HOT(instr_H_FUNCTOR)
VMI_HOT(instr_B_ATOM)
VMI_HOT(instr_A_ADD)
VMI_HOT(instr_S_LMQUAL)
VMI_HOT(helper_I_FEXITDET)
VMI_HOT(helper_depart_alerted)
VMI_HOT(helper_c_cut)
VMI_HOT(instr_B_RLIST)
VMI_HOT(instr_B_POP)
VMI_HOT(instr_T_STRING)
VMI_HOT(instr_B_VAR1)
VMI_HOT(instr_A_ADD_FC)
VMI_HOT(instr_A_LT)
VMI_HOT(instr_I_VAR)
VMI_HOT(instr_I_FCALLNDETVA)
VMI_HOT(instr_I_EXITCLEANUP)
VMI_HOT(instr_B_FUNCTOR)
VMI_HOT(instr_A_IS)
VMI_HOT(instr_B_ARG_VF)
VMI_HOT(instr_B_UNIFY_VV)
VMI_HOT(instr_A_MUL)
VMI_HOT(instr_A_VAR1)
VMI_HOT(instr_H_NIL)
VMI_HOT(instr_B_VAR2)
VMI_HOT(helper_mcall_cont)
VMI_HOT(instr_S_MQUAL)
VMI_HOT(instr_H_VOID)
