/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#define ERR_NO_ERROR		0
#define ERR_INSTANTIATION	1	/* void */
#define ERR_TYPE		2	/* atom_t expected, term_t value */
#define ERR_DOMAIN		3	/* atom_t domain, term_t value */
#define ERR_REPRESENTATION	4	/* atom_t what */
#define ERR_MODIFY_STATIC_PROC	5	/* predicate_t proc */
#define ERR_EVALUATION		6	/* atom_t what */
#define ERR_AR_TYPE		7	/* atom_t expected, Number value */
#define ERR_NOT_EVALUABLE	8	/* functor_t func */
#define ERR_DIV_BY_ZERO		9	/* void */
#define ERR_FAILED	       10	/* predicate_t proc */
#define ERR_FILE_OPERATION     11	/* atom_t action, atom_t type, term_t */
#define ERR_PERMISSION	       12	/* atom_t type, atom_t op, term_t obj*/
#define ERR_NOTIMPLEMENTED     13	/* atom_t what */
#define ERR_EXISTENCE	       14	/* atom_t type, term_t obj */
#define ERR_STREAM_OP	       15	/* atom_t action, term_t obj */
#define ERR_RESOURCE	       16	/* atom_t resource */
#define ERR_NOMEM	       17	/* void */
#define ERR_SYSCALL	       18	/* void */
#define ERR_SHELL_FAILED       19	/* term_t command */
#define ERR_SHELL_SIGNALLED    20	/* term_t command, int signal */
#define ERR_AR_UNDEF	       21	/* void */
#define ERR_AR_OVERFLOW	       22	/* void */
#define ERR_AR_UNDERFLOW       23	/* void */
#define ERR_UNDEFINED_PROC     24	/* Definition def */
#define ERR_SIGNALLED	       25	/* int sig, char *name */
#define ERR_CLOSED_STREAM      26	/* IOSTREAM * */
#define ERR_BUSY	       27	/* mutexes */
#define ERR_PERMISSION_PROC    28	/* op, type, Definition */
#define ERR_DDE_OP	       29	/* op, error */
#define ERR_SYNTAX	       30	/* what */
#define ERR_SHARED_OBJECT_OP   31	/* op, error */

#define MSG_ERRNO		((char *)(-1))

