/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef MODEL_H_INCLUDED
#define MODEL_H_INCLUDED

#define MAXOMITTED 32

#define CDATA_ELEMENT	((dtd_element *)1)

typedef struct _dtd_state
{ struct _state_transition *transitions;
  struct _state_expander *expander;
} dtd_state;

dtd_state      *new_dtd_state(void);
dtd_state *	make_dtd_transition(dtd_state *here, dtd_element *e);
int		same_state(dtd_state *final, dtd_state *here);
int 		find_omitted_path(dtd_state *state, dtd_element *e,
				  dtd_element **path);
dtd_state *	make_state_engine(dtd_element *e);
void		free_state_engine(dtd_state *state);
void		state_allows_for(dtd_state *state,
				 dtd_element **allow, int *n);

#endif /*MODEL_H_INCLUDED*/
