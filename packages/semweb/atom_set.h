/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

#ifndef ATOM_SET_H_INCLUDED
#define ATOM_SET_H_INCLUDED

typedef struct avl_node
{ struct avl_node *left;
  struct avl_node *right;
  atom_t key;
  int bal;
} avl_node;

#define FREE_CHUNK_SIZE 1024

typedef struct avl_free_list
{ struct avl_free_list *next;
  int left;
  avl_node nodes[FREE_CHUNK_SIZE];
} avl_free_list;

typedef struct avl_tree
{ avl_node      *root;
  long		 size;			/* # nodes in the tree */
  avl_free_list *free_list;
  avl_free_list  block1;
} avl_tree;


void avl_init(avl_tree *tree);
void avl_destroy(avl_tree *tree);
int  avl_insert(avl_tree *tree, atom_t key);

#endif /*ATOM_SET_H_INCLUDED*/
