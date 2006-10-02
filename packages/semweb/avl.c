/** <plaintext>
*
* avl.c -- C source file for avl trees. Contains the auxillary routines
*          and defines for the avl tree functions and user interface and
*          includes all the necessary public and private routines
*
* Created 03/01/89 by Brad Appleton
*
* ^{Mods:* }
*
* Fri Jul 14 13:53:42 1989, Rev 1.0, brad(0165)
*
_______________________________________________________________________________

                                   LICENSE
                                   =======

This software is not subject to any license  of  the  American  Telephone  and
Telegraph Company or of the Regents of the University of California.

Permission is granted to anyone to use this software for any  purpose  on  any
computer  system,  and  to alter it and redistribute it freely, subject to the
following restrictions:

  1.  Neither  the  authors  of  the  software  nor  their   employers
      (including  any of the employers' subsidiaries and subdivisions)
      are responsible for maintaining & supporting  this  software  or
      for any consequences resulting from the use of this software, no
      matter how awful, even if they arise from flaws in the  software
      (see LIABILITY).

  2.  The origin of this software must not be  misrepresented,  either
      by  explicit  claim  or  by omission.  Since few users ever read
      sources, credits must appear in the documentation.

  3.  Altered versions must be plainly marked as such, and must not be
      misrepresented  as being the original software.  Since few users
      ever read sources, credits must appear in the documentation.

  4.  This notice may not be removed or altered.
_______________________________________________________________________________

                                NO WARRANTY
                                ===========

Because this software is licensed free  of  charge,  we  (the  authors  and/or
distributors  of  this software) provide absolutely no warranty, to the extent
permitted by applicable state law.  Except when otherwise stated  in  writing,
the  authors  and  distributors  of this software and/or other parties provide
this program "as is"  without  warranty  of  any  kind,  either  expressed  or
implied,   including,   but   not   limited  to,  the  implied  warranties  of
merchantability and fitness for a particular purpose.  The entire risk  as  to
the  quality  and performance of this software lies with the recipients and/or
users of this software and not with any of the authors and/or distributors  of
this  software,  nor  with  any  of  their  employers,  including  any  of the
employers' subsidiaries and subdivisions. Should the program prove  defective,
the  recipients/users  of  this  software  assume  the  cost  of all necessary
servicing, repair, or correction.
_______________________________________________________________________________

                                 LIABILITY
                                 =========

In no event  unless  required  by  applicable  law  will  the  authors  and/or
distributors  of  this  software, their employers, and any subsidiaries and/or
subdivisions of their employers, and/or any other party who  may  redistribute
this software as permitted in the LICENSE section of this notice, be liable to
the recipients/users of this software for damages including any lost  profits,
lost  monies,  or  other special, incidental, or consequential damages arising
out of the use or inabilty to use (including but not limited to loss  of  data
or  data  being  rendered inaccurate or lossed sustained by third parties or a
failure of the software to operate with any other software or  hardware)  this
software, even if you have been advised of the possibility of such damages, or
for any claim by any other party.
_______________________________________________________________________________

Heavily  modified  by  Jan  Wielemaker,  wielemak@science.uva.nl.  Brief
summary:

	- Added prototypes and reformatted the sources.
	- Added includes
	- Swapped various arguments
	- Changes some of the interfaces
	- Added enumeration interface
**/

     /* some common #defines used throughout most of my files */
#define  PUBLIC			/* default */
#define  FALSE    0
#define  TRUE     !FALSE

     /* some defines for debugging purposes */
#ifdef NDEBUG
#define DBG(x)			/* x */
#else
#define DBG(x)          x
#endif

#define  NEXTERN		/* dont include "extern" declarations from header files */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "avl.h"		/* public types for avl trees */
#ifdef WIN32
#define inline __inline
#endif

/************************************************************************
*       Auxillary functions
*
*       routines to allocate/de-allocate an AVL node,
*       and determine the type of an AVL node.
************************************************************************/

/* ckalloc(size) -- allocate space; check for success */
static void *
ckalloc(int size)
{ void *ptr;

  if ( !(ptr = malloc((size_t) size)) )
  { fprintf(stderr, "Unable to allocate storage.");
    exit(1);
  }

  return ptr;
}				/* ckalloc */


#define sizeofnode(size) ((size_t)&((AVLtree)0)->data[0] + size)

/*
* new_node() -- get space for a new node and its data;
*               return the address of the new node
*/
static AVLtree
new_node(AVL_TREE tree, void *data)
{ AVLtree root;

  if ( tree->alloc )
    root = (*tree->alloc)(tree->client_data, sizeofnode(tree->isize));
  else
    root = ckalloc(sizeofnode(tree->isize));

  memcpy(root->data, data, tree->isize);
  root->bal = BALANCED;
  root->subtree[LEFT] = root->subtree[RIGHT] = NULL_TREE;

  return root;
}				/* new_node */


/*
* free_node()  --  free space for a node and its data!
*                  reset the node pointer to NULL
*/
static void
free_node(AVL_TREE tree, AVLtree *rootp)
{ AVLtree root = *rootp;

  if ( tree->destroy )
    (*tree->destroy)(root->data);

  if ( tree->free )
    (*tree->free)(tree->client_data, root, sizeofnode(tree->isize));
  else
    free(root);
  
  *rootp = NULL_TREE;
}				/* free_node */


/*
* node_type() -- determine the number of null pointers for a given
*                node in an AVL tree, Returns a value of type NODE
*                which is an enumeration type with the following values:
*
*                  IS_TREE     --  both subtrees are non-empty
*                  IS_LBRANCH  --  left subtree is non-empty; right is empty
*                  IS_RBRANCH  --  right subtree is non-empty; left is empty
*                  IS_LEAF     --  both subtrees are empty
*                  IS_NULL     --  given tree is empty
*/
static NODE
node_type(AVLtree tree)
{ if (tree == NULL_TREE)
  { return IS_NULL;
  } else if ((tree->subtree[LEFT] != NULL_TREE) &&
	     (tree->subtree[RIGHT] != NULL_TREE))
  { return IS_TREE;
  } else if (tree->subtree[LEFT] != NULL_TREE)
  { return IS_LBRANCH;
  } else if (tree->subtree[RIGHT] != NULL_TREE)
  { return IS_RBRANCH;
  } else
  { return IS_LEAF;
  }
}				/* node_type */



/************************************************************************
*       static functions for manipulating AVL trees
*
*  This following defines a set of routines for creating, maintaining, and
*  manipulating AVL Trees as an Abtract Data Type. The routines in this
*  file that are accessible (through the avl tree user-interface) to other
*  files to allow other programmers to:
*
*   Insert, Delete, and Find a given data item from a Tree.
*
*   Delete and Find the minimal and Maximal items in a Tree.
*
*   Walk through every node in a tree performing a giving operation.
*
*   Walk through and free up space for every node in a tree while performing
*   a given operation on the data items as they are encountered.
************************************************************************/



/************************************************************************
*       routines used to find the minimal and maximal elements
*       (nodes) of an AVL tree.
************************************************************************/

/*
* avl_min() -- compare function used to find the minimal element in a tree
*/
static int
avl_min(void *elt1, void *elt2, NODE nd_typ)
{ return (nd_typ == IS_RBRANCH || nd_typ == IS_LEAF)
	 ? 0	/* left subtree is empty -- this is the minimum */
	 : -1;			/* keep going left */
}				/* avl_min */


/*
* avl_max() -- compare function used to find the maximal element in a tree
*/
static int
avl_max(void *elt1, void *elt2, NODE nd_typ)
{ return (nd_typ == IS_LBRANCH || nd_typ == IS_LEAF)
	? 0	/* right subtree is empty -- this is the maximum */
	: 1;			/* keep going right */
}				/* avl_max */



/************************************************************************
*       Routines to perform rotations on AVL trees
************************************************************************/

/*
* rotate_once()  --  rotate a given node in the given direction
*                    to restore the balance of a tree
*/
static short
rotate_once(AVLtree * rootp, DIRECTION dir)
{ DIRECTION other_dir = OPPOSITE(dir);	/* opposite direction to "dir" */
  AVLtree old_root = *rootp;	/* copy of original root of tree */
  short ht_unchanged;		/* true if height unchanged */

  ht_unchanged = ((*rootp)->subtree[other_dir]->bal) ? FALSE : TRUE;

  /* assign new root */
  *rootp = old_root->subtree[other_dir];

  /* new-root exchanges it's "dir" subtree for it's parent */
  old_root->subtree[other_dir] = (*rootp)->subtree[dir];
  (*rootp)->subtree[dir] = old_root;

  /* update balances */
  old_root->bal = -(dir == LEFT ? --((*rootp)->bal) : ++((*rootp)->bal));

  return ht_unchanged;
}				/* rotate_once */


/*
* rotate_twice()  --  rotate a given node in the given direction
*                     and then in the opposite direction
*                     to restore the balance of a tree
*/
static void
rotate_twice(AVLtree * rootp, DIRECTION dir)
{ DIRECTION other_dir = OPPOSITE(dir);
  AVLtree old_root = *rootp;
  AVLtree old_other_dir_subtree = (*rootp)->subtree[other_dir];

  /* assign new root */
  *rootp = old_root->subtree[other_dir]->subtree[dir];

  /* new-root exchanges it's "dir" subtree for it's grandparent */
  old_root->subtree[other_dir] = (*rootp)->subtree[dir];
  (*rootp)->subtree[dir] = old_root;

  /* new-root exchanges it's "other-dir" subtree for it's parent */
  old_other_dir_subtree->subtree[dir] = (*rootp)->subtree[other_dir];
  (*rootp)->subtree[other_dir] = old_other_dir_subtree;

  /* update balances */
  (*rootp)->subtree[LEFT]->bal = -MAX((*rootp)->bal, 0);
  (*rootp)->subtree[RIGHT]->bal = -MIN((*rootp)->bal, 0);
  (*rootp)->bal = 0;

}				/* rotate_twice */


/************************************************************************
*                       Rebalance an AVL tree
************************************************************************/

/*
* balance()  --  determines and performs the  sequence of rotations needed
*                   (if any) to restore the balance of a given tree.
*
*     Returns 1 if tree height changed due to rotation; 0 otherwise
*/
static short
balance(AVLtree * rootp)
{ short special_case = FALSE;

  if (LEFT_IMBALANCE(*rootp))
  {				/* need a right rotation */
    if ((*rootp)->subtree[LEFT]->bal == RIGHT_HEAVY)
    { rotate_twice(rootp, RIGHT);	/* double RL rotation needed */
    } else
    {				/* single RR rotation needed */
      special_case = rotate_once(rootp, RIGHT);
    }
  } else if (RIGHT_IMBALANCE(*rootp))
  {				/* need a left rotation */
    if ((*rootp)->subtree[RIGHT]->bal == LEFT_HEAVY)
    { rotate_twice(rootp, LEFT);	/* double LR rotation needed */
    } else
    {				/* single LL rotation needed */
      special_case = rotate_once(rootp, LEFT);
    }
  } else
  { return HEIGHT_UNCHANGED;	/* no rotation occurred */
  }

  return (special_case) ? HEIGHT_UNCHANGED : HEIGHT_CHANGED;
}				/* balance */


/************************************************************************
*       Routines to:    Find an item in an AVL tree
*                       Insert an item into an AVL tree
*                       Delete an item from an AVL tree
************************************************************************/

/*
* avl_find() -- find an item in the given tree
*
*   PARAMETERS:
*                data       --  a pointer to the key to find
*                rootp      --  a pointer to an AVL tree
*                compar     --  name of a function to compare 2 data items
*/
static void *
avl_find(void *data, AVLtree tree, int (*compar) (void *l, void *r, NODE))
{ NODE nd_typ = node_type(tree);
  int cmp;

  while ((tree != NULL_TREE) &&
	 (cmp = (*compar)(data, tree->data, nd_typ)))
  { tree = tree->subtree[(cmp < 0) ? LEFT : RIGHT];
  }

  return (tree == NULL_TREE) ? NULL : tree->data;
}				/* avl_find */


/*
* avl_insert() -- insert an item into the given tree
*
*   PARAMETERS:
*                data       --  a pointer to a pointer to the data to add;
*                               On exit, *data is NULL if insertion succeeded,
*                               otherwise address of the duplicate key
*                rootp      --  a pointer to an AVL tree
*                compar     --  name of the function to compare 2 data items
*/
static short
avl_insert(AVL_TREE tree, AVLtree *rootp, void **data)
{ short increase;
  int cmp;

  if (*rootp == NULL_TREE)
  {				/* insert new node here */
    *rootp = new_node(tree, *data);
    *data = NULL;		/* set return value in data */
    return HEIGHT_CHANGED;
  }

  cmp = (*tree->compar)(*data, (*rootp)->data, IS_NULL);

  if (cmp < 0)
  {				/* insert into the left subtree */
    increase = -avl_insert(tree, &((*rootp)->subtree[LEFT]), data);
    if (*data != NULL)
      return HEIGHT_UNCHANGED;
  } else if (cmp > 0)
  {				/* insert into the right subtree */
    increase = avl_insert(tree, &((*rootp)->subtree[RIGHT]), data);
    if (*data != NULL)
      return HEIGHT_UNCHANGED;
  } else
  {				/* data already exists */
    *data = (*rootp)->data;	/* set return value in data */
    return HEIGHT_UNCHANGED;
  }

  (*rootp)->bal += increase;	/* update balance factor */

  /************************************************************************
  * re-balance if needed -- height of current tree increases only if its
  * subtree height increases and the current tree needs no rotation.
  ************************************************************************/
  return (increase && (*rootp)->bal)
	? (1 - balance(rootp))
	: HEIGHT_UNCHANGED;
}				/* avl_insert */


static void
memswap(void *p1, void *p2, size_t size)
{ char *s1 = p1;
  char *s2 = p2;
  char buf[256];

  while(size > 0)
  { int bytes = (size > sizeof(buf) ? sizeof(buf) : size);

    memcpy(buf, s1, bytes);
    memcpy(s1, s2, bytes);
    memcpy(s2, buf, bytes);

    s1 += bytes;
    s2 += bytes;
    size -= bytes;
  }
}


/*
* avl_delete() -- delete an item from the given tree
*
*   PARAMETERS:
*                data       --  a pointer to a pointer to the key to delete
*                               On exit, *data points to the deleted data item
*                               (or NULL if deletion failed).
*                rootp      --  a pointer to an AVL tree
*                compar     --  name of function to compare 2 data items
*/
static short
avl_delete(AVL_TREE tree, AVLtree *rootp, void *data, int *found,
	   int (*compar)(void*, void*, NODE))
{ short decrease;
  int cmp;
  AVLtree old_root = *rootp;
  NODE nd_typ = node_type(*rootp);
  DIRECTION dir = (nd_typ == IS_LBRANCH) ? LEFT : RIGHT;

  if (*rootp == NULL_TREE)
  { 				/* data not found */
    if ( found )
      *found = FALSE;
    return HEIGHT_UNCHANGED;
  }

  cmp = (*compar)(data, (*rootp)->data, nd_typ);

  if (cmp < 0)
  {				/* delete from left subtree */
    decrease = -avl_delete(tree, &((*rootp)->subtree[LEFT]), data, found, compar);
    if ( found && *found == FALSE )
      return HEIGHT_UNCHANGED;
  } else if (cmp > 0)
  {				/* delete from right subtree */
    decrease = avl_delete(tree, &((*rootp)->subtree[RIGHT]), data, found, compar);
    if ( found && *found == FALSE )
      return HEIGHT_UNCHANGED;
  } else
  {				/* cmp == 0 */
    decrease = 0;		/* JW: Silence compiler */
    if ( found )
      *found = TRUE;
    if ( data && data != (*rootp)->data )
    { if ( found )
	memcpy(data, (*rootp)->data, tree->isize); 
      else
	memswap(data, (*rootp)->data, tree->isize);
    }

      /***********************************************************************
      *  At this point we know "cmp" is zero and "*rootp" points to
      *  the node that we need to delete.  There are three cases:
      *
      *     1) The node is a leaf.  Remove it and return.
      *
      *     2) The node is a branch (has only 1 child). Make "*rootp"
      *        (the pointer to this node) point to the child.
      *
      *     3) The node has two children. We swap data with the successor of
      *        "*rootp" (the smallest item in its right subtree) and delete
      *        the successor from the right subtree of "*rootp".  The
      *        identifier "decrease" should be reset if the subtree height
      *        decreased due to the deletion of the successor of "rootp".
      ***********************************************************************/

    switch (nd_typ)
    {				/* what kind of node are we removing? */
      case IS_LEAF:
	free_node(tree, rootp);	/* free the leaf, its height     */
	return HEIGHT_CHANGED;	/* changes from 1 to 0, return 1 */

      case IS_RBRANCH:		/* only child becomes new root */
      case IS_LBRANCH:
	*rootp = (*rootp)->subtree[dir];
	free_node(tree, &old_root);	/* free the deleted node */
	return HEIGHT_CHANGED;	/* we just shortened the "dir" subtree */

      case IS_TREE:
	decrease = avl_delete(tree, 
			      &((*rootp)->subtree[RIGHT]),
			      (*rootp)->data, NULL,
			      avl_min);
        break;
      case IS_NULL:		/* JW: Silence compiler.  Cannot happen */
	assert(0);
	break;
    }				/* switch */
  }				/* else */

  (*rootp)->bal -= decrease;	/* update balance factor */

  /**********************************************************************
  * Rebalance if necessary -- the height of current tree changes if one
  * of two things happens: (1) a rotation was performed which changed
  * the height of the subtree (2) the subtree height decreased and now
  * matches the height of its other subtree (so the current tree now
  * has a zero balance when it previously did not).
  **********************************************************************/
  if (decrease && (*rootp)->bal)
  { return balance(rootp);	/* rebalance and see if height changed */
  } else if (decrease && !(*rootp)->bal)
  { return HEIGHT_CHANGED;	/* balanced because subtree decreased */
  } else
  { return HEIGHT_UNCHANGED;
  }
}				/* avl_delete */



/**
*       Routines which Recursively Traverse an AVL TREE
*
* These routines may perform a particular action function upon each node
* encountered. In these cases, "action" has the following definition:
*
*   void action(data, order, node, level, bal)
*       void   *data
*       VISIT   order;
*       NODE    node;
*       short   bal;
*       int     level;
*
*         "data"    is a pointer to the data field of an AVL node
*         "order"   corresponds to whether this is the 1st, 2nd or 3rd time
*                   that this node has been visited.
*         "node"    indicates which children (if any) of the current node
*                   are null.
*         "level"   is the current level (or depth) in the tree of the
*                   curent node.
*         "bal"     is the balance factor of the current node.
**/


/************************************************************************
*       Walk an AVL tree, performing a given function at each node
************************************************************************/


/*
* avl_walk -- traverse the given tree performing "action"
*            upon each data item encountered.
*
*/
static void
avl_walk(AVLtree tree, void (*action)(void *data,
				      SIBLING_ORDER order, 
				      NODE type,
				      int level,
				      int balance),
	 SIBLING_ORDER sibling_order, int level)
{ DIRECTION dir1 = (sibling_order == LEFT_TO_RIGHT) ? LEFT : RIGHT;
  DIRECTION dir2 = OPPOSITE(dir1);
  NODE node = node_type(tree);

  if ( tree && action )
  { (*action) (tree->data, PREORDER, node, level, (int)tree->bal);

    if (tree->subtree[dir1] != NULL_TREE)
    { avl_walk(tree->subtree[dir1], action, sibling_order, level + 1);
    }

    (*action) (tree->data, INORDER, node, level, (int)tree->bal);

    if (tree->subtree[dir2] != NULL_TREE)
    { avl_walk(tree->subtree[dir2], action, sibling_order, level + 1);
    }

    (*action) (tree->data, POSTORDER, node, level, (int)tree->bal);
  }
  /* if non-empty tree */
}				/* avl_walk */


/*
* avl_free() -- free up space for all nodes in a given tree
*/
static void
avl_free(AVL_TREE tree, AVLtree *rootp)
{ if ( *rootp )
  { if ( (*rootp)->subtree[LEFT] != NULL_TREE )
      avl_free(tree, &(*rootp)->subtree[LEFT]);

    if ( (*rootp)->subtree[RIGHT] != NULL_TREE )
      avl_free(tree, &(*rootp)->subtree[RIGHT]);

    free_node(tree, rootp);
  }
}


		 /*******************************
		 *    ENUMERATION INTERFACE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
avlfindfirst()
avlfindnext()
avlfinddestroy()

This interface allows for  enumerating  all   elements  in  a key-range.
avl_find_ge() finds the first node whose  key   is  larger or equal to a
given  key.  avlfindnext()  returns  the    next  node.  avlfinddestroy()
destroyes memory allocated for the enum (if any).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static inline AVLtree
push_node(avl_enum *e, AVLtree node)
{ e->parents[e->current++] = node;

  return node;
}


static inline AVLtree
pop_node(avl_enum *e)
{ if ( --e->current >= 0 )
    return e->parents[e->current];

  return NULL;
}


static inline AVLtree
current_node(avl_enum *e)
{ if ( e->current >= 1 )
    return e->parents[e->current-1];

  return NULL;
}


void *
avlfindfirst(AVL_TREE tree, void *data, avl_enum *e)
{ AVLtree node = tree->root;

  if ( !node )
    return NULL;

  e->tree    = tree;
  e->current = 0;
  
  for(;;)
  { int diff = (*tree->compar)(data, node->data, IS_NULL);

    if ( diff < 0 )
    { push_node(e, node);
	
      if ( node->subtree[LEFT] )
      { node = node->subtree[LEFT];
      } else
      { return node->data;		/* key > target */
      }
    } else if ( diff > 0 )
    { if ( node->subtree[RIGHT] )
      { node = node->subtree[RIGHT];
      } else
      { if ( (node = current_node(e)) )
	  return node->data;

	return NULL;
      }
    } else
    { return push_node(e, node)->data;	/* equal hit */
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Last pushed node is  the  node  returned.   All  nodes  to  the left are
considered `done'. We must return all nodes   to  the right, followed by
the parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void *
avlfindnext(avl_enum *e)
{ AVLtree n = pop_node(e);

  if ( n->subtree[RIGHT] )
  { n = push_node(e, n->subtree[RIGHT]);
    while(n->subtree[LEFT])
      n = push_node(e, n->subtree[LEFT]);
    return n->data;
  }

  n = current_node(e);
  
  return n ? n->data : NULL;
}


void
avlfinddestroy(avl_enum *e)
{
}


/**********************************************************************
*
*               C-interface (public functions) for avl trees
*
*       These are the functions that are visible to the user of the
*       AVL Tree Library. Mostly they just return or modify a
*       particular attribute, or Call a private functions with the
*       given parameters.
*
*       Note that public routine names begin with "avl" whereas
*       private routine names that are called by public routines
*       begin with "avl_" (the underscore character is added).
*
*       Each public routine must convert (cast) any argument of the
*       public type "AVL_TREE" to a pointer to on object of the
*       private type "AVLdescriptor" before passing the actual
*       AVL tree to any of the private routines. In this way, the
*       type "AVL_TREE" is implemented as an opaque type.
*
*       An "AVLdescriptor" is merely a container for AVL-tree
*       objects which contains the pointer to the root of the
*       tree and the various attributes of the tree.
*
*       The function types prototypes for the routines which follow
*       are declared in the include file "avl.h"
*
***********************************************************************/



/*
* avlinit() -- get space for an AVL descriptor for the given tree
*              structure and initialize its fields.
*/
PUBLIC AVL_TREE
avlinit(AVL_TREE tree,
	void *cdata, size_t isize,
	int (*compare)(void *l, void*r, NODE type),
	void (*destroy)(void *data),
	void* (*alloc)(void *cdata, size_t bytes),
	void (*free)(void *cdata, void *data, size_t bytes))
{ tree->root = NULL_TREE;
  tree->compar = compare;
  tree->destroy = destroy;
  tree->alloc = alloc;
  tree->free = free;
  tree->isize = isize;
  tree->client_data = cdata;
  tree->count = 0;

  return (AVL_TREE) tree;
}				/* avlinit */



/*
* avldispose() -- free up all space associated with the given tree structure.
*/
PUBLIC void
avlfree(AVL_TREE tree)
{ avl_free(tree, &(tree->root));
}				/* avldispose */



/*
* avlwalk() -- traverse the given tree structure and perform the
*              given action on each data item in the tree.
*/
PUBLIC void
avlwalk(AVL_TREE tree,
	void (*action)(void *data,
		       SIBLING_ORDER order, 
		       NODE type,
		       int level,
		       int balance),
	SIBLING_ORDER sibling_order)
{ avl_walk(tree->root, action, sibling_order, 1);
}				/* avlwalk */



/*
* avlcount() --  return the number of nodes in the given tree
*/
PUBLIC int
avlcount(AVL_TREE tree)
{ return tree->count;
}				/* avlcount */



/*
* avlins() -- insert the given item into the tree structure
*/
PUBLIC void *
avlins(AVL_TREE tree, void *data)
{ avl_insert(tree, &tree->root, &data);
  if ( data == NULL )
    tree->count++;

  return data;
}				/* avlins */



/*
* avldel() -- delete the given item from the given tree structure
*/
PUBLIC int
avldel(AVL_TREE tree, void *data)
{ int found;

  avl_delete(tree, &tree->root, data, &found, tree->compar);
  if ( found )
    tree->count--;

  return found;
}


/*
* avlfind() -- find the given item in the given tree structure
*              and return its address (NULL if not found).
*/
PUBLIC void *
avlfind(AVL_TREE tree, void *data)
{ return avl_find(data, tree->root, tree->compar);
}				/* avlfind */



/*
* avldelmin() -- delete the minimal item from the given tree structure
*/
PUBLIC int
avldelmin(AVL_TREE tree, void *data)
{ int found;

  avl_delete(tree, &tree->root, data, &found, avl_min);
  if ( found )
    tree->count--;

  return found;
}				/* avldelmin */



/*
* avlfindmin() -- find the minimal item in the given tree structure
*              and return its address (NULL if not found).
*/
PUBLIC void *
avlfindmin(AVL_TREE tree)
{ return avl_find(NULL, tree->root, avl_min);
}				/* avlfindmin */



/*
* avldelmax() -- delete the maximal item from the given tree structure
*/
PUBLIC int
avldelmax(AVL_TREE tree, void *data)
{ int found;

  avl_delete(tree, &tree->root, data, &found, avl_max);
  if ( found )
    tree->count--;

  return found;
}				/* avldelmax */



/*
* avlfindmax() -- find the maximal item in the given tree structure
*              and return its address (NULL if not found).
*/
PUBLIC void *
avlfindmax(AVL_TREE tree)
{ return avl_find(NULL, tree->root, avl_max);
}				/* avlfindmax */
