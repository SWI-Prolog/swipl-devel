/** <plaintext>
*
*  avl.h -- public types and external declarations for avl trees
*
*  Created 03/01/89 by Brad Appleton
*
* ^{Mods:* }
*
* Fri Jul 14 13:54:12 1989, Rev 1.0, brad(0165)
*
**/

#ifndef AVL_H_INCLUDED
#define AVL_H_INCLUDED

       /* definition of traversal type */
typedef  enum  { PREORDER, INORDER, POSTORDER }  VISIT;


       /* definition of sibling order type */
typedef  enum  { LEFT_TO_RIGHT, RIGHT_TO_LEFT }  SIBLING_ORDER;


       /* definition of node type */
typedef  enum  { IS_TREE, IS_LBRANCH, IS_RBRANCH, IS_LEAF, IS_NULL }  NODE;

  /* definition of a NULL action and a NULL tree */
#define NULL_ACTION    ((void(*)()) NULL)
#define NULL_TREE      ((AVLtree) NULL)


        /* MIN and MAX macros (used for rebalancing) */
#define  MIN(a,b)    ((a) < (b) ? (a) : (b))
#define  MAX(a,b)    ((a) > (b) ? (a) : (b))


/* BEGIN Internal definitions */
       /* Directional Definitions */
typedef  short  DIRECTION;
#define  LEFT             0
#define  RIGHT            1
#define  OPPOSITE(x)     (1 - (x))


       /* return codes used by avl_insert(), avl_delete(), and balance() */
#define  HEIGHT_UNCHANGED       0
#define  HEIGHT_CHANGED         1


       /* Balance Definitions */
#define  LEFT_HEAVY            -1
#define  BALANCED               0
#define  RIGHT_HEAVY            1
#define  LEFT_IMBALANCE(nd)    ((nd)->bal < LEFT_HEAVY)
#define  RIGHT_IMBALANCE(nd)   ((nd)->bal > RIGHT_HEAVY)


  /* structure for a node in an AVL tree */
typedef struct avl_node
{ struct avl_node  *subtree[2];		/* LEFT and RIGHT subtrees */
  short       bal;			/* balance factor */
  void	      *data;			/* data on my back */
} AVLnode, *AVLtree;

/* End Internal definitions */

  /* structure which holds information about an AVL tree */
typedef struct avl_tree
{ AVLtree     root;           /* pointer to the root node of the tree */
  long        count;          /* number of nodes in the tree */
			      /* function used to compare keys */
  int         (*compar)(void *l, void*r, NODE type); 
  void        (*destroy)(void *data, VISIT v, NODE type, int level); 
  void*	      (*alloc)(size_t size);
  void	      (*free)(void* data, size_t size);
  int	      isize;	      /* item data size */
} avl_tree, *AVL_TREE;

#define AVL_ENUM_MAX 32			/* balanced tree, allows for 2**32 */
					/* nodes */

typedef struct avl_enum
{ AVL_TREE tree;
  int current;
  AVLtree parents[AVL_ENUM_MAX];
} avl_enum;

void *avlfindfirst(AVL_TREE tree, void *key, avl_enum *e);
void *avlfindnext(avl_enum *e);
void  avlfinddestroy(avl_enum *e);

     /* Constructor and Destructor functions for AVL trees:
     *          avlfree is a macro for avldispose in the fashion
     *          of free(). It assumes certain default values
     *          (shown below) for the deallocation function and
     *          for the order in which children are traversed.
     */
extern AVL_TREE     avlinit(AVL_TREE tree,
			    size_t isize,
			    int (*compare)(void *l, void*r, NODE type),
			    void (*destroy)(void *d, VISIT v, NODE t, int l),
			    void* (*alloc)(size_t bytes),
			    void (*free)(void *data, size_t bytes));
extern void         avldispose(AVL_TREE tree, SIBLING_ORDER);
#define avlfree(x)  avldispose((x), LEFT_TO_RIGHT)


       /* Routine for manipulating/accessing each data item in a tree */
extern void      avlwalk(AVL_TREE, void(*) (), SIBLING_ORDER);


       /* Routine for obtaining the size of an AVL tree */
extern int       avlcount(AVL_TREE);


       /* Routines to search for a given item */
extern void    *avlins(AVL_TREE tree, void *data);
extern int	avldel(AVL_TREE tree, void *data);
extern void    *avlfind(AVL_TREE tree, void *data);


       /* Routines to search for the minimal item of a tree */
extern void     *avldelmin(AVL_TREE tree);
extern void     *avlfindmin(AVL_TREE tree);


       /* Routines to search for the maximal item of a tree */
extern void     *avldelmax(AVL_TREE tree);
extern void     *avlfindmax(AVL_TREE tree);

#endif /* AVL_H_INCLUDED */
