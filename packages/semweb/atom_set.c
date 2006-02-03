#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include "atom_set.h"
#define AVL_DELETE 1

#define AVL_LEFT -1
#define AVL_NONE 0
#define AVL_RIGHT 1

static avl_node	*avl_new_node(avl_tree *tree);

static int
avl_compare(avl_tree *tree, void *v1, void *v2)
{ if ( tree->compare )
  { return (*tree->compare)(v1, v2);
  } else
  { char *k1 = v1;
    char *k2 = v2;
    
    return ((k1) < (k2) ? -1 : (k1) > (k2) ? 1 : 0);
  }
}

static void
avl_rotate_left(avl_node **n)
{ avl_node *tmp = *n;

  *n = (*n)->right;
  tmp->right = (*n)->left;
  (*n)->left = tmp;
}

static void
avl_rotate_right(avl_node **n)
{ avl_node *tmp = *n;

  *n = (*n)->left;
  tmp->left = (*n)->right;
  (*n)->right = tmp;
}

static int
avl_leftgrown(avl_node **n)
{ switch((*n)->bal)
  { case AVL_LEFT:
      if ( (*n)->left->bal == AVL_LEFT )
      { (*n)->bal = (*n)->left->bal = AVL_NONE;

        avl_rotate_right(n);
      } else
      { switch( (*n)->left->right->bal )
	{ case AVL_LEFT:
	    (*n)->bal       = AVL_RIGHT;
            (*n)->left->bal = AVL_NONE;
            break;

          case AVL_RIGHT:
            (*n)->bal       = AVL_NONE;
            (*n)->left->bal = AVL_LEFT;
            break;

          default:
            (*n)->bal       = AVL_NONE;
            (*n)->left->bal = AVL_NONE;
        }

        (*n)->left->right->bal = AVL_NONE;

        avl_rotate_left(&(*n)->left);
        avl_rotate_right(n);
      }

      return 0;

    case AVL_RIGHT:
      (*n)->bal = AVL_NONE;
      return 0;

    default:
      (*n)->bal = AVL_LEFT;
      return 1;
  }
}

static int
avl_rightgrown(avl_node **n)
{ switch((*n)->bal)
  { case AVL_LEFT:
      (*n)->bal = AVL_NONE;
      return 0;

    case AVL_RIGHT:
      if((*n)->right->bal == AVL_RIGHT)
      { (*n)->bal = (*n)->right->bal = AVL_NONE;
        avl_rotate_left(n);
      } else
      { switch((*n)->right->left->bal)
	{ case AVL_RIGHT:
	    (*n)->bal = AVL_LEFT;
            (*n)->right->bal = AVL_NONE;
            break;

          case AVL_LEFT:
            (*n)->bal = AVL_NONE;
            (*n)->right->bal = AVL_RIGHT;
            break;

          default:
            (*n)->bal = AVL_NONE;
            (*n)->right->bal = AVL_NONE;
        }

        (*n)->right->left->bal = AVL_NONE;
        avl_rotate_right(& (*n)->right);
        avl_rotate_left(n);
      }

      return 0;

    default:
      (*n)->bal = AVL_RIGHT;
      return 1;
  }
}


static int
avl_insert_node(avl_tree *tree, avl_node **n, void * key)
{ int tmp;
  int dif;

  if ( !(*n) )
  { *n = avl_new_node(tree);
    (*n)->key = key;
    (*n)->bal = 0;
    tree->size++;

    return 1;				/* added a node */
  }

  dif = avl_compare(tree, key, (*n)->key);
  if ( dif < 0)
  { if ( (tmp = avl_insert_node(tree, &(*n)->left, key)) == 1 )
    { return avl_leftgrown(n);
    }

    return tmp;
  } else if ( dif > 0)
  { if ( (tmp = avl_insert_node(tree, &(*n)->right, key)) == 1 )
    { return avl_rightgrown(n);
    }

    return tmp;
  }

  return -1;				/* already in tree */
}


static avl_node *
avl_find_n(avl_tree *tree, avl_node *n, void * key)
{ while(n)
  { int dif = avl_compare(tree, key, n->key);

    if ( dif < 0)
    { n = n->left;
    } else if ( dif > 0 )
    { n = n->right;
    } else
    { return n;
    }
  }

  return NULL;
}


int
avl_insert(avl_tree *tree, void * key, avl_node **node)
{ int rc;

  rc = avl_insert_node(tree, &tree->root, key);
  if ( node )
    *node = avl_find_n(tree, tree->root, key);

  return rc;
}


avl_node *
avl_find_node(avl_tree *tree, void * key)
{ return avl_find_n(tree, tree->root, key);
} 


#ifdef AVL_DELETE

static void	avl_free_node(avl_tree *tree, avl_node *node);

static int
avl_leftshrunk(avl_node **n)
{ switch((*n)->bal)
  { case AVL_LEFT:
    (*n)->bal = AVL_NONE;
    
    return 1;

    case AVL_RIGHT:
      if((*n)->right->bal == AVL_RIGHT)
      { (*n)->bal = (*n)->right->bal = AVL_NONE;
        avl_rotate_left(n);

        return 1;
      } else if((*n)->right->bal == AVL_NONE)
      { (*n)->bal = AVL_RIGHT;
        (*n)->right->bal = AVL_LEFT;
        avl_rotate_left(n);

        return 0;
      } else
      { switch((*n)->right->left->bal)
	{ case AVL_LEFT:
            (*n)->bal = AVL_NONE;
            (*n)->right->bal = AVL_RIGHT;
            break;

          case AVL_RIGHT:
            (*n)->bal = AVL_LEFT;
            (*n)->right->bal = AVL_NONE;
            break;

          default:
            (*n)->bal = AVL_NONE;
            (*n)->right->bal = AVL_NONE;
        }

        (*n)->right->left->bal = AVL_NONE;
        avl_rotate_right(&(*n)->right);
        avl_rotate_left(n);
        return 1;
      }

    default:
      (*n)->bal = AVL_RIGHT;
      return 0;
  }
}

static int
avl_rightshrunk(avl_node **n)
{ switch((*n)->bal)
  { case AVL_RIGHT:
      (*n)->bal = AVL_NONE;
      return 1;

    case AVL_LEFT:
      if((*n)->left->bal == AVL_LEFT)
      { (*n)->bal = (*n)->left->bal = AVL_NONE;
        avl_rotate_right(n);

        return 1;
      } else if((*n)->left->bal == AVL_NONE)
      { (*n)->bal = AVL_LEFT;
        (*n)->left->bal = AVL_RIGHT;
        avl_rotate_right(n);

        return 0;
      } else
      { switch((*n)->left->right->bal)
	{ case AVL_LEFT:
            (*n)->bal = AVL_RIGHT;
            (*n)->left->bal = AVL_NONE;
            break;

          case AVL_RIGHT:
            (*n)->bal = AVL_NONE;
            (*n)->left->bal = AVL_LEFT;
            break;

          default:
            (*n)->bal = AVL_NONE;
            (*n)->left->bal = AVL_NONE;
        }

        (*n)->left->right->bal = AVL_NONE;

        avl_rotate_left(&(*n)->left);
        avl_rotate_right(n);

        return 1;
      }

    default:
      (*n)->bal = AVL_LEFT;
      return 0;
  }
}

static int
avl_findhighest(avl_tree *tree, avl_node *target,avl_node **n,int *res)
{ avl_node *tmp;

  *res = 1;
  if ( !(*n) )
  { return 0;
  }

  if ( (*n)->right)
  { if ( !avl_findhighest(tree, target, &(*n)->right, res) )
    { return 0;
    }
    if(*res == 1)
    { *res = avl_rightshrunk(n);
    }

    return 1;
  }

  target->key = (*n)->key;
  tmp = *n;
  *n = (*n)->left;
  avl_free_node(tree, tmp);

  return 1;
}

static int
avl_findlowest(avl_tree *tree, avl_node *target, avl_node **n, int *res)
{ avl_node *tmp;

  *res = 1;
  if ( !(*n) )
    return 0;

  if ( (*n)->left )
  { if ( !avl_findlowest(tree, target, &(*n)->left,res) )
    { return 0;
    }
    if ( *res == 1 )
    { *res = avl_leftshrunk(n);
    }

    return 1;
  }

  target->key = (*n)->key;
  tmp = *n;
  *n = (*n)->right;
  avl_free_node(tree, tmp);

  return 1;
}


int
avl_delete(avl_tree *tree, avl_node **n, void *key)
{ int tmp = 1;
  int dif;

  if ( !n )
    n = &tree->root;

  if ( !(*n) )
    return -1;				/* not found */

  dif = avl_compare(tree, key, (*n)->key);
  if ( dif < 0)
  { if ( (tmp = avl_delete(tree, &(*n)->left, key)) == 1 )
    { return avl_leftshrunk(n);
    }

    return tmp;
  } else if ( dif > 0 )
  { if ( (tmp = avl_delete(tree,&(*n)->right,key)) == 1 )
    { return avl_rightshrunk(n);
    }

    return tmp;
  }

  if ( (*n)->left )
  { if ( avl_findhighest(tree, *n, &((*n)->left), &tmp) )
    { if ( tmp == 1 )
      { tmp = avl_leftshrunk(n);
      }

      return tmp;
    }
  }
  if ( (*n)->right )
  { if ( avl_findlowest(tree, *n, &((*n)->right), &tmp))
    { if ( tmp == 1 )
      { tmp = avl_rightshrunk(n);
      }

      return tmp;
    }
  }

  tree->size--;
  if ( tree->destroy_node )
    (*tree->destroy_node)(*n);
  *n = NULL;

  return 1;
}

static void
avl_free_node(avl_tree *tree, avl_node *node)
{ /*TBD: make free-list*/
}

#endif /*AVL_DELETE*/

static avl_node *
avl_new_node(avl_tree *tree)
{ avl_free_list *l;
  avl_node *n;

  l = tree->free_list;
  if ( l->left == 0 )
  { l = PL_malloc(sizeof(*l));

    l->left = FREE_CHUNK_SIZE;
    l->next = tree->free_list;
    tree->free_list = l;
  }

  n = &l->nodes[--l->left];
  memset(n, 0, sizeof(*n));

  return n;
}


void
avl_init(avl_tree *tree)
{ tree->magic       = AVL_MAGIC;
  tree->root        = NULL;
  tree->size        = 0L;
  tree->destroy_node= NULL;
  tree->compare     = NULL;
  tree->block1.next = NULL;
  tree->block1.left = FREE_CHUNK_SIZE;
  tree->free_list   = &tree->block1;
}


static void
avl_destroy_nodes(avl_node *node, void (*destroy_node)(avl_node *node))
{ if ( node->left )
    avl_destroy_nodes(node->left, destroy_node);
  if ( node->right )
    avl_destroy_nodes(node->right, destroy_node);

  (*destroy_node)(node);
}


void
avl_destroy(avl_tree *tree)
{ avl_free_list *l, *n;

  if ( tree->destroy_node && tree->root )
    avl_destroy_nodes(tree->root, tree->destroy_node);

  for(l=tree->free_list; l != &tree->block1; l=n)
  { n = l->next;
    PL_free(l);
  }

  tree->magic = 0;
}


