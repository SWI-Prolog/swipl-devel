#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include "atom_set.h"

#define AVL_LEFT -1
#define AVL_NONE 0
#define AVL_RIGHT 1

#define COMPARE(tree, v1, v2) \
	((v1) < (v2) ? -1 : (v1) > (v2) ? 1 : 0)

static avl_node	*avl_new_node(avl_tree *tree);

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
avl_insert_node(avl_tree *tree, avl_node **n, atom_t key)
{ int tmp;
  int dif;

  if ( !n )
    n = &tree->root;

  if ( !(*n) )
  { *n = avl_new_node(tree);
    (*n)->key = key;
    (*n)->bal = 0;
    tree->size++;

    return 1;				/* added a node */
  }

  dif = COMPARE(tree, key, (*n)->key);
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


int
avl_insert(avl_tree *tree, atom_t key)
{ return avl_insert_node(tree, &tree->root, key);
}


#ifdef AVL_DELETE

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
avl_findhighest(avl_node *target,avl_node **n,int *res)
{ avl_node *tmp;

  *res = 1;
  if ( !(*n) )
  { return 0;
  }

  if ( (*n)->right)
  { if ( !avl_findhighest(target,&(*n)->right,res) )
    { return 0;
    }
    if(*res == 1)
    { *res = avl_rightshrunk(n);
    }

    return 1;
  }

  target->d = (*n)->d;
  tmp = *n;
  *n = (*n)->left;
  free(tmp);

  return 1;
}

static int
avl_findlowest(avl_node *target, avl_node **n, int *res)
{ avl_node *tmp;

  *res = 1;
  if ( !(*n) )
    return 0;

  if ( (*n)->left )
  { if ( !avl_findlowest(target,&(*n)->left,res) )
    { return 0;
    }
    if ( *res == 1 )
    { *res = avl_leftshrunk(n);
    }

    return 1;
  }

  target->d = (*n)->d;
  tmp = *n;
  *n = (*n)->right;
  free(tmp);

  return 1;
}

int
avl_delete(avl_tree *tree, avl_node **n, avl_dataset *key)
{ int tmp = 1;
  int dif;

  if ( !n )
    n = &tree->root;

  if ( !(*n) )
    return -1;				/* not found */

  dif = COMPARE(tree, key, (*n)->d);
  if ( dif < 0)
  { if ( (tmp = avl_remove(tree, &(*n)->left, key)) == 1 )
    { return avl_leftshrunk(n);
    }

    return tmp;
  } else if ( dif > 0 )
  { if ( (tmp = avl_remove(tree,&(*n)->right,key)) == 1 )
    { return avl_rightshrunk(n);
    }

    return tmp;
  }

  if ( (*n)->left )
  { if ( avl_findhighest(*n, &((*n)->left), &tmp) )
    { if ( tmp == 1 )
      { tmp = avl_leftshrunk(n);
      }

      return tmp;
    }
  }
  if ( (*n)->right )
  { if ( avl_findlowest(*n, &((*n)->right), &tmp))
    { if ( tmp == 1 )
      { tmp = avl_rightshrunk(n);
      }

      return tmp;
    }
  }

  tree->size--;
  *n = NULL;

  return 1;
}

#endif /*AVL_DELETE*/

const avl_node *
avl_find(avl_tree *tree, avl_node *n, atom_t key)
{ int dif;

  if ( !n )
    return NULL;

  dif = COMPARE(tree, key, n->key);
  if ( dif < 0 )
  { return avl_find(tree, n->left, key);
  } else if ( dif > 0)
  { return avl_find(tree, n->right, key);
  }

  return n;
}


static avl_node *
avl_new_node(avl_tree *tree)
{ avl_free_list *l;
  avl_node *n;

  if ( !(l=tree->free_list) || l->left == 0 )
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
{ tree->root      = NULL;
  tree->free_list = NULL;
  tree->size      = 0L;
}


void
avl_destroy(avl_tree *tree)
{ avl_free_list *l, *n;

  for(l=tree->free_list; l; l=n)
  { n = l->next;
    PL_free(l);
  }
}

