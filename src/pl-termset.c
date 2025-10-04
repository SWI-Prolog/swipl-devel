/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, University of Amsterdam
                         VU University Amsterdam
			 CWI, Amsterdam
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

#include "pl-incl.h"
#include "pl-termset.h"

typedef struct TermNode
{ term_t key;
  struct TermNode *left, *right;
  int level;
} TermNode;

#define TERMNODE_CHUNK 256

typedef struct TermNodeChunk
{ struct TermNode nodes[TERMNODE_CHUNK];
  int used;
  struct TermNodeChunk *next;
} TermNodeChunk;

static inline uintptr_t keyval(term_t t)
{ return (uintptr_t)valTermRef(t);
}

static TermNode *
alloc_node(term_set *s, term_t t)
{ TermNodeChunk *c = s->chunks;
  if ( !c || c->used >= TERMNODE_CHUNK )
  { c = malloc(sizeof(*c));
    if (!c) return NULL;
    c->used = 0;
    c->next = s->chunks;
    s->chunks = c;
  }
  TermNode *n = &c->nodes[c->used++];
  n->key = t;
  n->left = n->right = NULL;
  n->level = 1;

  return n;
}

static TermNode *
skew(TermNode *n)
{ if ( n && n->left && n->left->level == n->level )
  { TermNode *L = n->left;
    n->left = L->right;
    L->right = n;
    return L;
  }
  return n;
}

static TermNode *
split(TermNode *n)
{ if ( n && n->right && n->right->right &&
       n->right->right->level == n->level )
  { TermNode *R = n->right;
    n->right = R->left;
    R->left = n;
    R->level++;
    return R;
  }
  return n;
}

static TermNode *
insert_node(term_set *s, TermNode *n, term_t t, bool *inserted)
{ if ( !n ) {
    *inserted = true;
    return alloc_node(s, t);
  }

  uintptr_t k = keyval(t), nk = keyval(n->key);

  if ( k < nk )
  { n->left = insert_node(s, n->left, t, inserted);
  } else if ( k > nk )
  { n->right = insert_node(s, n->right, t, inserted);
  } else
  { *inserted = false;
    return n;
  }

  n = skew(n);
  n = split(n);

  return n;
}

static bool
contains_node(TermNode *n, term_t t)
{ uintptr_t k = keyval(t);
  while (n)
  { uintptr_t nk = keyval(n->key);
    if (k < nk) n = n->left;
    else if (k > nk) n = n->right;
    else return true;
  }
  return false;
}

		 /*******************************
		 *          PUBLIC API          *
		 *******************************/

void
new_term_set(term_set *s)
{ s->root = NULL;
  s->chunks = NULL;
}

void
term_set_destroy(term_set *s)
{ TermNodeChunk *c = s->chunks;
  while (c)
  { TermNodeChunk *next = c->next;
    free(c);
    c = next;
  }
  s->chunks = NULL;
  s->root = NULL;
}

bool
term_set_insert(term_set *s, term_t t)
{ bool inserted = 0;
  s->root = insert_node(s, s->root, t, &inserted);

  return inserted;
}

bool
term_set_contains(term_set *s, term_t t)
{ return contains_node(s->root, t);
}
