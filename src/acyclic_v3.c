/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

A DFS search is used to iterate over all 'term chains' in  a term graph.
A term chain is a list of terms  where term N+1 is  the last argument of
term N.  Terms are 'TEMP' marked as we find them. When the end of a term
chain has been reached  all terms in the chain  are 'PERM' marked  as we
know that all terms  in that chain are acyclic.  If we reach a term that
was already TEMP marked then we terminate the search as a cycle has been
detected.  If we reach a term  that has already been PERM marked then we
backtrack as a shared term that we know to be acyclic has been reached.

Two strategies are used  to avoid repeated  pop+push cycles  of the same
term chain:

1. aggresively cache new term chains for all args of the tail term.
2. only cache the current term chain  if we know at least one arg of the
   tail term is itself a term.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ACYCLIC_TEMP_MASK	FIRST_MASK
#define ACYCLIC_PERM_MASK	MARK_MASK

#define set_acyclic_temp(p)	do { *(p) |= ACYCLIC_TEMP_MASK; } while(0)
#define set_acyclic_perm(p)	do { *(p) |= ACYCLIC_PERM_MASK; } while(0)

#define clear_acyclic_temp(p)	do { *(p) &= ~ACYCLIC_TEMP_MASK; } while(0)
#define clear_acyclic_perm(p)	do { *(p) &= ~ACYCLIC_PERM_MASK; } while(0)
#define clear_acyclic_both(p)	do { *(p) &= ~(ACYCLIC_TEMP_MASK|ACYCLIC_PERM_MASK); } while(0)

#define is_acyclic_temp(p)	(*(p) & ACYCLIC_TEMP_MASK)
#define is_acyclic_perm(p)	(*(p) & ACYCLIC_PERM_MASK)
#define is_acyclic_or_temp(p)	(*(p) & (ACYCLIC_TEMP_MASK|ACYCLIC_PERM_MASK))


typedef struct termChain
{ Functor	head;
  Functor	tail;
  Word          p;
} termChain;

typedef struct term_chain_agenda
{ termChain	work;
  segstack	stack;
} term_chain_agenda;


static int
ph_acyclic_mark_v3(Word p ARG_LD)
{
  term_chain_agenda agenda;
  termChain chains[32];
  Functor top = valueTerm(*p);
  Functor head = top;
  Functor tail = top;
  Functor iter;
  Word pdef;
  int arity;

  initSegStack(&agenda.stack, sizeof(termChain), sizeof(chains), chains);

  while ( TRUE )
  {
    if ( is_acyclic_temp(&tail->definition) )
    { if ( is_acyclic_perm(&tail->definition) )
      { goto end_of_chain;
      }
      else
      { clearSegStack(&agenda.stack);
        return FALSE;
      }
    }

    set_acyclic_temp(&tail->definition);

    arity = arityFunctor(tail->definition);

    if ( arity > 1 )
    { int i;
      int new_workspace = FALSE;

      iter = tail;
      for ( i = arity-2; i >= 0; i-- )
      {
        p = iter->arguments + i;
        deRef(p);

        if ( isTerm(*p) )
        {
          if ( !new_workspace )
          { pushSegStack(&agenda.stack, agenda.work, termChain);
            agenda.work.head = head;
            agenda.work.tail = tail;
            agenda.work.p = iter->arguments + arity-1;

            head = tail = valueTerm(*p);
            new_workspace = TRUE;
          }

          else
          { pushSegStack(&agenda.stack, agenda.work, termChain);
            agenda.work.head = agenda.work.tail = valueTerm(*p);
            agenda.work.p = NULL;
          }
        }
      }
      if ( new_workspace )
      { continue;
      }
    }

    p = tail->arguments + arity-1;
    deRef(p);

process_p:

    if ( isTerm(*p) )
    { tail = valueTerm(*p);
    }

    else
    {
end_of_chain:

      if ( head == top )
      { return TRUE;
      }

      iter = head;
      pdef = &iter->definition;
      while ( iter != tail )
      {
        set_acyclic_perm(pdef);

        p = iter->arguments + arityFunctor(*pdef) - 1;
        deRef(p);
        iter = valueTerm(*p);
        pdef = &iter->definition;
      }
      set_acyclic_perm(pdef);

      head = agenda.work.head;
      tail = agenda.work.tail;
      p = agenda.work.p;

      if ( !popSegStack(&agenda.stack, &agenda.work, termChain) )
      { assert(0);
      }

      if ( p )
      { deRef(p);
        goto process_p;
      }
    }
  }

  return TRUE;
}


static int
ph_acyclic_unmark_v3(Word p ARG_LD)
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      Word p = &f->definition;

      if ( is_acyclic_temp(p) )
      { clear_acyclic_both(p);
      }
      else
      { continue;
      }

      pushWorkAgenda(&agenda, arityFunctor(f->definition), f->arguments);
    }
  }

  return TRUE;
}

int
is_acyclic_v3(Word p ARG_LD)
{ int rc1;

  deRef(p);
  if ( isTerm(*p) )
  { rc1 = ph_acyclic_mark_v3(p PASS_LD);
    ph_acyclic_unmark_v3(p PASS_LD);

    return rc1;
  }

  return TRUE;
}
