/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include "parser.h"
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef XMLNS

static xmlns *
xmlns_push(dtd_parser *p, const ichar *ns, const ochar *url)
{ sgml_environment *env = p->environments;

  if ( env )
  { xmlns *n = malloc(sizeof(*n));

    n->name = (*ns ? dtd_add_symbol(p->dtd, ns) : (dtd_symbol *)NULL);
    n->url  = dtd_add_symbol(p->dtd, url); /* TBD: ochar/ichar */
    n->next = env->xmlns;
    env->xmlns = n;

    return n;
  }

  return NULL;
}


void
xmlns_free(sgml_environment *env)
{ xmlns *n, *next;

  for(n = env->xmlns; n; n = next)
  { next = n->next;

    free(n);
  }
}


xmlns *
xmlns_find(sgml_environment *env, dtd_symbol *ns)
{ for(; env; env = env->parent)
  { xmlns *n;

    for(n=env->xmlns; n; n = n->next)
    { if ( n->name == ns )
	return n;
    }
  }

  return NULL;
}


static ichar *
isxmlns(const ichar *s, int nschr)
{ if ( s[0]=='x' && s[1]=='m' && s[2]=='l' && s[3] =='n'&& s[4]=='s' )
  { if ( !s[5] )
      return (ichar *)s+5;			/* implicit */
    if ( s[5] == nschr )
      return (ichar *)s+6;
  }

  return NULL;
}


void
update_xmlns(dtd_parser *p, dtd_element *e, int natts, sgml_attribute *atts)
{ dtd_attr_list *al;
  int nschr = p->dtd->charfunc->func[CF_NS]; /* : */
       
  for(al=e->attributes; al; al=al->next)
  { dtd_attr *a = al->attribute;
    const ichar *name = a->name->name;

    if ( (name = isxmlns(name, nschr)) && /* TBD: flag when processing DTD */
	 a->type == AT_CDATA &&
	 (a->def == AT_FIXED || a->def == AT_DEFAULT) )
      xmlns_push(p, name, a->att_def.cdata);
  }

  for( ; natts-- > 0; atts++ )
  { const ichar *name = atts->definition->name->name;

    if ( (name=isxmlns(name, nschr)) && atts->definition->type == AT_CDATA )
      xmlns_push(p, name, atts->value.cdata);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
xmlns_resolve()
    Convert a symbol as returned by the XML level-1.0 parser to its namespace
    tuple {url}localname.  This function is not used internally, but provided
    for use from the call-back functions of the parser.  

    It exploits the stack of namespace-environments managed by the parser
    itself (see update_xmlns())
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
xmlns_resolve_attribute(dtd_parser *p, dtd_symbol *id,
			const ichar **local, const ichar **url)
{ dtd *dtd = p->dtd;
  int nschr = dtd->charfunc->func[CF_NS]; /* : */
  ichar buf[MAXNMLEN];
  ichar *o = buf;
  const ichar *s;
  xmlns *ns;

  for(s=id->name; *s; s++)
  { if ( *s == nschr )
    { dtd_symbol *n;

      *o = '\0';
      *local = s+1;
      n = dtd_add_symbol(dtd, buf);

      if ( istrprefix("xml", buf) )	/* XML reserved namespaces */
      { *url = n->name;
        return TRUE;
      } else if ( (ns = xmlns_find(p->environments, n)) )
      { if ( ns->url->name[0] )
	  *url = ns->url->name;
	else
	  *url = NULL;
	return TRUE;
      } else
      { *url = n->name;			/* undefined namespace */
	gripe(ERC_EXISTENCE, "namespace", n->name);
	return FALSE;
      }
    }
    *o++ = *s;
  }

  *local = id->name;

  if ( (ns = p->environments->thisns) && ns->url->name[0] )
    *url = ns->url->name;
  else
    *url = NULL;			/* no default namespace is defined */

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resolve the namespace for the current  element. This namespace is stored
in the environment as `thisns' and  acts   as  default for resolving the
namespaces of the attributes (see above).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
xmlns_resolve_element(dtd_parser *p, const ichar **local, const ichar **url)
{ sgml_environment *e;

  if ( (e=p->environments) )
  { dtd_symbol *id = e->element->name;
    dtd *dtd = p->dtd;
    int nschr = dtd->charfunc->func[CF_NS]; /* : */
    ichar buf[MAXNMLEN];
    ichar *o = buf;
    const ichar *s;
    xmlns *ns;
  
    for(s=id->name; *s; s++)
    { if ( *s == nschr )		/* explicit namespace */
      { dtd_symbol *n;
  
	*o = '\0';
	*local = s+1;
	n = dtd_add_symbol(dtd, buf);

	if ( (ns = xmlns_find(p->environments, n)) )
	{ if ( ns->url->name[0] )
	    *url = ns->url->name;
	  else
	    *url = NULL;
	  e->thisns = ns;		/* default for attributes */
	  return TRUE;
	} else
	{ *url = n->name;		/* undefined namespace */
	  gripe(ERC_EXISTENCE, "namespace", n->name);
	  e->thisns = xmlns_push(p, n->name, n->name); /* define implicitly */
	  return FALSE;
	}
      }
      *o++ = *s;
    }
  
    *local = id->name;
  
    if ( (ns = xmlns_find(p->environments, NULL)) )
    { if ( ns->url->name[0] )
	*url = ns->url->name;
      else
	*url = NULL;
      e->thisns = ns;
    } else
    { *url = NULL;			/* no default namespace is defined */
      e->thisns = NULL;
    }

    return TRUE;
  } else
    return FALSE;
}


#endif /*XMLNS*/

