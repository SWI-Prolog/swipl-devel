/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CLASS_H
#define _PCE_CLASS_H

extern "C" {				/* abstract C interface */
Any 		XPCE_makeclass(const Any name,
			       const Any super,
			       const Any summary);
Any 		XPCE_defcxxclass(const Any name,
				 const Any super,
				 const Any summary,
				 const Any makefunction);

Any 		XPCE_defvar(const Any cl,
			    const Any name,
			    const Any group,
			    const Any summary,
			    const Any type,
			    const Any access,
			    const Any initial);

PceStatus	XPCE_defsendmethodv(const Any cl,
				    const Any name,
				    const Any group,
				    const Any summary,
				    const Any implementation,
				    const int argc, const Any types[]);

PceStatus	XPCE_defgetmethodv(const Any cl,
				   const Any name,
				   const Any group,
				   const Any summary,
				   const Any ret_type,
				   const Any implementation,
				   const int argc, const Any types[]);

PceStatus	XPCE_send_superv(Any receiver, Any selector,
				 int argc, const Any argv[]);
Any 		XPCE_get_superv(Any receiver, Any selector,
				int argc, const Any argv[]);
PceStatus	XPCE_store(Any in, const Any var, const Any value);
Any 		XPCE_fetch(Any in, const Any var);
}

class PceVariable :public PceArg
{ 
public:
  PceVariable(Any p)
  { self = p;
  }
};


class PceReceiver :public PceArg
{
public:
  PceReceiver(Any p)
  { self = p;
  }
  PceReceiver(PceArg a)
  { self = a.self;
  }

  PceStatus store(PceVariable *var, PceArg value)
  { return XPCE_store(self, var->self, value.self);
  }
  PceArg fetch(PceVariable *var)
  { return PceArg(XPCE_fetch(self, var->self));
  }

  PceStatus sendSuper(PceArg s)
  { return XPCE_send_superv(self, s, 0, NULL);
  }
  PceStatus sendSuper(PceArg s, PceArg a1)
  { Any av[1];
    av[0] = a1.self;
    return XPCE_send_superv(self, s, 1, (const Any *)av);
  }
  PceStatus sendSuper(PceArg s, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    return XPCE_send_superv(self, s, 2, (const Any *)av);
  }
  PceStatus sendSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    return XPCE_send_superv(self, s, 3, (const Any *)av);
  }
  PceStatus sendSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    return XPCE_send_superv(self, s, 4, (const Any *)av);
  }
  PceStatus sendSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
		      PceArg a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    return XPCE_send_superv(self, s, 5, (const Any *)av);
  }
  PceStatus sendSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
		      PceArg a5, PceArg a6)
  { Any av[6];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    return XPCE_send_superv(self, s, 6, (const Any *)av);
  }

  PceArg getSuper(PceArg s)
  { return XPCE_get_superv(self, s, 0, NULL);
  }
  PceArg getSuper(PceArg s, PceArg a1)
  { Any av[1];
    av[0] = a1.self;
    return XPCE_get_superv(self, s, 1, (const Any *)av);
  }
  PceArg getSuper(PceArg s, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    return XPCE_get_superv(self, s, 2, (const Any *)av);
  }
  PceArg getSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    return XPCE_get_superv(self, s, 3, (const Any *)av);
  }
  PceArg getSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    return XPCE_get_superv(self, s, 4, (const Any *)av);
  }
  PceArg getSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
		  PceArg a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    return XPCE_get_superv(self, s, 5, (const Any *)av);
  }
  PceArg getSuper(PceArg s, PceArg a1, PceArg a2, PceArg a3, PceArg a4,
		  PceArg a5, PceArg a6)
  { Any av[6];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    av[5] = a6.self;
    return XPCE_get_superv(self, s, 6, (const Any *)av);
  }
};


typedef PceArg (*PceMethodFunc0)(PceReceiver);
typedef PceArg (*PceMethodFunc1)(PceReceiver, PceArg);
typedef PceArg (*PceMethodFunc2)(PceReceiver, PceArg, PceArg);
typedef PceArg (*PceMethodFunc3)(PceReceiver, PceArg, PceArg, PceArg);
typedef PceArg (*PceMethodFunc4)(PceReceiver, PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*PceMethodFunc5)(PceReceiver, PceArg, PceArg, PceArg, PceArg,
				 PceArg);
typedef PceArg (*PceMethodFunc6)(PceReceiver, PceArg, PceArg, PceArg, PceArg,
				 PceArg, PceArg);
typedef PceArg (*PceMethodFunc7)(PceReceiver, PceArg, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg);
typedef PceArg (*PceMethodFunc8)(PceReceiver, PceArg, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*PceMethodFunc9)(PceReceiver, PceArg, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg, PceArg, PceArg);

typedef PceStatus (*PceMethodProc0)(PceReceiver);
typedef PceStatus (*PceMethodProc1)(PceReceiver, PceArg);
typedef PceStatus (*PceMethodProc2)(PceReceiver, PceArg, PceArg);
typedef PceStatus (*PceMethodProc3)(PceReceiver, PceArg, PceArg, PceArg);
typedef PceStatus (*PceMethodProc4)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg);
typedef PceStatus (*PceMethodProc5)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg, PceArg);
typedef PceStatus (*PceMethodProc6)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg);
typedef PceStatus (*PceMethodProc7)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*PceMethodProc8)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg, PceArg,PceArg);
typedef PceStatus (*PceMethodProc9)(PceReceiver, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg, PceArg, PceArg,
				    PceArg);


class PceClass :public PceArg
{
public:
  PceClass(char *name) :
    PceArg((Any)XPCE_to_class(XPCE_to_name(name)))
  {
  }
  PceClass(PceArg name, PceArg super) :
    PceArg(XPCE_makeclass(name.self, super.self, NULL))
  {
  }
  PceClass(PceArg name, PceArg super, PceArg summary) :
    PceArg(XPCE_makeclass(name.self, super.self, summary))
  {
  }
  PceClass(PceArg name, PceArg super, PceArg summary,
	   PceStatus (*f)(PceClass)) :
    PceArg(XPCE_defcxxclass(name.self, super.self, summary, (void *)f))
  {
  }

					/* VARIABLES */
  PceVariable* defvar(PceArg name, PceArg group, PceArg summary,
		      PceArg type, PceArg access, PceArg init)
  { return new PceVariable(XPCE_defvar(self,
				       name.self, group.self, summary.self, 
				       type.self, access.self, init.self));
  }
  PceVariable* defvar(PceArg name, PceArg group, PceArg summary,
		      PceArg type, PceArg access)
  { return new PceVariable(XPCE_defvar(self,
				       name.self, group.self, summary.self, 
				       type.self, access.self, TheNil));
  }

					/* SEND-METHOD */
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc0 f)
  { XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 0, NULL);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc1 f, PceArg t1)
  { Any av[1];
    av[0] = t1.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 1, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc2 f, PceArg t1, PceArg t2)
  { Any av[2];
    av[0] = t1.self;
    av[1] = t2.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 2, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc3 f, PceArg t1, PceArg t2, PceArg t3)
  { Any av[3];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 3, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc4 f, PceArg t1, PceArg t2, PceArg t3,
		     PceArg t4)
  { Any av[4];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 4, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc5 f, PceArg t1, PceArg t2, PceArg t3,
		     PceArg t4, PceArg t5)
  { Any av[5];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    av[4] = t5.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 5, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc6 f, PceArg t1, PceArg t2, PceArg t3,
		     PceArg t4, PceArg t5, PceArg t6)
  { Any av[6];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    av[4] = t5.self;
    av[5] = t6.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 6, (const Any *)av);
  }
  void defsendmethod(PceArg name, PceArg grp, PceArg summ,
		     PceMethodProc7 f, PceArg t1, PceArg t2, PceArg t3,
		     PceArg t4, PceArg t5, PceArg t6, PceArg t7)
  { Any av[7];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    av[4] = t5.self;
    av[5] = t6.self;
    av[6] = t7.self;
    XPCE_defsendmethodv(self, name.self, grp.self, summ.self,
			(void *)f, 7, (const Any *)av);
  }

					/* GET-METHOD */
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc0 f)
  { XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 0, NULL);
  }
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc1 f, PceArg t1)
  { Any av[1];
    av[0] = t1.self;
    XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 1, (const Any *)av);
  }
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc2 f, PceArg t1, PceArg t2)
  { Any av[2];
    av[0] = t1.self;
    av[1] = t2.self;
    XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 2, (const Any *)av);
  }
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc3 f, PceArg t1, PceArg t2,
		    PceArg t3)
  { Any av[3];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 3, (const Any *)av);
  }
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc4 f, PceArg t1, PceArg t2,
		    PceArg t3, PceArg t4)
  { Any av[4];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 4, (const Any *)av);
  }
  void defgetmethod(PceArg name, PceArg group, PceArg summary,
		    PceArg rtype, PceMethodFunc5 f, PceArg t1, PceArg t2,
		    PceArg t3, PceArg t4, PceArg t5)
  { Any av[5];
    av[0] = t1.self;
    av[1] = t2.self;
    av[2] = t3.self;
    av[3] = t4.self;
    av[4] = t5.self;
    XPCE_defgetmethodv(self, name.self, group.self, summary.self,
		       rtype.self, (void *)f, 5, (const Any *)av);
  }
};

#endif /*!_PCE_CLASS_H*/
