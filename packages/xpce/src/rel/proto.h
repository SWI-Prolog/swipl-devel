#define COMMON(type) type SO_LOCAL

/* /staff/jan/src/pl/packages/xpce/src/rel/constraint.c */
COMMON(status)	lockConstraint(Constraint c, Any obj);
COMMON(status)	unlockConstraint(Constraint c, Any obj);
COMMON(status)	executeConstraint(Constraint c, Any obj);
COMMON(status)	makeClassConstraint(Class class);

/* /staff/jan/src/pl/packages/xpce/src/rel/hyper.c */
COMMON(status)	makeClassHyper(Class class);
COMMON(status)	makeClassChainHyper(Class class);

/* /staff/jan/src/pl/packages/xpce/src/rel/identity.c */
COMMON(status)	makeClassIdentity(Class class);

/* /staff/jan/src/pl/packages/xpce/src/rel/relation.c */
COMMON(status)	makeClassRelation(Class class);

/* /staff/jan/src/pl/packages/xpce/src/rel/spatial.c */
COMMON(status)	makeClassSpatial(Class class);
