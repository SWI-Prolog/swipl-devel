
/* ../src/rel/constraint.c */
status		lockConstraint(Constraint c, Any obj);
status		unlockConstraint(Constraint c, Any obj);
status		executeConstraint(Constraint c, Any obj);
status		makeClassConstraint(Class class);

/* ../src/rel/hyper.c */
status		makeClassHyper(Class class);
status		makeClassChainHyper(Class class);

/* ../src/rel/identity.c */
status		makeClassIdentity(Class class);

/* ../src/rel/relation.c */
status		makeClassRelation(Class class);

/* ../src/rel/spatial.c */
status		makeClassSpatial(Class class);
