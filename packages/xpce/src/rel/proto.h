
/* rel/constraint.c */
status		lockConstraint(Constraint c, Any obj);
status		unlockConstraint(Constraint c, Any obj);
status		executeConstraint(Constraint c, Any obj);
status		makeClassConstraint(Class class);

/* rel/hyper.c */
status		makeClassHyper(Class class);

/* rel/identity.c */
status		makeClassIdentity(Class class);

/* rel/relation.c */
status		makeClassRelation(Class class);

/* rel/spatial.c */
status		makeClassSpatial(Class class);
