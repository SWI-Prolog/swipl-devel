
/* constraint.c */
status		lockConstraint(Constraint c, Any obj);
status		unlockConstraint(Constraint c, Any obj);
status		executeConstraint(Constraint c, Any obj);
status		makeClassConstraint(Class class);

/* hyper.c */
status		makeClassHyper(Class class);

/* identity.c */
status		makeClassIdentity(Class class);

/* relation.c */
status		makeClassRelation(Class class);

/* spatial.c */
status		makeClassSpatial(Class class);
