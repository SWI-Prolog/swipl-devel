BEGIN	{ atom = functor = 0;
	}
/^#/	{ next;
	}
/^A/	{ printf "#define ATOM_%-12s (&atoms[%d])\n",$2,atom  > "pl-atom.ih"
	  printf "ATOM(%s),\n",$3  > "pl-atom.ic"
	  atom++;
	  next;
	}
/^F/	{ name = $2 $3;
	  printf "#define FUNCTOR_%-12s (&functors[%d])\n",name,functor > "pl-funct.ih"
	  printf "{ (FunctorDef)NULL, FUNCTOR_TYPE, ATOM_%s, %d },\n",$2,$3 > "pl-funct.ic"
	  functor++;
	}
