BEGIN	{ atom = functor = 0;
	}
/^#/	{ next;
	}
/^A/	{ printf "#define ATOM_%-12s MK_ATOM(%d)\n",$2,atom  > "pl-atom.ih"
	  printf "ATOM(%s),\n",$3  > "pl-atom.ic"
	  atom++;
	  next;
	}
/^F/	{ name = $2 $3;
	  printf "#define FUNCTOR_%-12s MK_FUNCTOR(%d, %d)\n",name,functor,$3 > "pl-funct.ih"
	  printf "FUNCTOR(ATOM_%s, %d),\n",$2,$3 > "pl-funct.ic"
	  functor++;
	}
