#!/bin/awk -f
BEGIN						  { copy = "no";
						    first = "yes";
						  }
/^%[ 	]*[a-z][a-zA-Z_0-9]*\/0.$/		  { copy = "yes";
						    if ( first == "yes" )
						    { print ".BD";
						      first = "no";
						    }
						    print $0;
						    next;
						  }
/^%[ 	]*title*:/				  { copy = "yes";
						    print $0;
						    next;
						  }
/^%[ 	]*library*:/				  { copy = "yes";
						    print $0;
						    next;
						  }
/^%[ 	]*[a-z][a-zA-Z_0-9]*\([A-Za-z0-9_?+ -,]*\)/ { copy = "yes";
						    if ( first == "yes" )
						    { print ".BD";
						      first = "no";
						    }
						    print $0;
						    next;
						  }
/^%/						  { if ( copy == "yes" )
						      print $0;
						    next;
						  }
						  { copy = "no";
						    next;
						  }
END						  { print ".ED";
						  }
