Java calling Prolog semweb library
----------------------------------

This demo is a very simple example   of loading and using the SWI-Prolog
semantic web library for parsing and  querying   of  RDF  data. Its main
purpose is actually to illustrate and test access from Prolog to foreign
resources. This demo uses sgml2pl.so and   rdf_db.so,  providing the XML
parser and RDF database.

ELF systems (Linux, Solaris and many more)
------------------------------------------

In the current setup libjpl.so contains  the Prolog kernel and therefore
the extensions must get the PL_* symbols   from this library. Java loads
shared objects without making their symbols  available to other modules.
This problem is avoided by preloading   libjpl.so  as illustrated in the
run_preloaded  function  defined  in  env.sh.    Below   is  a  portable
shell-script skeleton to deal with this situation:

----------------------------------------------------------------
#!/bin/sh

class=SemWeb

eval `$PL -dump-runtime-variables`
JPLSO="$PLBASE/lib/$PLARCH/libjpl.$PLSOEXT"
JPLJAR="$PLBASE/lib/jpl.jar"

if [ -z "$CLASSPATH" ]; then
   CLASSPATH=".:$JPLJAR";
else
   CLASSPATH=".:$JPLJAR:$CLASSPATH"
fi

env LD_PRELOAD=$JPLSO java $class
----------------------------------------------------------------
