################################################################
# Setup the environment for compiling and running the demos on
# Unix like platforms.  This script is sourced from the run.sh
# scripts in the various example directories.
# 
# You may need to edit this before running the demos
# 
# Required setup
# 
# 	* The directory holding java and javac must be in $PATH
# 	* JPL must be installed
# 	* Prolog must be available as one of "swi-prolog", "swipl"
#	  or "pl" in $PATH
#
################################################################

findexe()
{ oldifs="$IFS"
  IFS=:
  for d in $PATH; do
    if [ -x $d/$1 ]; then
       IFS="$oldifs"
       return 0
    fi
  done
  IFS="$oldifs"
  return 1
}

for f in swi-prolog swipl pl; do
  if [ -z "$PL" ]; then
     if findexe $f; then
        PL="$f"
     fi
  fi
done

if findexe java; then
  true
elif [ -x "$JAVA_HOME"/bin/java ]; then
  PATH="$PATH:$JAVA_HOME/bin"
else
  echo "ERROR: Cannot find java.  Please ensure JAVA_HOME is set"
  echo "ERROR: properly or java is in $PATH"
  exit 1
fi

if findexe javac; then
  true
else
  echo "ERROR: Cannot find javac.  This demo requires the SDK to"
  echo "ERROR: be installed and accessible through JAVA_HOME"
  echo "ERROR: or PATH"
  exit 1
fi

################################################################
# Setup the environment
################################################################

eval `$PL -dump-runtime-variables`

PLLIBDIR="$PLBASE/lib/$PLARCH"
JPLJAR="$PLBASE/lib/jpl.jar"

if [ -z "$LD_LIBRARY_PATH" ]; then
   LD_LIBRARY_PATH="$PLLIBDIR";
else
   LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$PLLIBDIR"
fi

if [ -z "$CLASSPATH" ]; then
   CLASSPATH=".:$JPLJAR";
else
   CLASSPATH=".:$JPLJAR:$CLASSPATH"
fi

export LD_LIBRARY_PATH CLASSPATH

################################################################
# compile Class
#
# Compile the indicated class if necessary
################################################################

compile()
{ if [ ! -f $1.class ]; then
    echo "Compiling $1"
    javac $1.java
  elif [ $1.java -nt $1.class ]; then
    echo "Recompiling $1"
    javac $1.java
  fi
}


################################################################
# run Class
# 
# Compiles Class if the .class file does not exsist and runs it
################################################################

run()
{ compile $1

  if [ "$JPL_COMPILE_ONLY" != "yes" ]; then
    echo ""
    echo "JPL demo: $1"
    echo ""

    java $1
  fi
}

################################################################
# run_preloaded Class
# 
# As run Class, but preloads libjpl.so to be able to use foreign
# extensions to Prolog.  See the SemWeb example
################################################################

run_preloaded()
{ compile $1

  if [ "$JPL_COMPILE_ONLY" != "yes" ]; then
    JPLSO="$PLBASE/lib/$PLARCH/libjpl.$PLSOEXT"

    echo ""
    echo "JPL demo: $1"
    echo "Using preloaded $JPLSO"
    echo ""

  
    env LD_PRELOAD=$JPLSO java $1
  fi
} 
