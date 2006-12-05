################################################################
# Build jpl.jar
################################################################

.SUFFIXES: .java .class

JAVAC="$(JAVA_HOME)\bin\javac"
JAR="$(JAVA_HOME)\bin\jar"
JAVADOC="$(JAVA_HOME)\bin\javadoc"
JPLJAR=..\..\jpl.jar
TSTJAR=..\..\jpltest.jar
JPLDOC=..\..\docs\java_api\javadoc

CLS=	jpl\Atom.java \
	jpl\Compound.java \
	jpl\Float.java \
	jpl\Integer.java \
	jpl\JPLException.java \
	jpl\JPL.java \
	jpl\PrologException.java \
	jpl\Query.java \
	jpl\Term.java \
	jpl\Util.java \
	jpl\Variable.java \
	jpl\Version.java

FLI=	jpl\fli\atom_t.java \
	jpl\fli\BooleanHolder.java \
	jpl\fli\DoubleHolder.java \
	jpl\fli\engine_t.java \
	jpl\fli\fid_t.java \
	jpl\fli\functor_t.java \
	jpl\fli\IntHolder.java \
	jpl\fli\Int64Holder.java \
	jpl\fli\LongHolder.java \
	jpl\fli\module_t.java \
	jpl\fli\ObjectHolder.java \
	jpl\fli\PointerHolder.java \
	jpl\fli\predicate_t.java \
	jpl\fli\Prolog.java \
	jpl\fli\qid_t.java \
	jpl\fli\StringHolder.java \
	jpl\fli\term_t.java

TEST=	jpl\test\Family.java \
	jpl\test\FetchBigTree.java \
	jpl\test\FetchLongList.java \
	jpl\test\Ga2.java \
	jpl\test\Ga.java \
	jpl\test\Garbo.java \
	jpl\test\Masstest.java \
	jpl\test\MaxObjects.java \
	jpl\test\ShadowA.java \
	jpl\test\ShadowB.java \
	jpl\test\SyntaxError.java \
	jpl\test\Test.java \
	jpl\test\TestJUnit.java \
	jpl\test\TestOLD.java

JPLJAVA=$(CLS) $(FLI)
TSTJAVA=$(TEST)

all:	$(JPLJAR) $(TSTJAR) $(JPLDOC)

$(JPLJAR):	$(JPLJAVA)
		$(JAVAC) $(JPLJAVA)
		$(JAR) cf $(JPLJAR) $(JPLJAVA:.java=.class)

$(TSTJAR):	$(JPLJAR) $(TSTJAVA) 
		$(JAVAC) -classpath "$(JPLJAR);$(JUNIT)" $(TSTJAVA)
		$(JAR) cf $(TSTJAR) $(TSTJAVA:.java=.class)

$(JPLDOC):	$(JPLJAVA)
		$(JAVADOC) -public -d $(JPLDOC) $(JPLJAVA)

clean::
	if exist jpl\*.class del jpl\*.class
	if exist jpl\fli\*.class del jpl\fli\*.class
	if exist jpl\test\*.class del jpl\test\*.class
	if exist jpl\util\*.class del jpl\util\*.class
	if exist *~ del *~

distclean:	clean
	if exist $(JPLJAR) del $(JPLJAR)
	if exist $(TSTJAR) del $(TSTJAR)
	if exist $(JPLDOC) rmdir /s /q $(JPLDOC)


