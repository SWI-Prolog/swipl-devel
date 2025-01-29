#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running delay_tests/test.sh (using call subsumption) ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2

#----------------------------------------------------------------------
# Tests of dynamically stratified negation.
#----------------------------------------------------------------------
    # XEMU and opts must be together in quotes
../sgentest.sh "$XEMU $opts" dynstrat1 "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat2 "test."	# No simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat3 "test."	# Needs p+n simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat4 "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat5 "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat6 "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" dynstrat7 "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" ross1 "test."	# No simplification (OK)
../sgentest.sh "$XEMU $opts" sel_unsusp "test."	# Tests selective unsuspension
					# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" dl_dupl "test."	# Needs p+n simplification (OK)
../sgentest.sh "$XEMU $opts" asl_dupl "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" gfp "test."		# Needs cascading simplifications (OK)
#----------------------------------------------------------------------
# Tests that at some early point in the development gave false results.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" fr1 "test."		#
../sgentest.sh "$XEMU $opts" fr2 "test."		#
../sgentest.sh "$XEMU $opts" fr3 "test."		#
../sgentest.sh "$XEMU $opts" fr4 "test."		#
../sgentest.sh "$XEMU $opts" fr5 "test."		#
../sgentest.sh "$XEMU $opts" fr6 "test."		#
../sgentest.sh "$XEMU $opts" fr7 "test."		#
../sgentest.sh "$XEMU $opts" fr8 "test."		#
../sgentest.sh "$XEMU $opts" fr9 "test."		#
../sgentest.sh "$XEMU $opts" fr19 "test."	#
../sgentest.sh "$XEMU $opts" fr20 "test."	#
../sgentest.sh "$XEMU $opts" fr21 "test."	#
../sgentest.sh "$XEMU $opts" fr22 "test."	#
../sgentest.sh "$XEMU $opts" fr23 "test."	#
../sgentest.sh "$XEMU $opts" fr25 "test."	#
../sgentest.sh "$XEMU $opts" fr26 "test."	#
../sgentest.sh "$XEMU $opts" fr27 "test."	#
../sgentest.sh "$XEMU $opts" fr28 "test."	#
../sgentest.sh "$XEMU $opts" fr29 "test."	#shows need for undeleting answers (OK)
../sgentest.sh "$XEMU $opts" fr30 "test."	#shows need for junking answers (OK)
#----------------------------------------------------------------------
# Tests that at some early point in the development caused
# segmentation faults.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" seg1 "test."		# (OK)
../sgentest.sh "$XEMU $opts" seg2 "test."		# (OK)
../sgentest.sh "$XEMU $opts" seg3 "test."		# (OK)
../sgentest.sh "$XEMU $opts" seg4 "test."		# (OK)
../sgentest.sh "$XEMU $opts" seg5 "test."		# Only when interpreted (OK)
../sgentest.sh "$XEMU $opts" fr24 "test."		# Needed restoration of ptcpreg (OK)
#----------------------------------------------------------------------
# Tests from David's automatic generation meta-interpreter that at
# some early point in the development caused segmentation faults.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" interp0 "test."	#
../sgentest.sh "$XEMU $opts" interp1 "test."	#
../sgentest.sh "$XEMU $opts" interp2 "test."	# was giving wrong results (OK)
#../sgentest.sh "$XEMU $opts" interp3 "test."	# was causing an infinite loop
../sgentest.sh "$XEMU $opts" interp4 "test."	# showed need for ptcpreg in Prolog CPs
../sgentest.sh "$XEMU $opts" interp5 "test."	#
../sgentest.sh "$XEMU $opts" interp6 "test."	#
../sgentest.sh "$XEMU $opts" interp7 "test."	#
../sgentest.sh "$XEMU $opts" interp8 "test."	#
../sgentest.sh "$XEMU $opts" interp9 "test."	#
../sgentest.sh "$XEMU $opts" interp10 "test."	#
../sgentest.sh "$XEMU $opts" interp11 "test."	# was giving wrong results (OK)
#----------------------------------------------------------------------
# Genome tests involving unclassified negation.
#----------------------------------------------------------------------
#	sgentest.csh "$XEMU $opts" q6 "test."
#----------------------------------------------------------------------
# Tests of non-stratified negation.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" two_ary "test."
../sgentest.sh "$XEMU $opts" abol_susp1 "test."	# Tests abolishing suspensions (OK)
../sgentest.sh "$XEMU $opts" abol_susp2 "test."	# Tests abolishing suspensions (OK)
../sgentest.sh "$XEMU $opts" przy1 "test."	# No simplification (OK)
../sgentest.sh "$XEMU $opts" przy1_simp "test."	# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" nonstrat1 "test."	# No simplification (OK)
../sgentest.sh "$XEMU $opts" nonstrat2 "test."	# Needs n simplification (OK)
#----------------------------------------------------------------------
# Tests of well-founded negation.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" p1 "test."		# No simplification (OK)
../sgentest.sh "$XEMU $opts" p2 "test."		# No simplification (OK)
../sgentest.sh "$XEMU $opts" p3 "test."		# No simplification (OK)
../sgentest.sh "$XEMU $opts" p4 "test."		# No simplification (OK)
../sgentest.sh "$XEMU $opts" p5 "test."		# Needs n simplification (OK)
../sgentest.sh "$XEMU $opts" simpl_win "test."	# No simplification (OK)
../sgentest.sh "$XEMU $opts" win "test."		# Tests cond vs uncond. answers
					# No simplification (OK)
../sgentest.sh "$XEMU $opts" cond_uncond "test."	# Tests cond vs uncond. answers
					# and early simplification (OK)
../sgentest.sh "$XEMU $opts" ullman3 "test."	# Requires answers to be returned
					# from completed tables (OK)
../sgentest.sh "$XEMU $opts" undef1 "test."	# To test printing of delay lists
../sgentest.sh "$XEMU $opts" undef2 "test."	# To test printing of delay lists
#----------------------------------------------------------------------
# Tests of positive simplification.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" pos_simpl1 "test."	# To test printing of delay lists
#----------------------------------------------------------------------
# A mixture of negation tests.
#----------------------------------------------------------------------
#	sgentest.csh "$XEMU $opts" pot_pouri "test."
#----------------------------------------------------------------------
# Tests that may require answer completion.
#----------------------------------------------------------------------
#	sgentest.csh "$XEMU $opts" weidong1 "test."
../sgentest.sh "$XEMU $opts" weidong2 "test."	# Requires simpl. + ans. compl. (OK)
../sgentest.sh "$XEMU $opts" weidong3 "test."	# OK
../sgentest.sh "$XEMU $opts" weidong4 "test."	# Requires simpl. + ans. compl. (WRONG)
../sgentest.sh "$XEMU $opts" weidong5 "test."	# Requires answer completion (OK)
../sgentest.sh "$XEMU $opts" weidong6 "test."	# Requires answer completion (OK)
../sgentest.sh "$XEMU $opts" weidong7 "test."	# Requires answer completion (WRONG)
../sgentest.sh "$XEMU $opts" weidong8 "test."	# Requires answer completion (WRONG)
#----------------------------------------------------------------------
# Tests that do require answer completion.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" interp12 "test."	# Requires answer completion (WRONG)
../sgentest.sh "$XEMU $opts" interp13 "test."	# Requires answer completion (WRONG)
../sgentest.sh "$XEMU $opts" interp14 "test."	# Requires answer completion (WRONG)
../sgentest.sh "$XEMU $opts" interp15 "test."	# Requires answer completion (WRONG)
#----------------------------------------------------------------------
# Tests of floundering behaviour.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" avoid_flounder "test."
#----------------------------------------------------------------------
# Tests of residual program.
#----------------------------------------------------------------------
# Still buggy -- need to fix (TLS)
#../sgentest.sh "$XEMU $opts" residual1 "test."
#----------------------------------------------------------------------
# Tests of residual program and findall/3.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" fa "test."
#----------------------------------------------------------------------
# Tests of variables in delay list.
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" delay_var "test."
#----------------------------------------------------------------------
# Tests of simplification and delete return
#----------------------------------------------------------------------
../sgentest.sh "$XEMU $opts" dret_test "test."
../sgentest.sh "$XEMU $opts" tabsimp_seq "test(20000)."
