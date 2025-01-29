#! /bin/sh

echo "-------------------------------------------------------"
echo "--- Running delay_tests/test.sh                     ---"
echo "-------------------------------------------------------"

XEMU=$1
opts=$2

#----------------------------------------------------------------------
# Tests of dynamically stratified negation.
#----------------------------------------------------------------------
    # XEMU and opts must be together in quotes
../gentest.sh "$XEMU $opts" dynstrat1 "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat2 "test."	# No simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat3 "test."	# Needs p+n simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat4 "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat5 "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat6 "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" dynstrat7 "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" ross1 "test."	# No simplification (OK)
../gentest.sh "$XEMU $opts" sel_unsusp "test."	# Tests selective unsuspension
					# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" dl_dupl "test."	# Needs p+n simplification (OK)
../gentest.sh "$XEMU $opts" asl_dupl "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" gfp "test."		# Needs cascading simplifications (OK)
#----------------------------------------------------------------------
# Tests that at some early point in the development gave false results.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" fr1 "test."		#
../gentest.sh "$XEMU $opts" fr2 "test."		#
../gentest.sh "$XEMU $opts" fr3 "test."		#
../gentest.sh "$XEMU $opts" fr4 "test."		#
../gentest.sh "$XEMU $opts" fr5 "test."		#
../gentest.sh "$XEMU $opts" fr6 "test."		#
../gentest.sh "$XEMU $opts" fr7 "test."		#
../gentest.sh "$XEMU $opts" fr8 "test."		#
../gentest.sh "$XEMU $opts" fr9 "test."		#
../gentest.sh "$XEMU $opts" fr19 "test."	#
../gentest.sh "$XEMU $opts" fr20 "test."	#
../gentest.sh "$XEMU $opts" fr21 "test."	#
../gentest.sh "$XEMU $opts" fr22 "test."	#
../gentest.sh "$XEMU $opts" fr23 "test."	#
../gentest.sh "$XEMU $opts" fr25 "test."	#
../gentest.sh "$XEMU $opts" fr26 "test."	#
../gentest.sh "$XEMU $opts" fr27 "test."	#
../gentest.sh "$XEMU $opts" fr28 "test."	#
../gentest.sh "$XEMU $opts" fr29 "test."	#shows need for undeleting answers (OK)
../gentest.sh "$XEMU $opts" fr30 "test."	#shows need for junking answers (OK)
#----------------------------------------------------------------------
# Tests that at some early point in the development caused
# segmentation faults.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" seg1 "test."		# (OK)
../gentest.sh "$XEMU $opts" seg2 "test."		# (OK)
../gentest.sh "$XEMU $opts" seg3 "test."		# (OK)
../gentest.sh "$XEMU $opts" seg4 "test."		# (OK)
../gentest.sh "$XEMU $opts" seg5 "test."		# Only when interpreted (OK)
../gentest.sh "$XEMU $opts" fr24 "test."		# Needed restoration of ptcpreg (OK)
#----------------------------------------------------------------------
# Tests from David's automatic generation meta-interpreter that at
# some early point in the development caused segmentation faults.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" interp0 "test."	#
../gentest.sh "$XEMU $opts" interp1 "test."	#
../gentest.sh "$XEMU $opts" interp2 "test."	# was giving wrong results (OK)
#../gentest.sh "$XEMU $opts" interp3 "test."	# was causing an infinite loop
../gentest.sh "$XEMU $opts" interp4 "test."	# showed need for ptcpreg in Prolog CPs
../gentest.sh "$XEMU $opts" interp5 "test."	#
../gentest.sh "$XEMU $opts" interp6 "test."	#
../gentest.sh "$XEMU $opts" interp7 "test."	#
../gentest.sh "$XEMU $opts" interp8 "test."	#
../gentest.sh "$XEMU $opts" interp9 "test."	#
../gentest.sh "$XEMU $opts" interp10 "test."	#
../gentest.sh "$XEMU $opts" interp11 "test."	# was giving wrong results (OK)
#----------------------------------------------------------------------
# Genome tests involving unclassified negation.
#----------------------------------------------------------------------
#	gentest.csh "$XEMU $opts" q6 "test."
#----------------------------------------------------------------------
# Tests of non-stratified negation.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" two_ary "test."
../gentest.sh "$XEMU $opts" abol_susp1 "test."	# Tests abolishing suspensions (OK)
../gentest.sh "$XEMU $opts" abol_susp2 "test."	# Tests abolishing suspensions (OK)
../gentest.sh "$XEMU $opts" przy1 "test."	# No simplification (OK)
../gentest.sh "$XEMU $opts" przy1_simp "test."	# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" nonstrat1 "test."	# No simplification (OK)
../gentest.sh "$XEMU $opts" nonstrat2 "test."	# Needs n simplification (OK)
#----------------------------------------------------------------------
# Tests of well-founded negation.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" p1 "test."		# No simplification (OK)
../gentest.sh "$XEMU $opts" p2 "test."		# No simplification (OK)
../gentest.sh "$XEMU $opts" p3 "test."		# No simplification (OK)
../gentest.sh "$XEMU $opts" p4 "test."		# No simplification (OK)
../gentest.sh "$XEMU $opts" p5 "test."		# Needs n simplification (OK)
../gentest.sh "$XEMU $opts" simpl_win "test."	# No simplification (OK)
../gentest.sh "$XEMU $opts" win "test."		# Tests cond vs uncond. answers
					# No simplification (OK)
../gentest.sh "$XEMU $opts" cond_uncond "test."	# Tests cond vs uncond. answers
					# and early simplification (OK)
../gentest.sh "$XEMU $opts" ullman3 "test."	# Requires answers to be returned
					# from completed tables (OK)
../gentest.sh "$XEMU $opts" undef1 "test."	# To test printing of delay lists
../gentest.sh "$XEMU $opts" undef2 "test."	# To test printing of delay lists
#----------------------------------------------------------------------
# Tests of positive simplification.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" pos_simpl1 "test."	# To test printing of delay lists
#----------------------------------------------------------------------
# A mixture of negation tests.
#----------------------------------------------------------------------
#	gentest.csh "$XEMU $opts" pot_pouri "test."
#----------------------------------------------------------------------
# Tests that may require answer completion.
#----------------------------------------------------------------------
#	gentest.csh "$XEMU $opts" weidong1 "test."
../gentest.sh "$XEMU $opts" weidong2 "test."	# Requires simpl. + ans. compl. (OK)
../gentest.sh "$XEMU $opts" weidong3 "test."	# OK
../gentest.sh "$XEMU $opts" weidong4 "test."	# Requires simpl. + ans. compl. (WRONG)
../gentest.sh "$XEMU $opts" weidong5 "test."	# Requires answer completion (OK)
../gentest.sh "$XEMU $opts" weidong6 "test."	# Requires answer completion (OK)
../gentest.sh "$XEMU $opts" weidong7 "test."	# Requires answer completion (WRONG)
../gentest.sh "$XEMU $opts" weidong8 "test."	# Requires answer completion (WRONG)
#----------------------------------------------------------------------
# Tests that do require answer completion.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" interp12 "test."	# Requires answer completion (WRONG)
../gentest.sh "$XEMU $opts" interp13 "test."	# Requires answer completion (WRONG)
../gentest.sh "$XEMU $opts" interp14 "test."	# Requires answer completion (WRONG)
../gentest.sh "$XEMU $opts" interp15 "test."	# Requires answer completion (WRONG)
#----------------------------------------------------------------------
# Tests of floundering behaviour.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" avoid_flounder "test."
#----------------------------------------------------------------------
# Tests of residual program.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" residual1 "test."
#----------------------------------------------------------------------
# Tests of residual program and findall/3.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" fa "test."
#----------------------------------------------------------------------
# Tests of variables in delay list.
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" delay_var "test."
#----------------------------------------------------------------------
# Test of simplification and delete return
#----------------------------------------------------------------------
# ../gentest.sh "$XEMU $opts" dret_test "test."

../gentest.sh "$XEMU $opts" tabsimp_seq "test(20000)."
#----------------------------------------------------------------------
# Test of tnot
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" wmay_winbug "test."
#----------------------------------------------------------------------
# Test of answer completion
#----------------------------------------------------------------------
../gentest.sh "$XEMU $opts" ac_tests "test."
