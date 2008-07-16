//tabstop=4
//*****************************************************************************/
// Project: jpl
//
// File:    $Id$
// Date:    $Date$
// Author:  Fred Dushin <fadushin@syr.edu>
//          
//
// Description:
//    
//
// -------------------------------------------------------------------------
// Copyright (c) 2004 Paul Singleton
// Copyright (c) 1998 Fred Dushin
//                    All rights reserved.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library Public License for more details.
//*****************************************************************************/
package jpl;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;
import jpl.fli.*;

//----------------------------------------------------------------------/
// Query
/**
 * A Query instance is created by an application in order to query the Prolog database
 * (or to invoke a built-in predicate).
 * It is initialised with a
 * Compound (or Atom) denoting the goal which is to be called, and also contains assorted private state
 * relating to solutions.  In some future version, it may contain details of the module
 * in which the goal is to be called.<p>
 * A Query is either open or closed: when closed, it has no connection to the Prolog system;
 * when open, it is linked to an active goal within a Prolog engine.<p>
 * The Query class implements the Enumeration interface,
 * through which one can obtain successive solutions.  The Enumeration
 * hasMoreElements() method returns true if the call or redo succeeded (otherwise
 * false), and if the call or redo did succeed, the nextElement() method returns
 * a Hashtable representing variable bindings; the elements in the
 * Hashtable are Terms, indexed by the (String) names of the Variables with which they are associated.
 * For example, if <i>p(a)</i> and <i>p(b)</i> are facts in the Prolog
 * database, then the following is equivalent to printing all
 * the solutions to the Prolog query <i>p(X)</i>:
 * <pre>
 * Variable X = new Variable("X");
 * Term arg[] = { X };
 * Query    q = new Query("p", arg);
 * 
 * while (q.hasMoreElements()){
 *     Term bound_to_x = ((Hashtable) q.nextElement()).get("X");
 *     System.out.println(bound_to_x);
 * }
 * </pre>
 * Make sure to close the Query (using the close() method) if you do not need
 * any further solutions which it may have.
 * It is safe (although redundant) to close a Query whose solutions are already exhausted,
 * or which is already closed.
 * 
 * To obtain just one solution from a Query, use the oneSolution() method.
 * 
 * To obtain all solutions, use the allSolutions() method.
 * 
 * To obtain at most N solutions, use the nSolutions() method.
 * 
 * To determine merely whether the Query is provable,
 * use the hasSolution() method
 * (i.e. has at least one solution).
 * <hr>
 * <i>
 * Copyright (C) 2007  Paul Singleton<p>
 * Copyright (C) 1998  Fred Dushin
 * <p>
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * <p>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library Public License for more details.<p>
 * </i>
 * <hr>
 * @author  Fred Dushin <fadushin@syr.edu>
 * @version $Revision$
 */
// Implementation notes:  
// 
//----------------------------------------------------------------------/
public class Query implements Enumeration {
	//==================================================================/
	//  Attributes
	//==================================================================/
	private static Map			m				= new Hashtable();	// maps (engine_t) engine handle to (Query) topmost query
	/**
	 * the Compound (hence perhaps an Atom, but not Integer, Float or Variable) corresponding to the goal of this Query
	 */
	protected final Compound	goal_;								// set by all initialisers
	protected final String		hostModule		= "user";			// until revised constructors allow this to be specified
	protected final String		contextModule	= "user";			// until revised constructors allow this to be specified
	/**
	 * @deprecated  Use .goal().name() instead.
	 * @return the name of this Query's goal (redundant, deprecated)
	 */
	public final String name() {
		return goal_.name(); // it can only be a Compound or Atom
	}
	/**
	 * @deprecated  Use .goal().args() instead.
	 * @return the arguments of this Query's goal (redundant, deprecated)
	 */
	public final Term[] args() {
		return goal_.args();
	}
	/**
	 * Returns the Compound (hence perhaps an Atom) which is the goal of this Query
	 * @return a Term representing the goal of this Query
	 */
	public final Compound goal() {
		return goal_;
	}
	//==================================================================/
	//  Constructors and Initialization
	//==================================================================/
	//------------------------------------------------------------------/
	// Query
	/**
	 * This constructor creates a Query whose goal is the specified Term.
	 * The Query is initially closed.
	 * <b>NB</b>  Creating an instance of the Query class does not
	 * result in a call to a Prolog engine.
	 * <b>NB</b>  The goal can be a Compound or an Atom (Atom extends Compound), but cannot be an instance
	 * of jpl.Float, jpl.Integer or jpl.Variable.
	 * @param   t the goal of this Query
	 */
	public Query(Term t) { // formerly insisted (confusingly) on a Compound (or Atom)
		this.goal_ = Query1(t);
	}
	private Compound Query1(Term t) {
		if (t instanceof Compound) {
			return (Compound) t;
		} else if (t instanceof Integer) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not an Integer)");
		} else if (t instanceof Float) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not a Float)");
		} else if (t instanceof Variable) {
			throw new JPLException("a Query's goal must be an Atom or Compound (not a Variable)");
		} else {
			throw new JPLException("a Query's goal must be an Atom or Compound");
		}
	}
	// Query
	/**
	 * If text denotes an atom, this constructor is shorthand for
	 * <font face="monospace">new Query(new Compound(name,args))</font>,
	 * but if text denotes a term containing N query (?) symbols
	 * and there are N args, each query is replaced by its corresponding arg
	 * to provide the new Query's goal.
	 * 
	 * @param   text  the name of the principal functor of this Query's goal
	 * @param   args  the arguments of this Query's goal
	 */
	public Query(String text, Term[] args) {
		this(Query1(text, args));
	}
	// convenience case for a single arg
	public Query(String text, Term arg) {
		this(Query1(text, new Term[] { arg }));
	}
	private static Term Query1(String text, Term[] args) {
		Term t = Util.textToTerm(text);
		if (t instanceof Atom) {
			return new Compound(text, args);
		} else {
			return t.putParams(args);
		}
	}
	// Query
	/**
	 * This constructor builds a Query from the given Prolog source text.
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @param   text  the Prolog source text of this Query
	 */
	public Query(String text) {
		this(Util.textToTerm(text));
	}
	//==================================================================/
	//  Making Prolog Queries
	//==================================================================/
	/**
	 * These variables are used and set across the hasMoreElements
	 * and nextElement Enumeration interface implementation
	 */
	private boolean		open		= false;
	// the following state variables are used and defined only if this query is open:
	// private boolean called = false; // open/get/close vs. hasMoreSolutions/nextSolution
	private engine_t	engine		= null;	// handle of attached Prolog engine iff open, else null
	private Query		subQuery	= null;	// the open Query (if any) on top of which this open Query is stacked, else null
	private predicate_t	predicate	= null;	// handle of this Query's predicate iff open, else undefined
	private fid_t		fid			= null; // id of current Prolog foreign frame iff open, else null
	private term_t		term0		= null;	// term refs of this Query's args iff open, else undefined
	private qid_t		qid			= null;	// id of current Prolog query iff open, else null
	//
	/**
	 * isOpen() returns true iff the query is open.
	 * @return	true if the query is open, otherwise false.
	 */
	public synchronized final boolean isOpen() {
		return open;
	}
	//------------------------------------------------------------------/
	// hasMoreSolutions
	/**
	 * This method returns true if JPL was able to initiate a "call" of this
	 * Query within a Prolog engine.  It is designed to be used
	 * with the nextSolution() method to retrieve one or
	 * more substitutions in the form of Hashtables.  To iterate through
	 * all the solutions to a Query, for example, one might write
	 * <pre>
	 * Query q = // obtain Query reference
	 * while (q.hasMoreSolutions()) {
	 *     Hashtable solution = q.nextSolution();
	 *     // process solution...
	 * }
	 * </pre>
	 * To ensure thread-safety, you should wrap sequential calls to
	 * this method in a synchronized block, using the static
	 * lock method to obtain the monitor.
	 * <pre>
	 * Query q = // obtain Query reference
	 * synchronized ( jpl.Query.lock() ){
	 *     while ( q.hasMoreElements() ){
	 *          Hashtable solution = q.nextSolution();
	 *          // process solution...
	 *     }
	 * }
	 * </pre>
	 * 
	 * @return  true if the Prolog query succeeds; otherwise false.
	 */
	public synchronized final boolean hasMoreSolutions() {
		if (!open) {
			open();
		}
		return get1();
	}
	//------------------------------------------------------------------/
	// open
	/**
	 * This method returns true if JPL was able to initiate a "call" of this
	 * Query within the Prolog engine.  It is designed to be used
	 * with the getSolution() and close() methods to retrieve one or
	 * more substitutions in the form of Hashtables.
	 * <pre>
	 * Query q = // obtain Query reference
	 * Hashtable soln;
	 * q.open();
	 * while ((soln = q.getSolution()) != null) {
	 *      // process solution...
	 * }
	 * </pre>
	 * <p>
	 * If this method is called on an already-open Query,
	 * or if the query cannot be set up for whatever reason,
	 * then a JPLException will be thrown.
	 */
	public synchronized final void open() {
		if (open) {
			throw new JPLException("Query is already open");
		}
		// int self = Prolog.thread_self();
		// System.out.println("JPL thread_self()=" + self);
		if (Prolog.thread_self() == -1) { // this Java thread has no attached Prolog engine?
			engine = Prolog.attach_pool_engine(); // may block for a while, or fail
			// System.out.println("JPL attaching engine[" + engine.value + "] for " + this.hashCode() + ":" + this.toString());
		} else { // this Java thread has an attached engine
			engine = Prolog.current_engine();
			// System.out.println("JPL   reusing engine[" + engine.value + "] for " + this.hashCode() + ":" + this.toString());
		}
		if (m.containsKey(new Long(engine.value))) {
			subQuery = (Query) m.get(new Long(engine.value)); // get this engine's previous topmost query
			// System.out.println("JPL   reusing engine[" + engine.value + "] pushing " + subQuery.hashCode() + ":" + subQuery.toString());
		} else {
			subQuery = null;
		}
		m.put(new Long(engine.value), this); // update this engine's topmost query
		//
		// here, we must check for a module prefis, e.g. jpl:jpl_modifier_bit(volatile,T)
		String module;
		Term goal;
		if (goal_.hasFunctor(":", 2)) {
			if (goal_.arg(1).isAtom()) {
				module = goal_.arg(1).name();
			} else if (goal_.arg(1).isVariable()) {
				throw new PrologException(Util.textParamsToTerm("error(instantiation_error,?)", new Term[] { goal_ }));
			} else {
				throw new PrologException(Util.textParamsToTerm("error(type_error(atom,?),?)", new Term[] { goal_.arg(1), goal_ }));
			}
			goal = goal_.arg(2);
		} else {
			module = contextModule;
			goal = goal_;
		}
		predicate = Prolog.predicate(goal.name(), goal.arity(), module); // was hostModule
		fid = Prolog.open_foreign_frame();
		Map varnames_to_vars = new Hashtable();
		term0 = Term.putTerms(varnames_to_vars, goal.args());
		// THINKS: invert varnames_to_Vars and use it when getting substitutions?
		qid = Prolog.open_query(Prolog.new_module(Prolog.new_atom(contextModule)), Prolog.Q_CATCH_EXCEPTION, predicate, term0);
		open = true;
		// called = false;
	}
	private final boolean get1() { // try to get the next solution; if none, close the query;
		if (Prolog.next_solution(qid)) {
			// called = true; // OK to call get2()
			return true;
		} else {
			// if failure was due to throw/1, build exception term and throw it
			term_t exception_term_t = Prolog.exception(qid);
			if (exception_term_t.value != 0L) {
				Term exception_term = Term.getTerm(new Hashtable(), exception_term_t);
				close();
				throw new PrologException(exception_term);
			} else {
				close();
				return false;
			}
		}
	}
	//------------------------------------------------------------------/
	// getSolution
	/**
	 * This method returns a java.util.Hashtable, which represents
	 * a set of bindings from the names of query variables to terms within the solution.
	 * <p>
	 * For example, if a Query has an occurrence of a jpl.Variable,
	 * say, named "X", one can obtain the Term bound to "X" in the solution
	 * by looking up "X" in the Hashtable.
	 * <pre>
	 * Variable x = new Variable("X");
	 * Query q = // obtain Query reference (with x in the Term array)
	 * while (q.hasMoreSolutions()) {
	 *     Hashtable solution = q.nextSolution();
	 *     // make t the Term bound to "X" in the solution
	 *     Term t = (Term) solution.get("X");
	 *     // ...
	 * }
	 * </pre>
	 * Programmers should obey the following rules when using this method.
	 * <menu>
	 * <li> The nextSolution() method should only be called after the
	 * hasMoreSolutions() method returns true; otherwise a JPLException
	 * will be raised, indicating that the Query is no longer open.
	 * <li> The nextSolution() and hasMoreSolutions() should be called
	 * in the same thread of execution, for a given Query
	 * instance.
	 * </menu>
	 * 
	 * This method will throw a JPLException if Query is not open.
	 * 
	 * @return  A Hashtable representing a substitution, or null
	 */
	public synchronized final Hashtable getSolution() {
		// oughta check: thread has query's engine
		if (!open) {
			throw new JPLException("Query is not open");
		} else if (get1()) {
			return get2();
		} else {
			return null;
		}
	}
	public synchronized final Hashtable getSubstWithNameVars() {
		// oughta check: thread has query's engine
		if (!open) {
			throw new JPLException("Query is not open");
		} else if (get1()) {
			return get2WithNameVars();
		} else {
			return null;
		}
	}
	//------------------------------------------------------------------/
	// nextSolution
	/**
	 * This method returns a java.util.Hashtable, which represents
	 * a binding from the names of query variables to terms within the solution.
	 * <p>
	 * For example, if a Query has an occurrence of a jpl.Variable,
	 * say, named "X", one can obtain the Term bound to "X" in the solution
	 * by looking up "X" in the Hashtable.
	 * <pre>
	 * Variable x = new Variable("X");
	 * Query q = // obtain Query reference (with x in the Term array)
	 * while (q.hasMoreSolutions()) {
	 *     Hashtable solution = q.nextSolution();
	 *     // make t the Term bound to "X" in the solution
	 *     Term t = (Term) solution.get("X");
	 *     // ...
	 * }
	 * </pre>
	 * Programmers should obey the following rules when using this method.
	 * <menu>
	 * <li> The nextSolution() method should only be called after the
	 * hasMoreSolutions() method returns true; otherwise a JPLException
	 * will be raised, indicating that the Query is no longer open.
	 * <li> The nextSolution() and hasMoreSolutions() should be called
	 * in the same thread of execution, for a given Query
	 * instance.
	 * </menu>
	 * 
	 * This method will throw a JPLException if Query is not open.
	 * 
	 * @return  A Hashtable representing a substitution.
	 */
	public synchronized final Hashtable nextSolution() {
		return get2();
	}
	private final Hashtable get2() {
		if (!open) {
			throw new JPLException("Query is not open");
		} else {
			Hashtable substitution = new Hashtable();
			// NB I reckon computeSubstitutions needn't be in Term (but where else?)
			Term.getSubsts(substitution, new Hashtable(), goal_.args);
			return substitution;
		}
	}
	// assumes that Query's last arg is a Variable which will be bound to a [Name=Var,..] dict
	private final Hashtable get2WithNameVars() {
		if (!open) {
			throw new JPLException("Query is not open");
		} else {
			Term[] args = goal_.args; // for slight convenience below
			Term argNV = args[args.length - 1]; // the Query's last arg
			String nameNV = ((Variable) argNV).name; // its name
			// get the [Name=Var,..] dict from the last arg
			Map varnames_to_Terms1 = new Hashtable();
			Map vars_to_Vars1 = new Hashtable();
			args[args.length - 1].getSubst(varnames_to_Terms1, vars_to_Vars1);
			Hashtable varnames_to_Terms2 = new Hashtable();
			Term nvs = (Term) varnames_to_Terms1.get(nameNV);
			Map vars_to_Vars2 = Util.namevarsToMap(nvs);
			for (int i = 0; i < args.length - 1; ++i) {
				args[i].getSubst(varnames_to_Terms2, vars_to_Vars2);
			}
			return varnames_to_Terms2;
		}
	}
	//------------------------------------------------------------------/
	// hasMoreElements
	/**
	 * This method implements part of the java.util.Enumeration
	 * interface.  It is a wrapper for hasMoreSolutions.
	 * 
	 * @return  true if the Prolog query yields a (or another) solution, else false.
	 */
	public synchronized final boolean hasMoreElements() {
		return hasMoreSolutions();
	}
	//------------------------------------------------------------------/
	// nextElement
	/**
	 * This method implements part of the java.util.Enumeration
	 * interface.  It is a wrapper for nextSolution.
	 * <p>
	 * 
	 * @return  A Hashtable representing a substitution.
	 */
	public synchronized final Object nextElement() {
		return nextSolution();
	}
	public synchronized final void rewind() {
		close();
	}
	/**
	 * This method can be used to close an open query before its solutions are exhausted. 
	 * It is called automatically when solutions are exhausted, i.e. when hasMoreSolutions() fails.
	 * Calling close() on an already closed Query is harmless (has no effect).<p>
	 * 
	 * Here is one way to get the first three solutions to a Query:
	 * <pre>
	 * Query q = new Query(predicate, args);
	 * Hashtable sub1 = (Hashtable) q.nextSolution();
	 * Hashtable sub2 = (Hashtable) q.nextSolution();
	 * Hashtable sub3 = (Hashtable) q.nextSolution();
	 * q.close();
	 * </pre><p>
	 */
	public synchronized final void close() {
		if (!open) {
			return; // it is not an error to attempt to close a closed Query
		}
		if (Prolog.thread_self() == -1) {
			throw new JPLException("no engine is attached to this thread");
		}
		if (Prolog.current_engine().value != engine.value) {
			throw new JPLException("this Query's engine is not that which is attached to this thread");
		}
		Query topmost = (Query) m.get(new Long(engine.value));
		if (topmost != this) {
			throw new JPLException("this Query (" + this.hashCode() + ":" + this.toString() + ") is not topmost (" + topmost.hashCode() + ":" + topmost.toString() + ") within its engine["
					+ engine.value + "]");
		}
		Prolog.close_query(qid);
		qid = null; // for tidiness
		jpl.fli.Prolog.discard_foreign_frame(fid);
		fid = null; // for tidiness
		m.remove(new Long(engine.value));
		if (subQuery == null) { // only Query open in this engine?
			if (Prolog.current_engine_is_pool()) { // this (Query's) engine is from the pool?
				Prolog.release_pool_engine();
				// System.out.println("JPL releasing engine[" + engine.value + "]");
			} else {
				// System.out.println("JPL   leaving engine[" + engine.value + "]");
			}
		} else {
			m.put(new Long(engine.value), subQuery);
			// System.out.println("JPL retaining engine[" + engine.value + "] popping subQuery(" + subQuery.hashCode() + ":" + subQuery.toString() + ")");
		}
		open = false; // this Query is now  closed
		engine = null; // this Query, being closed, is no longer associated with any Prolog engine
		subQuery = null; // this Query, being closed, is not stacked upon any other Query
	}
	/**
	 * calls the Query's goal to exhaustion
	 * and returns an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found).
	 * @return an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found)
	 * <b>NB</b> in JPL 1.0.1, this method (inconsistently) returned null when a Query had no solutions;
	 * in JPL 2.x onwards it returns an empty array (thus the length of the array is, in every case,
	 * the quantity of solutions).<p>
	 * <b>NB</b> in JPL 1.0.1, bindings were keyed (awkwardly) by Variable instances;
	 * in JPL 2.x onwards they are keyed by the (String) names of variables,
	 * which is consistent with the Term type being just a concrete syntax for terms (and hence queries).<p>
	 */
	public synchronized final Hashtable[] allSolutions() {
		if (open) {
			throw new JPLException("Query is already open");
		} else {
			// get a vector of solutions:
			Vector v = new Vector();
			while (hasMoreSolutions()) {
				v.addElement(nextSolution());
			}
			// turn the vector into an array:
			Hashtable solutions[] = new Hashtable[v.size()]; // 0 solutions -> Hashtable[0]
			v.copyInto(solutions);
			return solutions;
		}
	}
	/**
	 * This static method creates a Query whose goal is the given Term,
	 * calls it to exhaustion,
	 * and returns an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found).
	 * Throws JPLException if goal is neither a jpl.Atom nor a jpl.Compound.
	 * 
	 * @return an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found)
	 * 
	 * @param   goal the goal of this Query
	 */
	public static final Hashtable[] allSolutions(Term goal) {
		return (new Query(goal)).allSolutions();
	}
	/**
	 * This static method creates a Query from the given Prolog source text fragment,
	 * calls it to exhaustion,
	 * and returns an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found).
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @return an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found)
	 * 
	 * @param   text  a Prolog source text fragment denoting a goal
	 */
	public static final Hashtable[] allSolutions(String text) {
		return (new Query(text)).allSolutions();
	}
	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N questionmark (?) symbols and there are N accompanying Term params,
	 * this static method replaces each questionmark symbol by its respective param,
	 * calls the resulting goal to exhaustion,
	 * and returns an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found).
	 * 
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args;
	 * the resulting goal is then called as above.
	 * This letter mode is redundant, deprecated (informally), and retained only for backward compatibility.
	 * 
	 * @return an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Hashtable represents a solution, in the order in which they were found)
	 * 
	 * @param   text  the Prolog source text of a goal, in which questionmarks are regarded as substitutible parameters
	 * @param   params  terms to be substituted for the respective questionmarks in the query text
	 */
	public static final Hashtable[] allSolutions(String text, Term[] params) {
		return (new Query(text, params)).allSolutions();
	}
	/**
	 * calls the Query's goal to exhaustion or until N solutions are found, whichever is sooner,
	 * and returns an array containing (as possibly empty Hashtables of variablename-to-term bindings) every found solution (in the order in which they were found).
	 * @return an array of Hashtables (possibly none), each of which is a solution
	 * (in the order in which they were found) of the Query; at most 'n' solutions will be found and returned.
	 * <b>NB</b> in JPL 1.0.1, this method (inconsistently) returned null when a Query had no solutions;
	 * in JPL 2.x onwards it returns an empty array (thus the length of the array is, in every case,
	 * the quantity of solutions).<p>
	 * <b>NB</b> in JPL 1.0.1, bindings were keyed (awkwardly) by Variable instances;
	 * in JPL 2.x onwards they are keyed by the (String) names of variables,
	 * which is consistent with the Term type being just a concrete syntax for terms (and hence queries).<p>
	 */
	public synchronized final Hashtable[] nSolutions(long n) {
		if (open) {
			throw new JPLException("Query is already open");
		} else {
			// get a vector of solutions:
			Vector v = new Vector();
			for (long i = 0; i++ < n && hasMoreSolutions();) {
				v.addElement(nextSolution());
			}
			// turn the vector into an array:
			Hashtable solutions[] = new Hashtable[v.size()]; // 0 solutions -> Hashtable[0]
			v.copyInto(solutions);
			return solutions;
		}
	}
	/**
	 * This static method creates a Query whose goal is the given Term,
	 * calls it to exhaustion or until N solutions are found, whichever is sooner,
	 * and returns an array containing (as possibly empty Hashtables of variablename-to-term bindings) every found solution (in the order in which they were found).
	 * Throws JPLException if goal is neither a jpl.Atom nor a jpl.Compound.
	 * 
	 * @param   goal the goal of this Query
	 */
	public static final Hashtable[] nSolutions(Term goal, long n) {
		return (new Query(goal)).nSolutions(n);
	}
	/**
	 * This static method creates a Query from the given Prolog source text fragment,
	 * calls it to exhaustion or until N solutions are found, whichever is sooner,
	 * and returns an array containing (as possibly empty Hashtables of variablename-to-term bindings) every found solution (in the order in which they were found).
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @param   text  a Prolog source text fragment denoting a goal
	 */
	public static final Hashtable[] nSolutions(String text, long n) {
		return (new Query(text)).nSolutions(n);
	}
	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N questionmark (?) symbols and there are N accompanying params,
	 * this static method replaces each questionmark symbol by its respective param,
	 * calls the resulting goal to exhaustion or until N solutions are found, whichever is sooner,
	 * and returns an array containing (as possibly empty Hashtables of variablename-to-term bindings) every found solution (in the order in which they were found).
	 * 
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args;
	 * the resulting goal is then called as above.
	 * This latter mode is redundant, deprecated (informally), and retained only for backward compatibility.
	 * 
	 * @param   text  the Prolog source text of a goal, in which questionmarks are regarded as substitutible parameters
	 * @param   params  terms to be substituted for the respective questionmarks in the query text
	 */
	public static final Hashtable[] nSolutions(String text, Term[] params, long n) {
		return (new Query(text, params)).nSolutions(n);
	}
	/**
	 * Returns the first solution, if any, as a (possibly empty) Hashtable of variablename-to-term bindings, else null.
	 * 
	 * This method will throw a JPLException if this Query is already open (and the Query will remain open as before).
	 * Otherwise, upon return, the Query will be closed.
	 * @return the first solution, if the query has one, as a (possibly empty) Hashtable.
	 * If the return value is null, this means that the Query has no solutions.<p>
	 */
	public synchronized final Hashtable oneSolution() {
		if (open) {
			throw new JPLException("Query is already open");
		} else {
			Hashtable solution;
			if (hasMoreSolutions()) {
				solution = nextSolution();
				close(); // safe, whether or not this is the only solution
			} else {
				solution = null;
			}
			return solution;
		}
	}
	/**
	 * This static method creates a Query (whose goal is the specified Term)
	 * and calls it at most once, returning the first solution, if there is one, as a (possibly empty) Hashtable, else null.
	 * The goal can be a jpl.Atom or a jpl.Compound, but cannot be an instance
	 * of jpl.Float, jpl.Integer or jpl.Variable.
	 * 
	 * @param   goal the goal of this Query
	 */
	public static final Hashtable oneSolution(Term goal) {
		return (new Query(goal)).oneSolution();
	}
	/**
	 * This static method creates a Query from the given Prolog source text fragment,
	 * and calls it at most once, returning the first solution, if there is one, as a (possibly empty) Hashtable, else null.
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @param   text  a Prolog source text fragment denoting a goal
	 */
	public static final Hashtable oneSolution(String text) {
		return (new Query(text)).oneSolution();
	}
	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N questionmark (?) symbols
	 * and there are N params, each questionmark symbol is replaced by its respective param
	 * to provide the goal of this query:
	 * the resulting goal is then called (at most once) and the first solution, if there is one, is returned as a (possibly empty) Hashtable, else null.
	 * 
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of the goal and params are the args;
	 * the resulting goal is then called as above.
	 * This latter mode is redundant, deprecated (informally), and retained only for backward compatibility.
	 * 
	 * @param   text  the Prolog source text of a goal, in which questionmarks are regarded as substitutible parameters
	 * @param   params  terms to be substituted for the respective questionmarks in the query text
	 */
	public static final Hashtable oneSolution(String text, Term[] params) {
		return (new Query(text, params)).oneSolution();
	}
	/**
	 * This method will attempt to call this Query's goal within an available Prolog engine.
	 * @return the provability of the Query, i.e. 'true' if it has at least
	 * one solution, 'false' if the call fails without finding a solution.<p>
	 * 
	 * Only the first solution (if there is one) will be found;
	 * any bindings will be discarded, and the Query will be closed.<p>
	 * This method will throw a JPLException if this Query is already open.
	 * 
	 * @deprecated  Use .hasSolution() instead.
	 */
	public synchronized final boolean query() {
		return oneSolution() != null;
	}
	/**
	 * This method will attempt to call this Query's goal within an available Prolog engine.
	 * @return the provability of the Query, i.e. 'true' if it has at least
	 * one solution, 'false' if the call fails without finding a solution.<p>
	 * 
	 * Only the first solution (if there is one) will be found;
	 * any bindings will be discarded, and the Query will be closed.<p>
	 * This method will throw a JPLException if this Query is already open.
	 */
	public synchronized final boolean hasSolution() {
		return oneSolution() != null;
	}
	/**
	 * This static method creates a Query (whose goal is the specified Term)
	 * and calls it at most once, returning true if a solution was found, else false.
	 * The goal can be a jpl.Atom or a jpl.Compound, but cannot be an instance
	 * of jpl.Float, jpl.Integer or jpl.Variable.
	 * 
	 * @param   goal the goal of this Query
	 */
	public static final boolean hasSolution(Term goal) {
		return (new Query(goal)).hasSolution();
	}
	/**
	 * This static method creates a Query from the given Prolog source text
	 * and calls it at most once, returning true if a solution was found, else false.
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 * 
	 * @param   text the goal of this Query, as Prolog source text
	 */
	public static final boolean hasSolution(String text) {
		return (new Query(text)).hasSolution();
	}
	/**
	 * If text denotes (in traditional Prolog source syntax) a term containing N questionmark (?) symbols
	 * and there are N params, each questionmark symbol is replaced by its corresponding arg
	 * to provide the new Query's goal: the resulting Query is called as described above.
	 * 
	 * Otherwise, if text denotes an atom, this static method creates a Query
	 * where text is the name of its goal and args are its args;
	 * it then calls this goal (at most once) and returns true if a solution was found, else false.
	 * This latter mode is redundant, deprecated (informally), and retained only for backward compatibility.
	 * 
	 * @param   text  the Prolog source text of a goal, in which questionmarks are regarded as substitutible parameters
	 * @param   params  terms to be substituted for the respective questionmarks in the query text
	 */
	public static final boolean hasSolution(String text, Term[] params) {
		return (new Query(text, params)).hasSolution();
	}
	//
	// this method doesn't work, but is intended to be called from another thread,
	// to abort a Query which is open and possibly currently executing nextSolution() or similar
	public final int abort() {
		if (open) {
			(new Thread(new Runnable() {
				public void run() {
					try {
						int rc1 = Prolog.attach_engine(engine);
						System.out.println("q.abort(): attach_engine() returns " + rc1);
						int rc2 = Prolog.action_abort();
						System.out.println("q.abort(): action_abort() returns " + rc2);
						// int rc3 = Prolog.release_pool_engine();
						// System.out.println("q.abort(): release_pool_engine() returns " + rc3);
					} catch (Exception e) {
					}
				}
			})).start(); // call the query in a separate thread
			/*
			 int rc0a = Prolog.pool_engine_id(this.engine);
			 System.out.println("q.abort(): this.engine has id=" + rc0a);

			 engine_t e = Prolog.current_engine();
			 System.out.println("q.abort(): " + (e == null ? "no current engine" : "current engine id=" + Prolog.pool_engine_id(e)));

			 int rc0b = Prolog.release_pool_engine();
			 System.err.println("q.abort(): release_pool_engine() returns " + rc0b);

			 engine_t e2 = Prolog.current_engine();
			 System.out.println("q.abort(): " + (e == null ? "no current engine" : "current engine id=" + Prolog.pool_engine_id(e2)));

			 int rc1 = Prolog.attach_engine(this.engine);
			 System.out.println("q.abort(): attach_engine() returns " + rc1);

			 engine_t e3 = Prolog.current_engine();
			 System.out.println("q.abort(): " + (e == null ? "no current engine" : "current engine id=" + Prolog.pool_engine_id(e3)));

			 int rc2 = Prolog.action_abort();
			 System.out.println("q.abort(): action_abort() returns " + rc2);

			 int rc3 = Prolog.release_pool_engine();
			 System.out.println("q.abort(): release_pool_engine() returns " + rc3);

			 int rc4 = Prolog.attach_engine(e);
			 System.out.println("q.abort(): attach_engine() returns " + rc4);
			 */
			return 0;
		} else {
			System.out.println("q.abort(): query is not open");
			return -1;
		}
	}
	//==================================================================/
	//  misc
	//==================================================================/
	/**
	 * Returns a crude String representation of a Query.
	 * 
	 * @return  a crude String representation of a Query
	 */
	public String toString() {
		return goal_.name + "( " + Term.toString(goal_.args) + " )";
	}
	//==================================================================/
	//  Methods (deprecated)
	//==================================================================/
	/**
	 * Returns a debug-friendly representation of a Query
	 * 
	 * @return  a debug-friendly representation of a Query
	 * @deprecated
	 */
	public String debugString() {
		return "(Query " + goal_.name + " " + Term.debugString(goal_.args) + ")";
	}
}
