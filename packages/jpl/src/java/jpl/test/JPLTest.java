/*
 * JPLTest.java
 * JUnit based test
 *
 * Created on 13 February 2006, 11:31
 */
package jpl.test;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import junit.framework.*;
import jpl.*;

/**
 *
 * @author rick
 */
public class JPLTest extends TestCase {
	// private static final Logger	logger	= Logger.getLogger(JPLTest.class.getName());
	private CountDownLatch	latch;
	public JPLTest(String testName) {
		super(testName);
	}
	protected void setUp() throws Exception {
		/* 
		 * Prolog file can be an empty file.  The JVM seems to crash with a
		 * SIGSEGV if you don't consult a file prior to interacting with JPL.
		 
		 final String prologFile = "jpl/test/test.pl"; // was "/home/rick/temp/test.pl";
		 System.out.println("prolog file is: " + prologFile);
		 String qString = "consult('" + prologFile + "')";
		 System.out.println("about to: " + qString);
		 Query query = new Query(qString);
		 System.out.println("Generated Query: " + query);
		 if (!query.hasSolution()) {
		 System.out.println(qString + " failed");
		 fail("Failed to consult prolog file.");
		 }
		 
		(new Query("true")).hasSolution();
		 */
	}
	public void testThreadedAdds() {
		latch = new CountDownLatch(4);
		final AddWithThreads[] addTasks = { new AddWithThreads("a", latch), new AddWithThreads("b", latch), new AddWithThreads("c", latch), new AddWithThreads("d", latch) };
		// System.out.println("Starting threads...");
		for (int i = 0; i < addTasks.length; i++) {
			addTasks[i].start();
		}
		try {
			// System.out.println("Latch is waiting");
			assertTrue("Timed out waiting for action to execute", latch.await(20, TimeUnit.SECONDS));
			// System.out.println("Latch has been flipped");
		} catch (final InterruptedException e) {
			fail("Waiting thread was interrupted: " + e);
		}
		for (int i = 0; i < AddWithThreads.REPS; i++) {
			for (int j = 0; j < addTasks.length; j++) {
				Query query = new Query(addTasks[j].getNamespace() + "(test('" + i + "'))");
				// System.out.println("query: " + query);
				boolean ret = query.hasMoreElements();
				query.close();
			}
		}
	}
}

class AddWithThreads extends Thread {
	private final CountDownLatch	latch;
	private final String			namespace;
	private static final Logger		logger	= Logger.getLogger(JPLTest.class.getName());
	public static final int			REPS	= 2000; // was 200
	public AddWithThreads(final String namespace, final CountDownLatch latch) {
		this.latch = latch;
		this.namespace = namespace;
		setName("namespace" + namespace); //set thread name for debugging
	}
	public String getNamespace() {
		return namespace;
	}
	public void run() {
		for (int i = 0; i < REPS; i++) {
			// System.out.println("Asserting test('" + i + "')");
			Query queryA = new Query("assert(" + namespace + "(test('" + i + "')))");
			Thread.yield();
			// System.out.println("adding query: " + queryA);
			boolean retA = queryA.hasMoreElements();
			queryA.close();
		}
		latch.countDown();
	}
}
