Multi-threading XPCE

Message Passing
===============

* Move global-data to thread-local data.  These are the 3 variables defined
  in trace.h.  Simple


Program objects
===============

* Simplest: lock resolveImplementationGoal()
  drawback: executes obtainers and other potentionally expensive code that
	    need not lock

* Alternative:
	Lock class-access methods:
	    getSendMethodClass()
	    getGetMethodClass()
	Also manipulation methods on same lock:
	    realising/method adding operations

Graphics
========

Make state maintained by xdraw.c and msdraw.c thread-safe.  Options:

	* Lock d_*() ... d_done() sequences

	* Use thread-local-data graphical resources

	* Make changedWindow() and display-manipulation thread-safe.


User objects
============

* Lock at the window level while changes are pending:
  When changing a graphical, grab the window-mutex.

	* GrabWindow
	* Window is released after it is updated and repainted (i.e. ->flush)

* User objects:

	+ leave to user (simple)
	+ Provide basic security on data objects (chains, vector, table)

	  - `ForAll' can take long and risks deadlocks!

	+ Allow objects to have a mutex associated?

	- send(Obj, mutex, {write,all,none})
	   - write: lock sends to this object
	   - read:  lock all

Work-threads
============


