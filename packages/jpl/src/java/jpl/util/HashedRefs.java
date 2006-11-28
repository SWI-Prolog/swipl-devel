package jpl.util;


/**
 * HashedRefs collision list.
 */
class HashedRefsEntry {
    int hash;
    Object obj;
    public int iref;
    public HashedRefsEntry next;
}


public
class HashedRefs {
    /**
     * The hash table data.
     */
    public transient HashedRefsEntry table[];

    /**
     * The total number of entries in the hash table.
     */
    private transient int count;

    /**
     * Rehashes the table when count exceeds this threshold.
     */
    private int threshold;

    /**
     * The load factor for the hashtable.
     */
    private float loadFactor;

    public HashedRefs(int initialCapacity, float loadFactor) {
	if ((initialCapacity <= 0) || (loadFactor <= 0.0)) {
	    throw new IllegalArgumentException();
	}
	this.loadFactor = loadFactor;
	table = new HashedRefsEntry[initialCapacity];
	threshold = (int)(initialCapacity * loadFactor);
    }

    public HashedRefs(int initialCapacity) {
	this(initialCapacity, 0.75f);
    }

    public HashedRefs() {
	this(101, 0.75f);
    }

    public int size() {
	return count;
    }

    protected void rehash() {
	int oldCapacity = table.length;
	HashedRefsEntry oldTable[] = table;

	int newCapacity = oldCapacity * 2 + 1;
	HashedRefsEntry newTable[] = new HashedRefsEntry[newCapacity];

	threshold = (int)(newCapacity * loadFactor);
	table = newTable;

	for (int i = oldCapacity ; i-- > 0 ;) {
	    for (HashedRefsEntry old = oldTable[i] ; old != null ; ) {
		HashedRefsEntry e = old;
		old = old.next;

		int index = (e.hash & 0x7FFFFFFF) % newCapacity;
		e.next = newTable[index];
		newTable[index] = e;
	    }
	}
    }

    public synchronized int add(Object obj, int iref) {
	// Make sure the object reference is not null
	if (obj == null) {
	    throw new NullPointerException();
	}

	// check whether object is already in the hashtable...
	HashedRefsEntry tab[] = table;
	int hash = java.lang.System.identityHashCode(obj);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (HashedRefsEntry e = tab[index] ; e != null ; e = e.next) {
	    if ((e.hash == hash) && (e.obj == obj)) {
		return e.iref;		// existing global reference to this object
	    }
	}

	if (count >= threshold) {
	    // Rehash the table if the threshold is exceeded
	    rehash();
	    return add(obj, iref);
	} 

	// create a new entry...
	HashedRefsEntry e = new HashedRefsEntry();
	e.hash = hash;
	e.obj = obj;
	e.iref = iref;
	e.next = tab[index];
	tab[index] = e;
	count++;
	return 0;			// indicates this reference has been added
    }

    public synchronized boolean del(Object obj) {
	HashedRefsEntry tab[] = table;
	int hash = java.lang.System.identityHashCode(obj);
	int index = (hash & 0x7FFFFFFF) % tab.length;
	for (HashedRefsEntry e = tab[index], prev = null ; e != null ; prev = e, e = e.next) {
	    if ((e.hash == hash) && (e.obj == obj)) {
		if (prev != null) {
		    prev.next = e.next;
		} else {
		    tab[index] = e.next;
		}
		count--;
		return true;
	    }
	}
	return false;
    }

    public synchronized void clear() {
	HashedRefsEntry tab[] = table;
	for (int index = tab.length; --index >= 0; )
	    tab[index] = null;
	count = 0;
    }

}
