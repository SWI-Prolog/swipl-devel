\libdoc{heaps}{Heaps}
\label{sec:lib:heaps}

\makebox[\linewidth]{\hfill Author: \emph{Richard A.
O'Keefe}}\vspace{2ex}

\noindent
A heap is a collection of Key-Datum pairs, represented as a labelled
binary tree where the key of each node is less than or equal to the
keys of its sons. Heaps can be used as priority queues.


\begin{description}
	\predicate{add_to_heap}{4}{+OldHeap, +Key, +Datum, -NewHeap}
		Inserts the new \arg{Key}-\arg{Datum} pair into \arg{OldHeap},
		giving \arg{NewHeap}. The insertion is not stable, that is,
		if you insert several pairs with the same \arg{Key} it is not
		defined which of them will come out first.

	\predicate{empty_heap}{1}{?Heap}
		Is true when \arg{Heap} is the empty heap.

	\predicate{get_from_heap}{4}{+OldHeap, ?Key, ?Datum, -NewHeap}
		Returns the \arg{Key}-\arg{Datum} pair in \arg{OldHeap}
		with the smallest \arg{Key}, and also a \arg{NewHeap}
		which is the \arg{OldHeap} with that pair deleted.

	\predicate{heap_size}{2}{+Heap, ?Size}
		\arg{Size} is the number of elements in \arg{Heap}.

	\predicate{heap_to_list}{2}{+Heap, -List}
		Returns the current set of Key-Datum pairs in \arg{Heap}
		as a List, sorted into ascending order of Keys.

	\predicate{list_to_heap}{2}{+List, -Heap}
		Takes a list of Key-Datum pairs and turns them into heap
		\arg{Heap}.

	\predicate{min_of_heap}{3}{+Heap, ?Key, ?Datum}
		Returns the \arg{Key}-\arg{Datum} pair of \arg{Heap}
		with minimal \arg{Key} without removing it. Fails if the heap
		is empty.

	\predicate{min_of_heap}{5}{+Heap, ?Key1, ?Datum1, ?Key2, ?Datum2}
		Returns the smallest (\arg{Key1}-\arg{Datum1}) and second
		smallest (\arg{Key2}-\arg{Datum2}) pairs in heap
		\arg{Heap}, without deleting them. It fails if the heap
		does not have at least two elements.

\end{description}

%end-of-file
