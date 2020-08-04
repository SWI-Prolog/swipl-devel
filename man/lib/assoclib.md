
## Introduction {#assoc-introduction}

An _association list_ as implemented by this library is a collection
of unique _keys_ that are associated to _values_. Keys must be ground,
values need not be.

An association list can be used to _fetch_ elements via their keys and
to _enumerate_ its elements in ascending order of their keys.

This library uses **AVL trees** to implement association lists. This means that

  - inserting a key
  - changing an association
  - fetching a single element

are all _O(log(N))_ _worst-case_ (and expected) time operations, where
_N_ denotes the number of elements in the association list.

The logarithmic overhead is often acceptable in practice. Notable
advantages of association lists over several other methods are:

  - `library(assoc)` is written entirely in Prolog, making it portable to
    other systems
  - the interface predicates fit the declarative nature of Prolog, avoiding
    destructive updates to terms
  - AVL trees scale very predictably and can be used to represent sparse arrays
    efficiently.


## Creating association lists {#assoc-creation}

An association list is _created_ with one of the following predicates:

  * [[empty_assoc/1]]
  * [[list_to_assoc/2]]
  * [[ord_list_to_assoc/2]]

## Querying association lists {#assoc-querying}

An association list can be _queried_ with:

  * [[get_assoc/3]]
  * [[get_assoc/5]]
  * [[max_assoc/3]]
  * [[min_assoc/3]]
  * [[gen_assoc/3]]

## Modifying association lists {#assoc-modifications}

Elements of an association list can be changed and inserted with:

  * [[put_assoc/4]]
  * [[del_assoc/4]]
  * [[del_min_assoc/4]]
  * [[del_max_assoc/4]]

## Conversion predicates {#assoc-conversion}

Conversion of (parts of) an association list to _lists_ is possible
with:

  * [[assoc_to_list/2]]
  * [[assoc_to_keys/2]]
  * [[assoc_to_values/2]]

## Reasoning about association lists and their elements {#assoc-reasoning}

Further inspection predicates of an association list and its elements
are:

  * [[is_assoc/1]]
  * [[map_assoc/2]]
  * [[map_assoc/3]]

