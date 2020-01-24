# Linked Lists

The immutable Haskell lists or streams are great for stream processing.
However, they may not be suitable for purposes where we need to store data for
a longer while. In such cases we need mutable linked lists in pinned memory for
high performance applications i.e. we need the C like linked lists. Here are
some cases where linked-lists may be warranted instead of immutable lists:

* Let's say we want to buffer incoming data in a list. The buffered data may be
  millions of elements. When we are buffering we may allocate cells from
  different areas of the GC heap. When there are other activities going on we
  may have to keep copying this buffered data during GCs. When we consume this
  buffer, again it creates a fragmented heap and we may have to copy some other
  long-lived data to defragment the heap. The point is that we should not have
  long-lived data in the GC heap.

* When we delete a node in the list, Haskell lists have to be recreated
  generating a lot of garbage. We cannot take a reference to the unmodified
  segments and reuse them in the new list. On the other hand with mutable
  linked-lists we can delete a node cheaply. This could be a common case in a
  hash table collision chain which requires deletion of elements.

* Similar to deletion, if we need to insert an element in the middle of the
  list, an immutable list has to be re-created.

* To implement a queue, two lists in the immutable model can be used
  efficiently if we are strictly adding at the end and deleting from the front
  and if there is sufficient batching so that swapping of the lists is not a
  common operation. If we have to insert elements in the middle or if we have
  to swap too many times again we will have the same GC issues as stated above.
  For example, in implementations of priority search queues or timer wheels we
  have to mutate the lists.
