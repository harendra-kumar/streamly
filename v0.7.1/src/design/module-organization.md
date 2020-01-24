# Internal vs External Modules

We keep all modules exposed to facilitate convenient exposure of experimental
APIs and constructors to users. It allows users of the library to experiment
much more easily and carry a caveat that these APIs can change in future
without notice.  Since everything is exposed, maintainers do not have to think
about what to expose as experimental and what remains completely hidden every
time something is added to the library.

We expose the internal modules via `Streamly.Internal` namespace to keep all
the internal modules together under one module tree and to have their
documentation also separated under one head in haddock docs.

Another decision point is about two choices:

1) Keep the implementation of all the APIs in an internal module and just
reexport selected APIs through the external module. The disadvantages of this
are:
a) users may not be able to easily figure out what unexposed APIs are available
other than the ones exposed through the external module. To avoid this problem
we can mark the unexposed APIs in the docs with a special comment.
b) In tests and benchmarks we will be using internal modules to test internal
and unexposed APIs. Since exposed APIs are exported via both internal and
external modules we will have to be careful in not using the internal module
for testing accidentally, instead we should always be using the exposed module
so that we are always testing exactly the way users will be using the APIs.

2) Keep the implementations of unexposed modules in the internal module file
and exposed module in the external module file. In this approach, users can
easily figure out the unexposed vs exposed APIs. But maintaining this would
require us to move the APIs from internal to external module file whenever we
expose an API.

We choose the first approach.

# Module Name Spaces

We use the "Streamly" prefix to all the module names so that they do not
conflict with any other module on Hackage.

We have the following module hierarchy under Streamly:

* Data: All the data structures that make use of the unpinned GC memory to
  store data.  These data structures are suitable for stream processing but
  may not be suitable for storing large amounts of data in memory for longer
  durations. These are suitable for short lived and smaller structures
  because they can be moved by the GC to defragment the heap.

* Memory: This name space is for data structures that make use of the memory as
  a persistent storage device. The memory may be allocated by foreign
  allocators or pinned memory allocated by GHC. Because the memory is pinned it
  can be used for interfacing with the system/kernel. These structures are
  efficient for storing large amounts of data for longer durations because it
  does not have to be copied by the GC. These structures may not be suitable
  for small, short lived data because that is likely to fragment the heap.

* FileSystem: This name space is for data structures that reside in files
  provided by a file system interface on top of storage devices.

* Network: This name space is for APIs that access data from remote computers
  over the network.

## Data and Memory

As explained above, we distinguish two types of data structures under "Data"
and "Memory".  Alternatively, we could have used a "Memory" namespace under
each data structure e.g.  "Streamly.Data.Array.Memory" instead of using a top
level "Streamly.Memory", however, we chose to distinguish such data structures
using a top level "Memory" name space because it enforces consistent naming by
fitting all such data structures under this top level hierarchy. It also makes
it easier to find out what all data structures fall in this category.

# Module Types and Naming

## Abstract modules

Abstract modules are meant to represent an abstract interface (e.g. a type
class).  Concrete modules can make use of this interface and possibly
extend it to provide concrete functionality.

The general convention in the Haskell ecosystem for naming an abstract
interface module is to name it as "Module.Class" (e.g. Control.Monad.IO.Class).
An alternative name could be "Module.Interface".

In some other cases such modules are named after the class name (e.g. see the
array package for an example). This is more appropriate when there is no single
hierarchy where we can place the ".Class" module. For example, we have arrays
in Data.Array, Memory.Array, we have to choose one over the other to place the
".Class" module for an array abstraction. Alternatively, we can choose
"Data.IsArray" instead.

Yet another way could be to use the parent module as an interface module and
the child modules as concrete modules. For example, "Streamly.Data.Stream"
module could provide the common "Stream" type and the "IsStream" type class.
The submodule "Streamly.Data.Stream.Serial" can provide a concrete "Serial"
stream type importing the "Streamly.Data.Stream" abstract module.

## Common Modules

Some modules represent common types or utility functions that are shared across
multiple modules. The general convention is to name such modules as
"Module.Types", "Module.Common", or "Module.Core".

## Constrained Modules

Some modules represent operations on a type which constrain a type using a type
class or a specific instance of a general type.  For example, we may have a
module representing operations on a stream of any type and another module that
specifically deals with operations on a Char stream. There are two ways to deal
with this.

First is to use a submodule for the constrained type. For example,
`Streamly.Data.Stream` represents a general stream type whereas
`Streamly.Data.Stream.Char` represents operations on a stream of Char type.
This makes sense where the type we are constraining to is a specific type
rather than a type constrained using a type class.

Second is to use a separate hierarchy for the constrained type. For example, we
could use `Streamly.Data.Array` for a general array and `Streamly.Prim.Array`
for an array that works on `Prim` types. This makes sense when the type is
constrained by a type class, we may have more data structures for that
constrained type to be bundled under that hierarchy.

## Aggregate modules

In some cases we may want to aggregate the functionality of several small
modules in a combined aggregate module. In many cases, the aggregate module is
made a parent module of the constituent modules.  The parent module depends on
the child modules and exposes the functionality from the constituent modules.

## Placeholder Modules

In some cases a parent module is just a placeholder in the namespace and does
not export any functionality.
