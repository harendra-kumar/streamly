Structured Loops (Direct Style)
===============================

::

  -- | Result of taking a single step in a stream
  data Step s a where
    Yield :: a -> s -> Step s a
    Stop  :: Step s a

  -- | Representation of a loop, step and state. Step returns the next value and
  -- the next state. We iterate on the state to keep producing more values.
  data Stream m a = forall s. Stream (s -> m (Step s a)) s

Generating
----------

::

  nil :: Monad m => Stream m a
  nil = Stream (const (return Stop)) ()

  -- | A single value
  value :: Applicative m => a -> Stream m a
  value x = Stream step True

      where

      step True  = return $ Yield x False
      step False = return $ Stop

  fromList :: Applicative m => [a] -> Stream m a
  fromList = Stream step

      where

      step _ (x:xs) = pure $ Yield x xs
      step _ []     = pure Stop

Eliminating
-----------

::

  foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
  foldrM f z (Stream step state) = go state

      where

      go st = do
            r <- step st
            case r of
              Yield x s -> f x (go s)
              Stop      -> z

In the above code the state is explicit and being threaded around in a
recursive loop. The state from the previous iteration is to be consumed
by the next iteration to generate the next value.  This mandates a
closed recursive loop. This is straightforward translation of imperative
loops to functional paradigm.

Transforming Loops
------------------

::

  mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
  mapM f (Stream step state) = Stream step' state

      where

      step' st = do
          r <- step st
          case r of
              Yield x s -> f x >>= \a -> return $ Yield a s
              Stop      -> return Stop

State wrapping
--------------

Some operations like "drop" (uniq, intersperse, deleteBy, insertBy) may
have to introduce a branch in the code by wrapping the state in another
layer::

  drop :: Monad m => Int -> Stream m a -> Stream m a
  drop n (Stream step state) = Stream step' (state, Just n)

      where

      step' (st, Just i)
        | i > 0 = do
            r <- step st
            return $
              case r of
                Yield _ s -> step' (s, Just (i - 1))
                Stop      -> Stop
        | otherwise = step' (st, Nothing)

      step' (st, Nothing) = do
        r <- step st
        return $
          case r of
            Yield x s -> Yield x (s, Nothing)
            Stop      -> Stop

Note that the branch is always checked for every element in the stream.
It is still quite efficient because the code fuses.

We should use rewrite rules to fuse such consecutive operations together as
they introduce branching which could be costly.

Composing Loops
---------------

Composing two independent loops together is not scalable, we need
to create a wrapper state, wrapping the state of the old stream:

::

  cons :: Monad m => m a -> Stream m a -> Stream m a
  cons m (Stream step state) = Stream step1 Nothing

      where

      step1 Nothing   = m >>= \x -> return $ Yield x (Just state)
      step1 (Just st) = do
          r <- step st
          return $
            case r of
              Yield a s -> Yield a (Just s)
              Stop      -> Stop

As we keep consing we keep creating more layers wrapping the state in
``Maybe``. These layers need to be traversed every time we run the step
function of the composed stream. In the above example ``step1`` needs
to always branch on ``Nothing/Just`` to reach to the wrapped stream. More
layers we add the more branching needs to occur to generate an element
of the stream. If we have ``n`` "cons" operations we need to go through:

* 1 branch at the top level to generate the first element
* 2 branches to generate the next element
* 3 branches to generate the third element
* n branches to generate the nth element

The total number of branches that we need to take is: ``1 + 2 + 3 ... n`` i.e.
``n * (n + 1)/2 = O(n^2)`` where ``n`` is the number of cons operations.

Conceptually, to avoid the introduction of a branch we could use a
mutable step function and state to modify step1/state after yielding
the first element. The next time we call it, it would be a different
function that i.e. "step" and its state "st". However, that would introduce
an indirection and mutability. There is a better way to do it with
immutability i.e. CPS.

CPS representation
==================

In the direct representation we represented a stream using a step and a
state. This model requires us to iterate the step on the state creating
an explicit loop. The state machine implemented by the step function is
incrementally modified by adding new layers in the state which introduce
branches to be traversed every time we go through the loop.

::

  newtype Stream m a = Stream
      { runStream :: forall r. (a -> Stream m a -> m r) -> m r -> m r }

Here we represent the stream as a single function.  Instead, the
function is provided with functions to be called next.

Notice, the function does not have to be called again and again to
iterate on a state for generating new values.  Therefore, there is no
closed recursion. There is no explicit loop.

We (the current ``runStream`` function) can choose which one of the supplied
functions (continuations) to call next.  If we decide to terminate
the stream execution we call the "stop" continuation. If we decide to
generate a value we call the "yield" continuation.

A stream execution is composed of a progression of such continuations
until one of those decides to call the stop continuation.  It is a
composition of functions, a tree of functions composed together.

Yield continuation
------------------

The yield continuation is provided with the generated value "a" and the
"Stream m a", the function representing the rest of the stream. Notice
that the stream function is done with one shot execution, there is no
closed loop or recursion, the future execution of the stream is the
responsibility of the continuation.

The continuation consumes the element "a" and then proceeds to call
"Stream m a" using a "yield" and "stop" continuation. By modifying the
"yield" and "stop" continuations that it passes to call "Stream m a", it
can control the execution of the stream.

Generating
----------

::

  nil :: Stream m a
  nil = Stream $ \_ stp -> stp

  value :: a -> Stream m a
  value a = Stream $ \val _ -> val a nil

  cons :: a -> Stream m a -> Stream m a
  cons a r = Stream $ \yld _ -> yld a r

  fromList :: [a] -> Stream m a
  fromList = Prelude.foldr cons nil

Eliminating
-----------

::

  foldrM :: (a -> m b -> m b) -> m b -> Stream m a -> m b
  foldrM step acc m = go m
      where
      go m1 =
          let stop = acc
              yieldk a r = step a (go r)
          in runStream yieldk stop m1

Note that unlike in direct style fold, there is no generator state being
threaded around here instead the function yielded by the continuation is
being executed.

Transforming
------------

::

  map :: (a -> b) -> Stream m a -> Stream m b
  map f = go

      where

      go m1 =
          Stream $ \yld stp ->
            let yieldk a r = yld (f a) (go r)
            in runStream yieldk stp m1

In direct style we had to examine the constructors to determine the
current state and execute code based on that. Here, we have to make
the next function call at each step. The former is much more efficient
because the compiler can optimize well to remove the constructors and
generate code with direct branches not involving the constructors. On
the other hand placing a function call is costlier. Though in some cases
it can be avoided by using foldr/build fusion but not always.

goto
----

::

  drop :: Int -> Stream m a -> Stream m a
  drop n = Stream $ go n

      where

      go n1 m1 =
        Stream $ \yld stp ->
          let yieldk _ r = runStream yld stp $ go (n1 - 1) r
          in if n1 <= 0
             then runStream yld stp m1
             else runStream yieldk stp m1

This shows a crucial difference between direct style and CPS. In direct
style we have to always check for the branch to determine if we are
dropping the elements or consuming. In this case we can see that once
we have taken the "then" path we never have to check the condition "n1
<= 0", it is out of the way.

Basically CPS provides us the ability to take an exit path and forget
about the past code forever. So if we have a million "drop" composed
together CPS would have no problem, after the final drop there won't be
any branches in the way whereas direct style would introduce a million
branches to be traversed forever.

Combining Streams
-----------------

::

  cons :: a -> Stream m a -> Stream m a
  cons a r = Stream $ \yld _ -> yld a r

Unlike in direct style representation, the performance of stream
generation is independent of the number of "cons" operations. There is
no quadratic complexity, we simply call the next continuation at each
step.

Similarly appending streams is independent of number of appends.

::

  serial :: Stream m a -> Stream m a -> Stream m a
  serial m1 m2 = go m1

      where

      go m =
        Stream $ \yld stp ->
           let stop       = runStream yld stp m2
               yieldk a r = yld a (go r)
           in runStream yieldk stop m

Interleaved production and consumption
--------------------------------------

In the direct style we have an explicit stream generator. A consumer
generates values using the generator and consumes them. In CPS the
consumer and generator are interleaved. The ``runStream`` function is a
generator and the "yield" continuation is a consumer. The consumer then
calls the generator again and so on.

Loop vs goto
------------

The direct style representation is like a structured loop with well
defined exit points. Whereas the CPS representation can exit from
anywhere. Therefore, exception handling and resource management in
direct style is much simpler to implement.
