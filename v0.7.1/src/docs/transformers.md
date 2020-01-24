## Using Monad Transformers

Common monad transformers can be used with streamly serial streams, without any
issues. `ReaderT` can be used with concurrent streams as well without any
issues.

The semantics of monads other than `ReaderT` with concurrent streams are
not yet finalized and will change in future, therefore as of now they are not
recommended to be used with concurrent streams.

## Ordering of Monad Transformers

In most cases it is a good idea to keep streamly as the top level monad.

## State Sharing
### Serial Applications

Read only global state can always be shared using the `Reader` monad.
Read-write global state can be shared either using an `IORef` in the `Reader`
monad or using the `State` monad.

See `AcidRain.hs` example for a usage of `StateT` in the serially executing
portion of the program.

### Concurrent Applications

The current recommended method for sharing modifiable global state across
concurrent tasks is to put the shared state inside an `IORef` in a `Reader`
monad or just share the `IORef` by passing it to the required functions. The
`IORef` can be updated atomically using `atomicModifyIORef`.

The `CirclingSquare.hs` example shares an `IORef` across parallel tasks.
