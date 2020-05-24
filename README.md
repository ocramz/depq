# depq

Double-ended priority queues

This library provides a type for 'DEPQ's, along with functions for constructing and querying them. 

## Usage

The `Data.DEPQ` module exports the user interface, which is similar to that of most Haskell data container libraries.

Populate a DEPQ (either from a `Foldable` collection such as a list or array or by `insert`ing incrementally) and query either of its extremes (with `findMin`, `findMax`, `popMin`, `popMax`, `topK`, `bottomK`).

Have fun!

## Implementation 

Currently the implementation is based on 'IntPSQ' (integer-indexed priority search queues) from the 'psqueues' package.

