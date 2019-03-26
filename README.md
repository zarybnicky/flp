# Dependently typed Braun heap

In this repository you will find a school project, an exercise in dependent
types. The code it not intended as anything beyond an exercise, it is not
throughly tested nor, I'd assume, performant.

A Braun heap is a binary tree that satisfies the heap property, and where every
node's subtrees are either equal in size, or the left one is one larger than the
right one.

The algorithm I used is directly inspired by the work in
http://toccata.lri.fr/gallery/braun_trees.en.html, written in the Why3 language.
I would have liked to go to a primary source but, not only is this site the
first Google result for the query "braun tree", but Okasaki's "Three Algorithms
on Braun Trees" is the only paper I found that mentions the structure.

The representation I used is a GADT (A Generalized Algebraic Datatype) with the
size of the tree as a type-level parameter, and a two-value helper type Offset
to help circumvent some limitation of the Haskell type-checker. I've also
defined a wrapper type 'SomeHeap' for use in client code.

```haskell
data Heap (n :: Nat) a where
  Empty :: Heap 0 a
  Node :: Offset m n -> Heap m a -> a -> Heap n a -> Heap (1 + m + n) a

data Offset m n where
  Even :: Offset n n
  Leaning :: Offset (1 + n) n

data SomeHeap a where
  SomeHeap :: KnownNat n => Heap n a -> SomeHeap a
```

The numeric type used for representing the size of the tree is GHC's built-in
type `Nat` from (the package `GHC.TypeLits`), which is however quite
limited. (Notably, it is not inductively defined.) To circumvent some of those
limitations, I've used two core plugins (core-to-core transformations):
- `GHC.TypeLits.KnownNat.Solver` for solving trivial KnownNat constraints like
  `KnownNat n => KnownNat (1 + n)`
- `GHC.TypeLits.Normalise` for solving more elaborate proofs like `(x ~ y + z)
  => (1 + x) ~ (1 + y + z)`

Without the built-in `Nat` type, another option would be to define Peano numbers
and operations on them from scratch, which would give us greater freedom in
proofs (e.g. not requiring the KnownNat constraing everywhere), but I wanted to
see how far would the built-in literals go.

The API defines two sets of operations, one on unwrapped heaps with the 'size'
parameter exposed, and the other on the type 'SomeHeap', which hides that
parameter but returns a Maybe on the 'extract' operation.


## Build Instructions

### Nix
For [Nix](https://nixos.org/nix/), building the project should be just a matter
of cloning the repository and running `nix-build`. I didn'ŧ pin the `nixpkgs`
commits (though I should have), so this assumes that your `nixpkgs` has the
`haskell.packages.ghc844` attribute.

```shell
$ nix-build -A braun-heap
...
/nix/store/8xvjgs72pfklvgwmqkidyw04bpijq60l-braun-heap-0.1.0.0

$ result/bin/flp-braun-heap
Braun Heap

A simple CLI for manipulating an integer Braun heap with type-level constraints on subtrees

Available commands: help, add, extract, size, print

>>>
```

Haddocks are generated as well, and available as a second output of the
derivation. To build them directly, run `nix-build -A braun-heap.doc`.

### Stack
A `stack.yaml` is prepared as well, so the usual [Stack](https://docs.haskellstack.org/en/stable/README/) command should work as well.

```
$ stack build braun-heap
...
Registering library for braun-heap-0.1.0.0..

$ stack exec flp-braun-heap
Braun Heap

A simple CLI for manipulating an integer Braun heap with type-level constraints on subtrees

Available commands: help, add, extract, size, print

>>>
```

Haddocks are available as well, with the command `stack build --haddock --open braun-heap`.
