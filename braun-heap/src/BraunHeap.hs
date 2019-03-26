{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Main inspiration: http://toccata.lri.fr/gallery/braun_trees.en.html
--
-- Compared to that version though, we need to explicitly encode the m + 1 == n || m == n assumption.
--
-- I tried many approaches that wouldn't require extra data:
-- - type family IsOffset a b :: Constraint
-- - (If (m == n) m (m + 1) ~ n) => ...
-- - (and others that I can't even remember)
--
-- Maybe we'd need to encode even more assumptions into the Node constructor to
-- do that, but the 'Offset' hack works for now.
--
-- Still, I'd like to add lemmas like 1 <= 1 + (n :: Nat), otherwise there are
-- lines like `extract Empty = absurd undefined`

module BraunHeap
  (
  -- * Manipulating with a raw heap
    Heap(..)
  , empty
  , singleton
  , insert
  , merge
  , pop
  -- * Manipulating with a wrapped heap
  , SomeHeap(..)
  , insertSome
  , popSome
  , sizeSome
  , toHeap
  -- * Reexports
  , prettyShow
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.List (foldl')
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (absurd)
import GHC.TypeLits (type (+), KnownNat, Nat, natVal)
import Text.PrettyPrint.HughesPJClass
  (Pretty, (<+>), ($$), nest, pPrint, prettyShow, text)


-- | A Braun heap (a min-heap). It can be either 'Empty' or a single-element
-- 'Node' with two subtrees. The subtrees are constrained in size - they are
-- either equal, or the left one is one element larger than the right one.
data Heap (n :: Nat) a where
  Empty :: Heap 0 a
  Node :: Offset m n -> Heap m a -> a -> Heap n a -> Heap (1 + m + n) a

deriving instance Show a => Show (Heap n a)

instance Foldable (Heap n) where
  foldMap _ Empty = mempty
  foldMap f (Node _ l x r) = foldMap f l <> f x <> foldMap f r

instance Pretty a => Pretty (Heap n a) where
  pPrint Empty = text "Empty"
  pPrint (Node t l x r) =
    (text "Node" <+> pPrint t <+> pPrint x) $$
    nest 2 (pPrint l $$ pPrint r)


-- | A wrapper around a 'Heap' hiding the 'KnownNat' parameter
data SomeHeap a where
  SomeHeap :: KnownNat n => Heap n a -> SomeHeap a

instance Foldable SomeHeap where
  foldMap f = go mempty
    where
      go m h =
        case extractSome h of
          Nothing -> m
          Just (x, h') -> go (m <> f x) h'

instance Pretty a => Pretty (SomeHeap a) where
  pPrint (SomeHeap a) = pPrint a


-- | Offset of a tree node. There are only two options, the subtrees can be
-- equal-sized, or the left one can have one element more.
data Offset m n where
  Even :: Offset n n
  Leaning :: Offset (1 + n) n

deriving instance Show (Offset m n)

instance Pretty (Offset m n) where
  pPrint = text . show


-- What I'd like to write.
-- nextOffset :: forall m n. Offset m n -> If (m == n) (Offset (1 + m) m) (Offset m m)
-- nextOffset Even = Leaning :: Offset (1 + m) m
-- nextOffset Leaning = Even :: Offset m m

-- | An empty heap
empty :: Heap 0 a
empty = Empty

-- | Create a heap of size one
singleton :: a -> Heap 1 a
singleton x = Node Even Empty x Empty

-- | Insert an element into a heap, returning a one-element larger heap (as
-- evidenced by the types).
insert :: Ord a => a -> Heap n a -> Heap (1 + n) a
insert x Empty = Node Even Empty x Empty
insert x (Node o l y r)
  | x <= y = Node n (insert y r) x l
  | otherwise = Node n (insert x r) y l
  where
    n = case o of
      Leaning -> Even
      Even -> Leaning

-- | Pop the smallest element of a non-empty heap (as evidenced by the types).
pop :: Ord a => Heap (1 + n) a -> (a, Heap n a)
pop Empty = absurd undefined -- Empty != Heap (1 + n) a
pop (Node o l y r) = (y, merge o l r)

-- | Merge two heaps, possible with different sizes as evidenced by the 'Offset'
-- argument.
merge :: Ord a => Offset m n -> Heap m a -> Heap n a -> Heap (m + n) a
merge Even = mergeEven
merge Leaning = mergeLeaning

-- | Merge two same-sized Braun heaps.
mergeEven :: Ord a => Heap n a -> Heap n a -> Heap (n + n) a
mergeEven l@(Node lo ll lx lr) r@(Node _ _ ly _)
  | lx <= ly = Node Leaning r lx (merge lo ll lr)
  | otherwise = let (x, l') = extract l in Node Leaning (replaceMin x r) ly l'
mergeEven Empty _ = Empty
mergeEven _ Empty = Empty

-- | Merge two Braun heaps with first tree having one element more.
mergeLeaning :: Ord a => Heap (1 + n) a -> Heap n a -> Heap (1 + n + n) a
mergeLeaning l@(Node lo ll lx lr) r@(Node _ rl ly rr)
  | lx <= ly = Node Even r lx (merge lo ll lr)
  | otherwise = case proof r rl rr Refl of
      Refl -> let (x, l') = extract l in Node Even (replaceMin x r) ly l'
  where
    -- No, the constraint isn't redundant like GHC says, this won't compile without it
    proof :: ((y + z) ~ w) => p x a -> p y a -> p z a -> x :~: (1 + y + z) -> x :~: (1 + w)
    proof _ _ _ Refl = Refl
mergeLeaning h Empty = h
mergeLeaning Empty h = h

-- | Extract a single element from the heap. Used in 'mergeEven' and 'mergeLeaning'.
extract :: Heap (1 + n) a -> (a, Heap n a)
extract Empty = absurd undefined -- Empty != Heap (1 + n) a
extract (Node Even l y r) = case l of
  Empty -> (y, Empty)
  Node{} -> let (x, l') = extract l in (x, Node Leaning r y l')
extract (Node Leaning l y r) =
            let (x, l') = extract l in (x, Node Even r y l')

-- | Replace the smallest element of a non-empty heap.
replaceMin :: Ord a => a -> Heap (1 + n) a -> Heap (1 + n) a
replaceMin _ Empty = absurd undefined -- Empty != Heap (1 + n) a
replaceMin a (Node o Empty _ r) = Node o Empty a r
replaceMin a (Node o l@(Node _ _ lx _) _ Empty)
  | lx <= a = Node o l a Empty
  | otherwise = Node o (replaceMin a l) lx Empty
replaceMin a (Node o l@(Node _ _ lx _) _ r@(Node _ _ rx _))
  | lx <= a, rx <= a = Node o l a r
  | rx <= lx = Node o (replaceMin a l) lx r
  | otherwise = Node o l rx (replaceMin a r)

-- | Insert an element into 'SomeHeap'
insertSome :: Ord a => a -> SomeHeap a -> SomeHeap a
insertSome x (SomeHeap h) = SomeHeap (insert x h)

-- | Pop an element from 'SomeHeap', returing a Maybe
popSome :: Ord a => SomeHeap a -> Maybe (a, SomeHeap a)
popSome (SomeHeap Empty) = Nothing
popSome (SomeHeap h@Node {}) =
  let (x, h') = pop h
   in Just (x, SomeHeap h')

extractSome :: SomeHeap a -> Maybe (a, SomeHeap a)
extractSome (SomeHeap Empty) = Nothing
extractSome (SomeHeap h@Node {}) =
  let (x, h') = extract h
   in Just (x, SomeHeap h')

-- | Get the size of 'SomeHeap'
sizeSome :: SomeHeap a -> Integer
sizeSome (SomeHeap (_ :: Heap n a)) = natVal (Proxy @n)

-- | Convert a 'Foldable' container into 'SomeHeap'
toHeap :: (Foldable f, Ord a) => f a -> SomeHeap a
toHeap = foldl' (\(SomeHeap h) x -> SomeHeap (insert x h)) (SomeHeap Empty)
