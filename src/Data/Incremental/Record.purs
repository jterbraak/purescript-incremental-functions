module Data.Incremental.Record
  ( IRecord(..)
  , get
  , update
  , class MonoidRL
  , memptyRL
  , class SemigroupRL
  , appendRL
  , class PatchRL
  , patchRL
  ) where

import Prelude

import Data.Incremental (class Patch, Change, Jet, fromChange, patch, toChange)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))

newtype IRecord r = IRecord (Record r)

derive instance newtypeIRecord :: Newtype (IRecord a) _

instance ( RowToList r rl, SemigroupRL rl r)
    => Semigroup (IRecord r) where
  append (IRecord x) (IRecord y) = IRecord (appendRL ( Proxy :: _ rl) x y)

class SemigroupRL (rl :: RowList Type ) r | rl -> r where
  appendRL :: Proxy rl -> Record r -> Record r -> Record r

instance SemigroupRL Nil () where
  appendRL _ _ _ = {}

instance
    ( IsSymbol l
    , Semigroup a
    , SemigroupRL rl r1
    , Row.Cons l a r1 r2
    , Row.Lacks l r1
    ) => SemigroupRL ( Cons l a rl) r2
    where
    appendRL _ x y =
        Record.insert l
            (append (Record.get l x) (Record.get l y))
            rest
        
        where
        
        l = Proxy :: _ l

        rest :: Record r1
        rest = appendRL (Proxy :: Proxy rl) (Record.delete l x) (Record.delete l y)

instance ( RowToList r rl, MonoidRL rl r ) => Monoid ( IRecord r ) where
  mempty = IRecord (memptyRL (Proxy :: Proxy rl))

class SemigroupRL rl r <= MonoidRL (rl :: RowList Type ) r | rl -> r where
  memptyRL :: Proxy rl -> Record r

instance MonoidRL Nil () where
  memptyRL _ = {}

instance 
    ( IsSymbol l
    , Monoid a
    , MonoidRL rl r1
    , Row.Cons l a r1 r2
    , Row.Lacks l r1
    ) => MonoidRL (Cons l a rl) r2
    where
    memptyRL _ =
        Record.insert l mempty rest
        
        where
        
        l = Proxy :: Proxy l

        rest :: Record r1
        rest = memptyRL (Proxy :: Proxy rl)

instance (RowToList r rl, RowToList d dl, MonoidRL dl d, PatchRL r rl d dl)
    => Patch (IRecord r) (IRecord d) where
  patch (IRecord r) (IRecord d) = IRecord (patchRL (Proxy :: Proxy rl) (Proxy :: Proxy dl) r d)

class MonoidRL dl d <= PatchRL r (rl :: RowList Type ) d (dl :: RowList Type ) | rl -> r, dl -> d, rl -> dl where
  patchRL :: Proxy rl -> Proxy dl -> Record r -> Record d -> Record r

instance PatchRL () Nil () Nil where
  patchRL _ _ _ _ = {}

instance
    ( IsSymbol l
    , Patch a m
    , PatchRL r1 rl d1 dl
    , Row.Cons l a r1 r2
    , Row.Cons l m d1 d2
    , Row.Lacks l r1
    , Row.Lacks l d1
    ) => PatchRL r2 ( Cons l a rl ) d2 ( Cons l m dl )
    where
    patchRL _ _ x y =
        Record.insert l
            (patch (Record.get l x) (Record.get l y))
            rest
        
        where
        
        l = Proxy :: Proxy l

        rest :: Record r1
        rest = patchRL (Proxy :: Proxy rl) (Proxy :: Proxy dl) (Record.delete l x) (Record.delete l y)

-- | An incremental property accessor function
get :: forall l a da r rl rest1 rest2 d dl
   . IsSymbol l
  => Row.Cons l a rest1 r
  => Row.Cons l da rest2 d
  => RowToList r rl
  => RowToList d dl
  => PatchRL r rl d dl
  => Patch a da
  => Proxy l
  -> Jet (IRecord r)
  -> Jet a
get l { position, velocity } =
  { position: Record.get l (unwrap position)
  , velocity: toChange (Record.get l (unwrap (fromChange velocity)))
  }

-- | An incremental property update function
update :: forall l a da r rl rest1 rest2 d dl
   . IsSymbol l
  => Row.Cons l a rest1 r
  => Row.Cons l da rest2 d
  => RowToList r rl
  => RowToList d dl
  => PatchRL r rl d dl
  => Patch a da
  => Proxy l
  -> Change a
  -> Change (IRecord r)
update l c = toChange (wrap (Record.set l (fromChange c) (unwrap (mempty :: IRecord d))))
