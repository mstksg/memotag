{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.MemoTag (
    MemoTag (_mtFunc, _mtValue, _mtResult)
  , mkMemoTag
  , mtValue
  , mtValue'
  , mtFunc
  , mtResult
  , mtTuple
  ) where

import Data.Profunctor
import Data.Functor.Contravariant
import Data.Monoid
import Data.Ord
import Data.Function
import Control.Comonad.Store.Class
import Control.Comonad

data MemoTag a b = MemoTag { _mtFunc   :: !(a -> b)
                           , _mtValue  :: !a
                           , _mtResult :: !b
                           }

instance Functor (MemoTag a) where
    fmap g (MemoTag f v r) = MemoTag (g . f) v (g r)

instance Comonad (MemoTag a) where
    extract = _mtResult
    duplicate mt@(MemoTag f v _) = MemoTag (mkMemoTag f) v mt
    extend g mt@(MemoTag f v _)  = MemoTag (g . mkMemoTag f) v (g mt)

instance ComonadStore a (MemoTag a) where
    pos = _mtValue
    peek v (MemoTag f _ _) = f v
    peeks g (MemoTag f v _) = f (g v)
    seek v (MemoTag f _ _) = MemoTag f v (f v)
    seeks g (MemoTag f v _) = let v' = g v
                              in  MemoTag f v' (f v')

instance Eq b => Eq (MemoTag a b) where
    (==) = on (==) _mtResult

instance Ord b => Ord (MemoTag a b) where
    compare = comparing _mtResult

mkMemoTag :: (a -> b) -> a -> MemoTag a b
mkMemoTag f v = MemoTag f v (f v)

-- mtValue :: Lens' (MemoTag a b) a
mtValue :: Functor f => (a -> f a) -> MemoTag a b -> f (MemoTag a b)
mtValue g (MemoTag f v _) = mkMemoTag f <$> g v

-- mtValue' :: Iso' (MemoTag a b) a
mtValue' :: (a -> b) -> forall p f. (Profunctor p, Functor f) => p a (f a) -> p (MemoTag a b) (f (MemoTag a b))
mtValue' f = dimap _mtValue (mkMemoTag f <$>)

-- mtFunc :: Lens (MemoTag a b) (MemoTag a c) (a -> b) (a -> c)
mtFunc :: Functor f => ((a -> b) -> f (a -> c)) -> MemoTag a b -> f (MemoTag a c)
mtFunc g (MemoTag f v _) = flip mkMemoTag v <$> g f

-- mtResult :: Getter (MemoTag a b) b
mtResult :: (Contravariant f, Functor f) => (b -> f b) -> MemoTag a b -> f (MemoTag a b)
mtResult g mt@(MemoTag _ _ r) = mt <$ g r

-- mtTuple :: Getter (MemoTag a b) b
mtTuple :: (Contravariant f, Functor f) => ((a, b) -> f (a, b)) -> MemoTag a b -> f (MemoTag a b)
mtTuple g mt@(MemoTag _ v r) = mt <$ g (v, r)
