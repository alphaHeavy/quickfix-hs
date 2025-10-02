{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Bifunctor.Swap (
    Swap (..),
    ) where

import Data.Bifunctor (Bifunctor (..))

import qualified Data.Tuple

-- | Symmetric 'Bifunctor's.
--
-- @
-- 'swap' . 'swap' = 'id'
-- @
--
-- If @p@ is a 'Bifunctor' the following property is assumed to hold:
--
-- @
-- 'swap' . 'bimap' f g = 'bimap' g f . 'swap'
-- @
--
-- 'Swap' isn't a subclass of 'Bifunctor', as for example
--
-- >>> newtype Bipredicate a b = Bipredicate (a -> b -> Bool)
--
-- is not a 'Bifunctor' but has 'Swap' instance
--
-- >>> instance Swap Bipredicate where swap (Bipredicate p) = Bipredicate (flip p)
--
class Swap p where
    swap :: p a b -> p b a

instance Swap (,) where
    swap = Data.Tuple.swap

instance Swap Either where
    swap (Left x) = Right x
    swap (Right x) = Left x

instance Swap ((,,) x) where
    swap (x,a,b) = (x,b,a)

instance Swap ((,,,) x y) where
    swap (x,y,a,b) = (x,y,b,a)

instance Swap ((,,,,) x y z) where
    swap (x,y,z,a,b) = (x,y,z,b,a)

instance Swap ((,,,,,) x y z w) where
    swap (x,y,z,w,a,b) = (x,y,z,w,b,a)

instance Swap ((,,,,,,) x y z w v) where
    swap (x,y,z,w,v,a,b) = (x,y,z,w,v,b,a)
