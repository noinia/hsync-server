{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.StorageTree where

import Control.Applicative
import Prelude
import Control.Lens
import Data.Semigroup
import Data.Set(Set)
import qualified Data.Set as S
import Data.SafeCopy

--------------------------------------------------------------------------------

type Path n = [n]




class Semigroup m => Measured m a | a -> m where
  measure :: a -> m

newtype OrdByName n a = OrdByName { _unOrdByName :: a }
                      deriving (Show,Read)

makeLenses ''OrdByName
$(deriveSafeCopy 0 'base ''OrdByName)


instance Eq n => Eq (OrdByName n (StorageTree n m a)) where
  (OrdByName (Node n _ _ _)) == (OrdByName (Node n' _ _ _)) = n == n'

instance Ord n => Ord (OrdByName n (StorageTree n m a)) where
  (OrdByName (Node n _ _ _)) `compare` (OrdByName (Node n' _ _ _)) = n `compare` n'

--------------------------------------------------------------------------------


data StorageTree n m a = Node { _name        :: n
                              , _measurement :: m
                              , _nodeData    :: a
                              , _children    :: Set (OrdByName n (StorageTree n m a))
                              }
                         deriving (Show,Read)

makeLenses ''StorageTree

-- Bleh, because of the constraint on n we cannot derive a safecopy instance
-- $(deriveSafeCopy 0 'base ''StorageTree)

instance (Ord n, SafeCopy n, SafeCopy m, SafeCopy a) => SafeCopy (StorageTree n m a) where
  putCopy (Node n m d chs) = contain $ do safePut n; safePut m; safePut d; safePut chs
  getCopy = contain $ Node <$> safeGet <*> safeGet <*> safeGet <*> safeGet



-- deriving instance (Eq n, Eq m, Eq a) => Eq (StorageTree n m a)




instance Measured m a => Measured m (StorageTree n m a) where
  measure (Node _ m _ _) = m




--------------------------------------------------------------------------------


-- Updates the element by applying the given function to it. Any intermediate
-- nodes are created if they do not exist. Furthermore, all measurements on the
-- path to the root are updated.
updateAt                           :: (Measured m a, Ord n)
                                   => Path n
                                   -> (a -> a)
                                   -> a    -- ^ default element to use for missing parents
                                   -> StorageTree n m a -> StorageTree n m a
updateAt []     f _      (Node n m a chs) = let a' = f a
                                            in Node n (m <> measure a') a' chs
updateAt (p:ps) f emptyE (Node n m a chs) = case lookupByName p chs of
    Nothing -> newNode (updateAt ps f emptyE $ emptyC p)
    Just c  -> newNode (updateAt ps f emptyE c)
  where
    newNode c = Node n  (m <> measure c) a      (S.insert (OrdByName c) chs)

    emptyC n' = Node n' (measure emptyE) emptyE mempty


lookupByName      :: Ord n => n -> Set (OrdByName n (StorageTree n m a))
                  -> Maybe (StorageTree n m a)
lookupByName n xs = let dummy = OrdByName $ Node n undefined undefined mempty
                    in S.lookupGE dummy xs >>= \(OrdByName x) ->
                         if _name x == n then Just x else Nothing


class HasEmpty a where
  emptyElement :: a


--------------------------------------------------------------------------------

class HasVersions a v where
  headVersionLens :: Lens' (a v) v

  headVersion :: a v -> v
  headVersion = view headVersionLens

  addVersion  :: v -> a v -> a v

-- | We hae an update to a version in the tree. That means we create a new version
updateVersionAt     :: (Measured m (a v), HasVersions a v, Ord n)
                    => Path n
                    -> (v -> v)
                    -> a v       -- ^ default element to use for missing parents
                    -> StorageTree n m (a v) -> StorageTree n m (a v)
updateVersionAt p f = updateAt p (\a -> let v' = f . headVersion $ a
                                        in addVersion v' a)
