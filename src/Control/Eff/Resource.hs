{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
-- | Allocate resources which are guaranteed to be released.
--
--   For more information, see the @resourcet@ package.
module Control.Eff.Resource ( Resource
                            , ResourceState
                            , ReleaseKey
                            , runResource
                            , allocate
                            , register
                            , release
                            , unprotect
                            ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as M
import Data.Typeable

-- | A resource's state. Type parameter @m@ is the Monad the resource
--   deallocation will run in.
data ResourceState m =
        -- | ResourceState takes the 'next' int to insert and a map of
        -- cleanup handlers
        ResourceState
          {-# UNPACK #-} !Int
          !(IntMap m)
  deriving Typeable

-- | The Resource effect. This effect keeps track of all registered actions,
--   and calls them upon exit (via 'runResource'). Actions may be registered
--   via register, or resources may be allocated atomically via allocate.
--   allocate corresponds closely to bracket.
--
--   Releasing may be performed before exit via the release function. This
--   is a highly recommended optimization, as it will ensure that scarce
--   resources are freed early. Note that calling release will deregister
--   the action, so that a release action will only ever be called once.
type Resource m = State (ResourceState m)

-- | A lookup key for a specific release action. This value
--   is returned by @register@ and @allocate@, and is passed to @release@.
newtype ReleaseKey = K Int
    deriving Typeable

withState :: (Typeable s, Member (State s) r)
          => (s -> Eff r (s, a))
          -> Eff r a
withState f = do
  oldState <- get
  (newState, ret) <- f oldState
  put newState
  return ret
{-# INLINE withState #-}

-- | Call a release action early, and deregister it from the list of
--   cleanup actions to be performed.
release :: (Typeable1 m, SetMember Lift (Lift m) r, Member (Resource (m ())) r)
        => ReleaseKey
        -> Eff r ()
release (K k) = withState $ \old@(ResourceState cnt m) ->
  case M.lookup k m of
    Nothing      -> return (old, ())
    Just cleanup -> do
      () <- lift cleanup
      return (ResourceState cnt (M.delete k m), ())
{-# INLINE release #-}

-- | Register some action that will be called precisely once, either when
--   'runResource' is called or when the 'ReleaseKey' is passed to 'release'.
register :: (Typeable1 m, Member (Resource (m ())) r)
         => m ()
         -> Eff r ReleaseKey
register cleanup =
  withState $ \(ResourceState cnt oldMap) ->
    return (ResourceState (cnt+1) (M.insert cnt cleanup oldMap), K cnt)
{-# INLINE register #-}

-- | Perform some allocation, and automatically register a cleanup action.
allocate :: (Typeable1 m, Monad m, Member (Resource (m ())) r, SetMember Lift (Lift m) r)
         => m a         -- ^ allocate
         -> (a -> m ()) -- ^ free resource
         -> Eff r (ReleaseKey, a)
allocate alloc dealloc = do
  res <- lift alloc -- TODO: Protect against asynchronous exceptions. Patches welcome!
  k   <- register (dealloc res)
  return (k, res)
{-# INLINE allocate #-}

-- | Unprotect resource from cleanup actions, this allowes you to send
--   resource into another resourcet process and reregister it there.
--
--   It returns an release action that should be run in order to clean
--   resource or Nothing in case if resource is already freed.
unprotect :: (Typeable1 m, Member (Resource (m ())) r)
          => ReleaseKey
          -> Eff r (Maybe (m ()))
unprotect (K k) =
  withState $ \old@(ResourceState cnt oldMap) ->
    case M.lookup k oldMap of
      Nothing    -> return (old, Nothing)
      v@(Just _) -> return (ResourceState cnt (M.delete k oldMap), v)
{-# INLINE unprotect #-}

-- | Unwrap a 'Resource' effect, and call all registered release actions.
runResource :: (Typeable1 m, Monad m, SetMember Lift (Lift m) r)
            => Eff (Resource (m ()) :> r) a
            -> Eff r a
runResource eff = do
  (ResourceState _ toClean, res) <- runState (ResourceState 0 M.empty) eff
  lift $ mapM_ snd (M.toDescList toClean)
  return res
{-# INLINE runResource #-}
