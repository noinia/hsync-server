{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
module HSync.Common.AcidState where




import Prelude
import Data.Data            (Typeable)
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid
    ( AcidState(..), EventState(..), EventResult(..)
    , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
    , IsAcidic(..), makeAcidic, openLocalState
    )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   (query', update')



--------------------------------------------------------------------------------
--- Using acid state from an arbitrary monad


class HasAcidState m st where
  getAcidState :: m (AcidState st)

queryAcid      :: forall event m.
                  ( Functor m
                  , MonadIO m
                  , QueryEvent event
                  , HasAcidState m (EventState event)
                  ) =>
                  event
                  -> m (EventResult event)
queryAcid event = do as <- getAcidState
                     query' (as :: AcidState (EventState event)) event


updateAcid      :: forall event m.
                   ( Functor m
                   , MonadIO m
                   , UpdateEvent event
                   , HasAcidState m (EventState event)
                   ) =>
                   event
                   -> m (EventResult event)
updateAcid event = do as <- getAcidState
                      update' (as :: AcidState (EventState event)) event


-- | bracket the opening and close of the `AcidState` handle.

-- automatically creates a checkpoint on close
withAcidState
  :: ( MonadBaseControl IO m
     , MonadIO m
     , IsAcidic st
     , Typeable st
     ) =>
     Maybe FilePath        -- ^ path to state directory
  -> st                    -- ^ initial state value
  -> (AcidState st -> m a) -- ^ function which uses the
                           --   `AcidState` handle
  -> m a
withAcidState mPath initialState =
  bracket (liftIO $ open initialState)
          (liftIO . createCheckpointAndClose)
  where
    open = maybe openLocalState openLocalStateFrom mPath
