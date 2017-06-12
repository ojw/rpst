-- | This should really be RPST.Game.Class.

module RPST.Class where

-- tbh I don't know what even goes here
class Monad m => MonadGame m where
  step :: TimeDelta -> m ()
