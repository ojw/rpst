{-# LANGUAGE FlexibleContexts #-}

-- | This should really be RPST.Game.Class.

module RPST.Class where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Data.Maybe          (isJust)
import           RPST.Lenses
import           RPST.Types
import           Control.Monad.Except

-- setOrders, stepGame are the interface used by the server

-- tbh I don't know what even goes here
-- I think this is the low-level game stuff only
-- I think I wanna implement processing a turn using these primitives
class (Monad m, MonadError GameError m) => MonadGame m where
  lookupCharacter :: CharacterId -> m Character
  lookupAbility :: CharacterId -> AbilityId -> m Ability

  setOrders :: Player -> Orders -> m ()
  resetOrders :: m () -- I think this is only done for both at once
  stepTime :: TimeDelta -> m ()
  resetTimer :: m ()
  tickCharacter :: CharacterId -> m CharacterState -- probably gonna want this
  playerOrders :: m (Maybe Orders, Maybe Orders)
  remainingTime :: m (Maybe Time)
  logEvent :: Event -> m ()
  getCharacters :: m [CharacterId] -- maybe

  -- payCost :: Cost -> CharacterId -> m ()
  -- applyAbility :: AbilityApplication -> m ()
  -- commandAbility :: Command -> m AbilityApplication

  -- could be computed from playerOrders
  orderedCommands :: m [(Priority, [Command])]
  commandApplication :: Command -> m AbilityApplication
  processApplication :: AbilityApplication -> m ()

processCommandGroup :: MonadGame m => [Command] -> m ()
processCommandGroup commands = do
  applications <- mapM commandApplication commands
  mapM_ processApplication applications

processCommands :: MonadGame m => m ()
processCommands = do
  commands <- orderedCommands
  mapM_ processCommandGroup (map snd commands)

getCommands :: MonadGame m => m ([Command], [Command])
getCommands = do
  (morder1, morder2) <- playerOrders
  return (toCommands morder1, toCommands morder2)
  where
    toCommands = maybe [] (view orders)

ordersIn :: MonadGame m => m Bool
ordersIn = do
  (m1, m2) <- playerOrders
  return (isJust m1 && isJust m2)

timeUp :: MonadGame m => m Bool
timeUp = do
  mtime <- remainingTime
  case mtime of
    Nothing   -> return False
    Just time -> return (time >= 0)

tickCharacters :: MonadGame m => m ()
tickCharacters = do
  characters <- getCharacters
  mapM_ tickCharacter characters

stepGame :: MonadGame m => TimeDelta -> m ()
stepGame elapsedTime = do
  run <- liftA2 (||) ordersIn timeUp
  if run then runTurn else stepTime elapsedTime

runTurn :: MonadGame m => m ()
runTurn = do
  processCommands
  resetOrders
  resetTimer
  tickCharacters
  return ()
