-- | Server

{-# LANGUAGE ScopedTypeVariables #-}

module RPST.Server where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad          (when)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Maybe             as Maybe
import           Data.Monoid            ((<>))
import           Data.Text              (Text)

import           RPST.Game
import           RPST.Lenses
import           RPST.Types

-- | Creating games and servers

viewGame :: Int -> Server -> IO (Maybe Game)
viewGame gid server = do
  gomes <- readTVarIO $ view games server
  return $ fmap snd (Map.lookup gid gomes)

makeServer :: IO Server
makeServer = do
  games <- newTVarIO mempty
  input <- newTChanIO
  return (Server games input)

messageServer :: Message -> Server -> IO ()
messageServer message server = atomically $ writeTChan (view input server) message

startGame :: Map User Player -> Game -> Server -> IO ()
startGame players game server = atomically $ do
  gameMap <- readTVar (view games server)
  let nextGameId = if (null gameMap)
                   then 0
                        -- uuuuugh partial functions
                   else (succ (maximum (Map.keys gameMap)))
      gameMap' = Map.insert nextGameId (players, game) gameMap
  writeTVar (view games server) gameMap'

-- | Validating messages to the server

validateMessage :: Server -> Message -> STM Bool
validateMessage server (Message user_ payload_ game_) = do
  games <- readTVar (view games server)
  let mgp = do
        (playerMap, game) <- Map.lookup game_ games
        player <- Map.lookup user_ playerMap
        return (game, player)
  case mgp of
    Nothing -> return False
    Just (game, player) -> case payload_ of
      CharacterOrders orders -> return (validateOrders player game orders)

validateOrders :: Player -> Game -> Orders -> Bool
validateOrders player game (Orders commands) = validateCommands player commands game

-- | Processing messages

readMessage :: Server -> STM Message
readMessage server = readTChan (view input server)

-- uuuugh
processMessage :: Server -> IO ()
processMessage server = atomically $ do
  message@(Message user_ payload_ game_) <- readMessage server
  gomes :: Map Int (Map User Player, Game) <- readTVar (view games server)
  isValid <- validateMessage server message
  when isValid $
    let mgs :: Maybe (Map Int (Map User Player, Game)) = do
          (players, gm) <- Map.lookup game_ gomes
          player <- Map.lookup user_ players
          game' <- fmap (stepGame 0) $
                     processMessagePayload player payload_ gm
          let games' = Map.insert game_ (players, game') gomes
          return games'
    in
    case mgs of
      Nothing           -> return ()
      Just updatedGames -> writeTVar (view games server) updatedGames

processMessagePayload :: Player -> MessagePayload -> Game -> Maybe Game
processMessagePayload player payload game =
  case payload of
    CharacterOrders orders -> case player of
      FirstPlayer  -> Just $ game & firstPlayerOrders .~ Just orders
      SecondPlayer -> Just $ game & secondPlayerOrders .~ Just orders
