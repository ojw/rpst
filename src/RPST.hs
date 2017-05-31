{-# LANGUAGE ScopedTypeVariables #-}

module RPST where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad          (when)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Maybe             as Maybe
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Debug.Trace

import           RPST.Types

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

commandContents :: Command -> Game -> Maybe (CharacterState, Ability, CharacterState)
commandContents com gom = do
  char <- Map.lookup (view character com) (view characters gom)
  abil <- char ^? character . abilities . ix (view ability com)
  targ <- Map.lookup (view target com) (view characters gom)
  return (char, abil, targ)

-- validateOrders :: Player -> Orders -> Game -> Bool
-- validateOrders player ordrs game = all f (view orders ordrs)
--   where f =


-- what a mess
validateMessage :: Server -> Message -> STM Bool
validateMessage server message = do
  return True
  -- let usr = view user message
  --     cmd = view command message
  --     gam = view game message
  -- games <- readTVar (view games server)
  -- let maybeSuccess = do
  --       (playerMap, game) <- Map.lookup gam games
  --       player <- Map.lookup usr playerMap
  --       chr <- Map.lookup (view character cmd) (view characters game)
  --       return ((view owner chr) == player)
  -- return $ maybe False id maybeSuccess

-- uuuugh
processMessage :: Server -> IO ()
processMessage server = atomically $ do
  message <- readTChan (view input server)
  gomes :: Map Int (Map User Player, Game) <- readTVar (view games server)
  isValid <- validateMessage server message
  when isValid $
    let gs' :: Maybe (Map Int (Map User Player, Game)) = do
          (players, gm) <- Map.lookup (view game message) gomes
          game' <- processMessagePayload (view user message) (view payload message) gm
          let games' :: Map Int (Map User Player, Game) = Map.insert (view game message) (players, game') gomes
          return games'
    in
    maybe
      (return ())
      (\gs -> writeTVar (view games server) gs)
      gs'

processMessagePayload :: User -> MessagePayload -> Game -> Maybe Game
processMessagePayload user payload game =
  case payload of
    CharacterOrders orders -> Just game

-- this needs way more intelligence around it
processCommand :: Command -> Game -> Maybe Game
processCommand command game = do
  char <- game ^? characters . ix (view character command) . character
  tgt  <- game ^? characters . ix (view target command)
  abty <- char ^? abilities . ix (view ability command)
  return $ game & characters . ix (view target command) %~ applyAbility abty
                & characters . ix (view character command) %~ payCost (view cost abty)

viewCharacter :: CharacterState -> Character -- ??? Eventually won't be Character... this is just for debugging or something
viewCharacter cs = view character cs & stats %~ applyDamage (view damage cs)

isAlive :: CharacterState -> Bool
isAlive char = maxHealth > healthDamage
  where
    maxHealth = view (character . stats . health) char
    d = view damage char -- ugh what was I thinking
    healthDamage = view health d

outcome :: Game -> Outcome
outcome game
  | firstDefeated && secondDefeated = MutualDeath
  | firstDefeated= WinnerIs SecondPlayer
  | secondDefeated = WinnerIs FirstPlayer
  | otherwise = Ongoing
  where
    firstDefeated = isDefeated FirstPlayer game
    secondDefeated = isDefeated SecondPlayer game

playerChars :: Player -> Game -> [CharacterState]
playerChars player game = filter (\c -> player == (view owner c)) allChars
  where
    allChars = Map.elems $ view characters game

isDefeated :: Player -> Game -> Bool
isDefeated player game = not $ any isAlive (playerChars player game)

new :: Character -> Player -> CharacterState
new char player = CharacterState char mempty player

applyDamage :: Damage -> Stats -> Stats
applyDamage d s = s & health -~ (view health d)
                    & energy -~ (view energy d)

-- jeez which of these do I want?
payCost :: Cost -> CharacterState -> CharacterState
payCost c cs = cs & damage .~ d'
  where
    d = view damage cs
    d' = d & health +~ view health c
           & energy +~ view energy c

payFor :: Ability -> CharacterState -> CharacterState
payFor ability = payCost (view cost ability)

damageEff :: Damage -> Effect
damageEff d = Effect $ \h -> h & damage . health +~ view health d
                               & damage . energy +~ view energy d

healingEff :: Damage -> Effect
healingEff d = Effect $ \h -> h & damage . health -~ view health d
                                & damage . energy -~ view energy d

applyAbility :: Ability -> CharacterState -> CharacterState
applyAbility ability = f
  where
    Effect f = view effect ability
