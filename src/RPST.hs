{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

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

data Stats = Stats
  { _statsHealth :: Int
  , _statsEnergy :: Int
  } deriving (Show)

instance Monoid Stats where
  mempty = Stats 0 0
  (Stats a b) `mappend` (Stats x y) = (Stats (a+x) (b+y))

instance Num Stats where
  (Stats h e) + (Stats h' e') = Stats (h + h') (e + e')
  (Stats h e) * (Stats h' e') = Stats (h * h') (e + e') -- but don't
  abs (Stats h e) = Stats (abs h) (abs e)
  signum (Stats h e) = Stats (signum h) (signum e)
  fromInteger i = (Stats (fromInteger i) 0) --jeez
  negate (Stats h e) = Stats (negate h) (negate e)

newtype Cost = Cost Stats
  deriving (Show)

newtype Damage = Damage Stats
  deriving (Show)

instance Monoid Damage where
  mempty = Damage (Stats 0 0)
  Damage (Stats a b) `mappend` Damage (Stats x y) = Damage (Stats (a+x) (b+y))

instance Num Damage where
  (Damage s) + (Damage s') = Damage (s + s')
  (Damage s) * (Damage s') = Damage (s * s')
  abs (Damage d) = Damage (abs d)
  signum (Damage d) = Damage (signum d)
  fromInteger i = Damage (fromInteger i)
  negate (Damage d) = Damage (negate d)

data TargetType = TTSelf | TTFriend | TTFoe | TTAny
  deriving (Show)

-- data Effect = Effect            -- jinkies
--   deriving (Show)

data Effect = Effect (CharacterState -> CharacterState)

instance Show Effect where
  show e = "Effect"

data Ability = Ability
  { _abilityName       :: Text
  , _abilityCost       :: Cost
  , _abilityTargetType :: TargetType
  , _abilityEffect     :: Effect
  } deriving (Show)

data Character = Character
  { _characterName       :: Text
  , _characterStats      :: Stats
  , _characterAbilities  :: [Ability]
  , _characterPointValue :: Int       -- assuming point value system later
  } deriving (Show)

data CharacterState = CharacterState -- I expect to eventually track damage differently... via a collection of effects with sources, durations, etc
  { _characterStateCharacter :: Character
  , _characterStateDamage    :: Damage
  , _characterStateOwner     :: Player
  } deriving (Show)

data Player = FirstPlayer | SecondPlayer
  deriving (Ord, Eq, Show)

data Command = Command
  { _commandCharacter :: Int -- for now...
  , _commandAbility   :: Int -- yuck...
  , _commandTarget    :: Int -- okay this is just a placeholder
  } deriving Show -- jeez

data Game = Game
  { _gameCharacters :: (Map Int CharacterState) -- maybe?
  } deriving Show

type User = Text

data Server = Server
  { _serverGames :: TVar (Map Int (Map User Player, Game))
  , _serverInput :: TChan Message
  }

-- eh... need something to capture the fact that commands should be
-- sent for each of a player's characters Probably Map Id Order, where
-- Order is an improved Command?
--
-- Eh actually just [Command] is fine, w/ some checking that it's
-- exhaustive, or defaulting for missed characters.
data Orders = Orders
  { _orders :: [Command]
  } deriving Show

data Message = Message
  { _messageUser    :: User
  , _messageCommand :: Command
  , _messageGame    :: Int
  } deriving Show

data Outcome = WinnerIs Player | MutualDeath | Ongoing
  deriving Show

makeFields ''Stats
makeFields ''Character
makeFields ''Ability
makeFields ''CharacterState
makeFields ''Command
makeFields ''Game
makeFields ''Server
makeFields ''Message

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

-- what a mess
validateMessage :: Server -> Message -> STM Bool
validateMessage server message = do
  let usr = view user message
      cmd = view command message
      gam = view game message
  games <- readTVar (view games server)
  let maybeSuccess = do
        (playerMap, game) <- Map.lookup gam games
        player <- Map.lookup usr playerMap
        chr <- Map.lookup (view character cmd) (view characters game)
        return ((view owner chr) == player)
  return $ maybe False id maybeSuccess

-- uuuugh
processMessage :: Server -> IO ()
processMessage server = atomically $ do
  message <- readTChan (view input server)
  gomes :: Map Int (Map User Player, Game) <- readTVar (view games server)
  isValid <- validateMessage server message
  let gs' :: Maybe (Map Int (Map User Player, Game)) = do
        (players, gm) <- Map.lookup (view game message) gomes
        game' <- processCommand (view command message) gm
        let games' :: Map Int (Map User Player, Game) = Map.insert (view game message) (players, game') gomes
        return games'
  when isValid $
    maybe
      (return ())
      (\gs -> writeTVar (view games server) gs)
      gs'

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
    (Damage d) = view damage char -- ugh what was I thinking
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
applyDamage (Damage d) s = s - d

-- jeez which of these do I want?
payCost :: Cost -> CharacterState -> CharacterState
payCost (Cost c) = damage +~ (Damage c)

payFor :: Ability -> CharacterState -> CharacterState
payFor ability = payCost (view cost ability)

damageEff :: Damage -> Effect
damageEff d = Effect (damage +~ d)

healingEff :: Damage -> Effect
healingEff h = Effect (damage -~ h)

applyAbility :: Ability -> CharacterState -> CharacterState
applyAbility ability = f
  where
    Effect f = view effect ability
