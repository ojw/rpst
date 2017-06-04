{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module RPST.Types where

import           Control.Concurrent.STM
import           Control.Error
import           Control.Lens
import           Data.Default
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

data Stats' a = Stats
  { _stats'Health :: Int
  , _stats'Energy :: Int
  } deriving (Show)

coerceStats :: Stats' a -> Stats' b
coerceStats (Stats x y) = Stats x y

instance Monoid (Stats' a) where
  mempty = Stats 0 0
  (Stats a b) `mappend` (Stats x y) = Stats (a+x) (b+y)

data CostStats
data DamageStats
data CharacterStats

type Stats = Stats' CharacterStats
type Cost = Stats' CostStats
type Damage = Stats' DamageStats

-- instance Num Damage where
--   (Damage s) + (Damage s') = Damage (s + s')
--   (Damage s) * (Damage s') = Damage (s * s')
--   abs (Damage d) = Damage (abs d)
--   signum (Damage d) = Damage (signum d)
--   fromInteger i = Damage (fromInteger i)
--   negate (Damage d) = Damage (negate d)

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

type TimeDelta = Int

data GameConfig = GameConfig
  { _gameConfigTimePerTurn  :: Maybe Int
  } deriving Show

instance Default GameConfig where
  def = GameConfig Nothing

data Game = Game
  { _gameCharacters         :: (Map Int CharacterState) -- maybe?
  , _gameFirstPlayerOrders  :: Maybe Orders
  , _gameSecondPlayerOrders :: Maybe Orders
  , _gameConfig             :: GameConfig
  , _gameTimer              :: Maybe Int -- is the game timed?
  } deriving Show

-- Server-related types.

type User = Text

type GameId = Int

data Server = Server
  { _serverGames :: TVar (Map GameId (Map User Player, Game))
  , _serverInput :: TChan Message
  }

-- eh... need something to capture the fact that commands should be
-- sent for each of a player's characters Probably Map Id Order, where
-- Order is an improved Command?
--
-- Eh actually just [Command] is fine, w/ some checking that it's
-- exhaustive, or defaulting for missed characters.
data Orders = Orders
  { _ordersOrders :: [Command]
  } deriving Show

data MessagePayload = CharacterOrders Orders
  deriving Show

data Message = Message
  { _messageUser    :: User
  , _messagePayload :: MessagePayload
  , _messageGame    :: Int
  } deriving Show

data Outcome = WinnerIs Player | MutualDeath | Ongoing
  deriving Show

data Error
  = GameDoesNotExist
  | UserIsNotInGame
  | UserDoesNotControlCharacter
  | CharacterIsNotInGame
  | CharacterDoesNotHaveAbility

-- gotta have soooomething here... not sure what yet
type Foo = ExceptT Error STM

makeFields ''Stats'
makeFields ''Character
makeFields ''Ability
makeFields ''CharacterState
makeFields ''Command
makeFields ''Game
makeFields ''Server
makeFields ''Message
makeFields ''Orders
makeFields ''GameConfig
