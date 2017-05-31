{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module RPST.Types where

import           Control.Concurrent.STM
import           Control.Lens
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

data Stats' a = Stats
  { _stats'Health :: Int
  , _stats'Energy :: Int
  } deriving (Show)

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

data Game = Game
  { _gameCharacters         :: (Map Int CharacterState) -- maybe?
  , _gameFirstPlayerOrders  :: Orders
  , _gameSecondPlayerOrders :: Orders
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

makeFields ''Stats'
makeFields ''Character
makeFields ''Ability
makeFields ''CharacterState
makeFields ''Command
makeFields ''Game
makeFields ''Server
makeFields ''Message
makeFields ''Orders
