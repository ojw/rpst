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

reverseStats :: Stats' a -> Stats' a
reverseStats (Stats h e) = Stats (negate h) (negate e)

data CostStats
data DamageStats
data CharacterStats

type Stats = Stats' CharacterStats
type Cost = Stats' CostStats
type Damage = Stats' DamageStats

data TargetType = TTSelf | TTFriend | TTFoe | TTAny
  deriving (Show)

data Effect -- = Effect (CharacterState -> CharacterState)
  = Damage Damage
  | Healing Damage
  | Status StatusState -- shouldn't be StatusState, but something in
                       -- between since tracking source here is wrong

-- hmm...
-- Or does Effect also take into account target defenses, etc?
-- data AppliedEffect
--   = AppliedDamage Damage
--   | AppliedHealing Damage
--   | AppliedStatus Status

-- So we'll have an event type, they'll get logged / rendered ...now
-- how do we log these events?  ...a writer / logger somewhere in the
-- stack?  ... actually the game monad should provide a logEvent or
-- similar, imo ... but mmaybe the free style handles logging better?
-- idk, this is logging I intend to display, which feels more semantic
data Event
  = AbilityApplied AbilityApplication
  | EffectGenerated Effect -- the effect as the source intended
  | EffectApplied Effect -- the effect as it impacts the target,
                         -- applying resistances, etc
  | CharacterStateChanged -- like death / incapacitation?

instance Show Effect where
  show e = "Effect"

data Ability = Ability
  { _abilityName       :: Text
  , _abilityCost       :: Cost
  , _abilityTargetType :: TargetType
  , _abilityEffect     :: Effect
  , _abilityPriority   :: Int -- proper type eventually
  } deriving (Show)

data Target = Target -- uuuuugh have to match this with TargetType somehow
  deriving Show

data AbilityApplication = AbilityApplication
  { _abilityApplicationAbility :: Ability
  , _abilityApplicationSource  :: CharacterState
  , _abilityApplicationTarget  :: CharacterState -- ugh, wrong
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
  , _characterStateStatuses  :: [StatusState]
  } deriving (Show)

type TurnNumber = Int

type Duration = Int

data Status
  = ExtraDamage Damage
  | ReducedDamage Damage
  | TemporaryHealth Int
  | TemporaryEnergy Int -- ugh this is dumb
  | Blocked
  | Poisoned Damage
  | RedirectingDamageTo Int -- character id
  | Dodging
  | DamageReduction Damage
  deriving Show

type AbilityId = Int
type CharacterId = Int

data StatusState = StatusState
  { _statusStateStatus   :: Status
  , _statusStateDuration :: Duration
  } deriving Show

data Source = Source -- so many Ints, wtf
  { _sourceCharacter :: CharacterId
  , _sourceAbility   :: AbilityId
  , _sourceTurn      :: TurnNumber
  } deriving Show

data Player = FirstPlayer | SecondPlayer
  deriving (Ord, Eq, Show)

data Command = Command
  { _commandCharacter :: Int -- for now...
  , _commandAbility   :: Int -- yuck...
  , _commandTarget    :: Int -- okay this is just a placeholder
  } deriving Show

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
