{-# LANGUAGE ScopedTypeVariables #-}

-- | Rules

module RPST.Game where

import           Control.Error
import           Control.Lens
import           Control.Monad (foldM, join)
import           Data.Group    (invert)
import qualified Data.List     as List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe
import           Data.Monoid   ((<>))
import           Debug.Trace
import           RPST.Lenses
import           RPST.Types

-- | Create a new game.

newGame :: GameConfig -> [Character] -> [Character] -> Game
newGame config p1Chars p2Chars = Game chars Nothing Nothing config time
  where
    time = view timePerTurn config
    chars  = Map.fromList $ zipWith (,) [1..] (chars1 ++ chars2)
    chars1 = map (new FirstPlayer) p1Chars
    chars2 = map (new SecondPlayer) p2Chars

-- | Advance a game.
-- If both players' orders are in, run the commands.
-- Otherwise, just tick the timer.

stepGame :: TimeDelta -> Game -> Game
stepGame elapsedTime game = if ordersIn || timeUp then runTurn game else tickTimer elapsedTime game
  where
    ordersIn = Maybe.isJust (view firstPlayerOrders game) &&
               Maybe.isJust (view secondPlayerOrders game)
    timeUp = maybe False (<= 0) (view timer game)

-- | Advance a game's timer.

tickTimer :: TimeDelta -> Game -> Game
tickTimer elapsedTime = timer . _Just -~ elapsedTime

resetOrders :: Game -> Game
resetOrders game = game & firstPlayerOrders .~ Nothing
                        & secondPlayerOrders .~ Nothing

resetTimer :: Game -> Game
resetTimer game = game & timer .~ view (config . timePerTurn) game

tickCharacters :: Game -> Game
tickCharacters = characters . traversed %~ tickCharacter

runTurn' :: Game -> Either GameError Game
runTurn' game = do
  let commands1 = maybe [] id (preview (firstPlayerOrders . _Just . orders) game)
      commands2 = maybe [] id (preview (secondPlayerOrders . _Just . orders) game)
  prioritizedCommands <- commandsWithPriority game (commands1 ++ commands2)
  let foo :: [[Command]] = map snd prioritizedCommands
  gom' <- foldM processCommands game foo
  (return . resetOrders . resetTimer . tickCharacters) gom'

runTurn :: Game -> Game
runTurn game = either (\_ -> game) id (runTurn' game)

-- | Returns Nothing if certain lookups fail - the source or target
-- characters or ability don't exist.  This shouldn't really happen,
-- but who knows?
--
-- This will probably wind up as an Either eventually.
--
-- There's gotta be a better way of doing this kinda thing.
-- Like parameterizing by a functor or something, idk.

commandApplication :: Game -> Command -> Either GameError AbilityApplication
commandApplication gom com@(Command source abty target) = do
  char <- note (CharacterDoesNotExist source)
               (Map.lookup source (view characters gom))
  abil <- note (CharacterDoesNotHaveAbility source abty)
               (char ^? character . abilities . ix (view ability com))
  targ <- note (CharacterDoesNotExist target)
               (Map.lookup target (view characters gom))
  return (AbilityApplication com abil char targ)

commandApplications :: Traversable t => t Command -> Game -> Either GameError (t AbilityApplication)
commandApplications commands gom = traverse (commandApplication gom) commands

-- | The command's source, target, and ability all exist, and the player controls the source character.

validateCommand :: Player -> Game -> Command -> Bool
validateCommand player game command = case commandApplication game command of
  Left e                                        -> False
  Right (AbilityApplication cmd abil char targ) -> player == view owner char

validateCommands :: Player -> [Command] -> Game -> Bool
validateCommands player commands game = all (validateCommand player game)commands

-- | Run a list of commands on a game.
-- If any commands return Left, returns Left.
-- Otherwise returns Right with a new game state.

processApplications :: [AbilityApplication] -> Game -> Either GameError Game
processApplications applications game = foldr update (pure game) applications
  where
    update application eitherGame = processApplication application =<< eitherGame



-- this needs way more intelligence around it
processApplication :: AbilityApplication -> Game -> Either GameError Game
processApplication (AbilityApplication cmd abty char tgt) game = do
  if isAlive char && isAlive tgt
    then return $ game & characters . ix (view target cmd) %~ applyAbility . AbilityApplication cmd abty char
                       & characters . ix (view character cmd) %~ payCost (view cost abty)
    else return game

-- first, generate the AbilityApplications; then, apply them
processCommands :: Game -> [Command] -> Either GameError Game
processCommands gom commands = do
  applications <- commandApplications commands gom
  processApplications applications gom

viewCharacter :: CharacterState -> Character -- ??? Eventually won't be Character... this is just for debugging or something
viewCharacter cs = view character cs & stats %~ applyDamage (view damage cs)

isAlive :: CharacterState -> Bool
isAlive char = maxHealth > currentDamage
  where
    maxHealth = view (character . stats . health) char
    currentDamage = view (damage . health) char -- ugh what was I thinking


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

new :: Player -> Character -> CharacterState
new player char = CharacterState char mempty player []

applyDamage :: Damage -> Stats -> Stats
applyDamage d s = s & health -~ (view health d)
                    & energy -~ (view energy d)

currentStats :: CharacterState -> Stats
currentStats state = applyDamage (view damage state) (view (character . stats) state)

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
damageEff d = Damage d

healingEff :: Damage -> Effect
healingEff d = Healing d

applicationEffect :: AbilityApplication -> Effect
applicationEffect application = case view (ability . effect) application of
  Damage d -> Damage (buffedDamage (view source application) d)
  Status s -> Status s

applyAbility :: AbilityApplication -> CharacterState
applyAbility (AbilityApplication _ ability source target) =
  case view effect ability of
    Damage d -> applyDamageEffect (buffedDamage source d) target
    Status s -> applyStatusEffect s target

applyDamageEffect :: Damage -> CharacterState -> CharacterState
applyDamageEffect (Stats h e) char = char & damage . health +~ h
                                           & damage . energy +~ e

applyStatusEffect :: StatusEffect -> CharacterState -> CharacterState
applyStatusEffect status char = char & statuses %~ cons status

buffedDamage :: CharacterState -> Damage -> Damage
buffedDamage state d = foldr buffDamage d (state ^.. statuses . traversed . status)

buffDamage :: Status -> Damage -> Damage
buffDamage (ExtraDamage e) d   = e <> d
buffDamage (ReducedDamage r) d = invert r <> d
buffDamage _ d                 = d

tickCharacter :: CharacterState -> CharacterState
tickCharacter = statuses . traversed . duration -~ 1

groupBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Map b [a]
groupBy f = foldr (\x -> Map.insertWith (++) (f x) [x]) Map.empty

-- convert chosen abilities to a priority map
-- execute in order

--- within a given priority class, all abilities generate applications at the same time, are applied at the same time (from the Users' POV)
-- later abilities' applications will be influenced by earlier abilities' effects

-- jeez
-- | The returned command are ordered by priority, from smallest to largest.
commandsWithPriority :: Traversable t => Game -> t Command -> Either GameError [(Priority, [Command])]
commandsWithPriority gom commands = do
  commandsWithPriority :: t (Int, Command) <- traverse (commandPriority gom) commands
  let foo :: Map Int [(Priority, Command)] = groupBy fst commandsWithPriority
  let bar :: Map Int [Command] = (fmap . fmap) snd foo
  let baz :: [(Int, [Command])] = Map.toList bar
  return (List.sortBy (\x1 x2 -> compare (fst x1) (fst x2)) baz)
  where commandPriority :: Game -> Command -> Either GameError (Priority, Command)
        commandPriority gom command = fmap (\c -> ((view (ability . priority) c), command)) (commandApplication gom command)
