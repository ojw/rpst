-- | Rules

module RPST.Game where

import           Control.Lens
import           Control.Monad (join)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe
import           Debug.Trace
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

-- | Perform all commands, returning an updated game.

runTurn :: Game -> Game
runTurn game = game'' & firstPlayerOrders .~ Nothing
                      & secondPlayerOrders .~ Nothing
                      & timer .~ view (config . timePerTurn) game
  where
    commands1 = maybe [] id (preview (firstPlayerOrders . _Just . orders) game)
    commands2 = maybe [] id (preview (secondPlayerOrders . _Just . orders) game)
    game' = processCommands (commands1 ++ commands2) game
    game'' = maybe game id game'

-- | Returns Nothing if certain lookups fail - the source or target
-- characters or ability don't exist.  This shouldn't really happen,
-- but who knows?
--
-- This will probably wind up as an Either eventually.
--
-- There's gotta be a better way of doing this kinda thing.
-- Like parameterizing by a functor or something, idk.

commandContents :: Command -> Game -> Maybe (CharacterState, Ability, CharacterState)
commandContents com gom = do
  char <- Map.lookup (view character com) (view characters gom)
  abil <- char ^? character . abilities . ix (view ability com)
  targ <- Map.lookup (view target com) (view characters gom)
  return (char, abil, targ)

-- | The command's source, target, and ability all exist, and the player controls the source character.

validateCommand :: Player -> Game -> Command -> Bool
validateCommand player game command = case commandContents command game of
  Nothing                 -> False
  Just (char, abil, targ) -> player == view owner char

validateCommands :: Player -> [Command] -> Game -> Bool
validateCommands player commands game = all (validateCommand player game)commands

-- | Run a list of commands on a game.
-- If any commands return Nothing, returns Nothing.
-- Otherwise returns a new game state.

processCommands :: [Command] -> Game -> Maybe Game
processCommands commands game = foldr update (Just game) commands
  where
    update command maybeGame = processCommand command =<< maybeGame-- maybeGame >>= processCommand command

-- | Might be nothing if any of a bunch of lookups miss.

-- this needs way more intelligence around it
processCommand :: Command -> Game -> Maybe Game
processCommand command game = do
  (char,  abty, tgt) <- commandContents command game
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

new :: Player -> Character -> CharacterState
new player char = CharacterState char mempty player []

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
damageEff d = Damage d

healingEff :: Damage -> Effect
healingEff d = Healing d

applyAbility :: Ability -> CharacterState -> CharacterState
applyAbility ability char = case view effect ability of
  Damage d -> applyDamageEffect d char
  Status s -> applyStatusEffect s char

applyDamageEffect :: Damage -> CharacterState -> CharacterState
applyDamageEffect (Stats h e) char = char & damage . health +~ h
                                           & damage . energy +~ e

applyStatusEffect :: StatusState -> CharacterState -> CharacterState
applyStatusEffect status char = char & buffs %~ cons status
