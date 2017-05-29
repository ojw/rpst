{-# LANGUAGE OverloadedStrings #-}
-- | Like an emacs scratch buffer, for a haskell project

module Scratch where

import           Control.Lens
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           RPST

punch :: Ability
punch = Ability "Punch" (Cost (Stats 0 1)) TTFoe (damageEff 2)

puncher :: Character
puncher = Character
  { _characterName = "Puncher"
  , _characterStats = Stats 10 10
  , _characterAbilities = [punch]
  , _characterPointValue = 0
  }

stab :: Ability
stab = Ability "Stab" (Cost (Stats 0 1)) TTFoe (damageEff 4)

stabber :: Character
stabber = puncher
  & name .~ "Stabber"
  & abilities .~ [stab]

players :: Map User Player
players = Map.fromList [("James", FirstPlayer), ("Clayton", SecondPlayer)]

gom :: Game
gom = Game $ Map.fromList [(1, new puncher FirstPlayer), (2, new stabber SecondPlayer)]

command1 :: Command
command1 = Command 1 0 2

command2 :: Command
command2 = Command 2 0 1

message1 = Message "James" command1 0

test = do
  server <- makeServer
  startGame players gom server
  messageServer message1 server
  messageServer message1 server
  messageServer message1 server
  messageServer message1 server
  messageServer message1 server
  processMessage server
  processMessage server
  processMessage server
  game <- viewGame 0 server
  print $ fmap outcome game
  processMessage server
  processMessage server
  game <- viewGame 0 server
  print $ fmap outcome game
