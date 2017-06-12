module RPST.Experiment where

import RPST.Types

type family Key a

data Id a = Id (Key a)
data Actual a = Actual a

class Lup m a where
  lup :: Id a -> Actual a

type instance Key Character = Int
type instance Key Ability = Int

-- type instance Key
