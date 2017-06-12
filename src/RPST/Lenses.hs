{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

-- | Lenses for types.

module RPST.Lenses where

import           Control.Lens
import           RPST.Types

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
makeFields ''StatusState
makeFields ''AbilityApplication
