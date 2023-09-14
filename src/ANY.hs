module ANY
  ( ANY (..),
    module ANY.Conversions,
  )
where

import ANY.Conversions
import Types (ANY (..))

instance Show ANY where
  show = toForm