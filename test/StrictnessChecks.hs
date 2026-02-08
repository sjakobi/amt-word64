module StrictnessChecks
  ( WhnfBox (..)
  , isWhnf
  ) where

import Data.Maybe (isNothing)
import NoThunks.Class (NoThunks (..), unsafeNoThunks)

-- | A wrapper for checking WHNF without forcing fields.
newtype WhnfBox a = WhnfBox a

instance NoThunks (WhnfBox a) where
  wNoThunks _ _ = pure Nothing
  showTypeOf _ = "WhnfBox"

{- | True if the wrapped value is in WHNF (outer constructor evaluated).

This uses 'unsafeNoThunks' with a shallow 'NoThunks' instance, so it only
checks for a top-level thunk and does not traverse fields.
-}
isWhnf :: WhnfBox a -> Bool
isWhnf = isNothing . unsafeNoThunks
