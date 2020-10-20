-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- Necessary if we don't want to have to pull the entire template-haskell
-- library as a dependency.
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.String.Interpolate.Extra
    ( stringQQ
    ) where


import Data.String.Interpolate
    ( __i )

-- | Just a re-export under a different name. Seems like the parser for
-- stylish-haskell does not like quasi-quoter starting with underscores. So,
-- defining here in a separate module because of template haskell stage
-- restrictions.
stringQQ :: _
stringQQ = __i
