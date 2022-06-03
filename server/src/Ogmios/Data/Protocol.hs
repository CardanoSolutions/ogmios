--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Protocol
    ( MethodName
    ) where

import Ogmios.Prelude

import qualified Codec.Json.Wsp as Wsp

type instance Wsp.ServiceName (Wsp.Request _) = "ogmios"

type MethodName = String
