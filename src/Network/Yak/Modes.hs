{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Yak.Modes where

import Data.Semigroup
import Data.Text
import Network.Yak.Modes.Types

data HostMask =
    HostMask

instance ModeParameter HostMask

typeA "ban" 'b' ''HostMask

typeA "exception" 'e' ''HostMask

typeD "secret" 's'

typeD "moderated" 'm'

typeB "key" 'k' ''Text

typeC "clientLimit" 'l' ''Int

typeD "protected" 'm'

test :: ModeStr
test = putBan HostMask <> delKey "foobar" <> putModerated

test' :: ModeStr
test' = mconcat [putBan HostMask, delKey "foobar", putModerated]
