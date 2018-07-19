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

typeB "key" 'k' ''Text

typeC "clientLimit" 'l' ''Int

typeD "protected" 'm'

typeD "secret" 's'

typeD "moderated" 'm'

test :: ModeStr
test = putBan HostMask <> delKey "foobar" <> putModerated

test' :: ModeStr
test' = mconcat [putBan HostMask, delKey "foobar", putModerated]
