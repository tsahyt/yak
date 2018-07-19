{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Yak.Modes
    ( emitModeOp
    , fetchModeOp
    , ModeType(..)
    , Mode(..)
    , ModeParameter(..)
    , ModeOp
    , get
    , set
    , unset
    -- * Common Channel Modes
    , ban
    , exception
    , clientLimit
    , inviteOnly
    , inviteOnlyException
    , key
    , moderated
    , protectedTopic
    , noExternal
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (ByteString)
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

data ModeType
    = TypeA
    | TypeB
    | TypeC
    | TypeD

data Mode :: ModeType -> * -> * where
    ModeTypeA :: Char -> Mode 'TypeA a
    ModeTypeB :: Char -> Mode 'TypeB a
    ModeTypeC :: Char -> Mode 'TypeC a
    ModeTypeD :: Char -> Mode 'TypeD Void

class ModeParameter a where
    renderMode :: a -> ByteString
    seizeMode :: Parser a

instance ModeParameter Void where
    renderMode = absurd
    seizeMode = empty

instance ModeParameter Int where
    renderMode = B.pack . show
    seizeMode = A.decimal

instance ModeParameter Word where
    renderMode = B.pack . show
    seizeMode = A.decimal

instance ModeParameter Text where
    renderMode = encodeUtf8
    seizeMode =
        decodeUtf8 <$> do
            x <- A.takeTill (\x -> A.isSpace x || x == ',')
            if B.null x
                then empty
                else pure x

data ModeOp where
    GetMode :: Mode 'TypeA a -> ModeOp
    SetMode :: ModeParameter a => Mode t a -> Maybe a -> ModeOp
    UnsetMode :: ModeParameter a => Mode t a -> Maybe a -> ModeOp
    More :: [ModeOp] -> ModeOp

emitModeOp :: ModeOp -> ByteString
emitModeOp = undefined

fetchModeOp :: ByteString -> Maybe ModeOp
fetchModeOp = undefined

instance Semigroup ModeOp where
    More a <> More b = More $ a <> b
    More a <> b = More $ a <> [b]
    a <> More b = More $ a : b
    a <> b = More [a, b]

instance Monoid ModeOp where
    mempty = More []
    mappend = (<>)

get :: Mode 'TypeA a -> ModeOp
get = GetMode

class SetMode m where
    type ModeSetter m
    set :: m -> ModeSetter m

instance ModeParameter a => SetMode (Mode 'TypeA a) where
    type ModeSetter (Mode 'TypeA a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance ModeParameter a => SetMode (Mode 'TypeB a) where
    type ModeSetter (Mode 'TypeB a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance ModeParameter a => SetMode (Mode 'TypeC a) where
    type ModeSetter (Mode 'TypeC a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance SetMode (Mode 'TypeD Void) where
    type ModeSetter (Mode 'TypeD Void) = ModeOp
    set m = SetMode m Nothing

class UnsetMode m where
    type ModeUnsetter m
    unset :: m -> ModeUnsetter m

instance ModeParameter a => UnsetMode (Mode 'TypeA a) where
    type ModeUnsetter (Mode 'TypeA a) = a -> ModeOp
    unset m a = UnsetMode m (Just a)

instance ModeParameter a => UnsetMode (Mode 'TypeB a) where
    type ModeUnsetter (Mode 'TypeB a) = a -> ModeOp
    unset m a = UnsetMode m (Just a)

instance ModeParameter a => UnsetMode (Mode 'TypeC a) where
    type ModeUnsetter (Mode 'TypeC a) = ModeOp
    unset m = UnsetMode m Nothing

instance UnsetMode (Mode 'TypeD Void) where
    type ModeUnsetter (Mode 'TypeD Void) = ModeOp
    unset m = UnsetMode m Nothing

type HostMask = Text

ban :: Mode 'TypeA HostMask
ban = ModeTypeA 'b'

exception :: Mode 'TypeA HostMask
exception = ModeTypeA 'e'

clientLimit :: Mode 'TypeC Int
clientLimit = ModeTypeC 'l'

inviteOnly :: Mode 'TypeD Void
inviteOnly = ModeTypeD 'i'

inviteOnlyException :: Mode 'TypeA HostMask
inviteOnlyException = ModeTypeA 'I'

key :: Mode 'TypeB Text
key = ModeTypeB 'k'

moderated :: Mode 'TypeD Void
moderated = ModeTypeD 'm'

protectedTopic :: Mode 'TypeD Void
protectedTopic = ModeTypeD 't'

noExternal :: Mode 'TypeD Void
noExternal = ModeTypeD 'n'

{-
 -test :: ModeOp
 -test = set ban "foo!bar@qux" <> unset ban "qux!fuu@bar" <> get ban
 -}
