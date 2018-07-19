{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Yak.Modes
(
    ModeType(..),
    Mode(..),
    ModeOp,
    get,
    set,
    unset,
    -- * Common Channel Modes
    ban,
    exception,
    clientLimit,
    inviteOnly,
    inviteOnlyException,
    key,
    moderated,
    protectedTopic,
    noExternal,
)
where

import Data.Text (Text)
import Data.Semigroup

data ModeType = TypeA | TypeB | TypeC | TypeD

data Mode :: ModeType -> * -> * where
    ModeTypeA :: Char -> Mode 'TypeA a
    ModeTypeB :: Char -> Mode 'TypeB a
    ModeTypeC :: Char -> Mode 'TypeC a
    ModeTypeD :: Char -> Mode 'TypeD a

data ModeOp where
    GetMode :: Mode 'TypeA a -> ModeOp
    SetMode :: Mode t a -> Maybe a -> ModeOp
    UnsetMode :: Mode t a -> Maybe a -> ModeOp
    More :: [ModeOp] -> ModeOp

instance Semigroup ModeOp where
    More a <> More b = More $ a <> b
    More a <> b = More $ a <> [b]
    a <> More b = More $ a : b
    a <> b = More [a,b]

instance Monoid ModeOp where
    mempty = More []
    mappend = (<>)

get :: Mode 'TypeA a -> ModeOp
get = GetMode

class SetMode m where
    type ModeSetter m
    set :: m -> ModeSetter m

instance SetMode (Mode 'TypeA a) where
    type ModeSetter (Mode 'TypeA a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance SetMode (Mode 'TypeB a) where
    type ModeSetter (Mode 'TypeB a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance SetMode (Mode 'TypeC a) where
    type ModeSetter (Mode 'TypeC a) = a -> ModeOp
    set m a = SetMode m (Just a)

instance SetMode (Mode 'TypeD a) where
    type ModeSetter (Mode 'TypeD a) = ModeOp
    set m = SetMode m Nothing

class UnsetMode m where
    type ModeUnsetter m
    unset :: m -> ModeUnsetter m

instance UnsetMode (Mode 'TypeA a) where
    type ModeUnsetter (Mode 'TypeA a) = a -> ModeOp
    unset m a = UnsetMode m (Just a)

instance UnsetMode (Mode 'TypeB a) where
    type ModeUnsetter (Mode 'TypeB a) = a -> ModeOp
    unset m a = UnsetMode m (Just a)

instance UnsetMode (Mode 'TypeC a) where
    type ModeUnsetter (Mode 'TypeC a) = ModeOp
    unset m = UnsetMode m Nothing

instance UnsetMode (Mode 'TypeD a) where
    type ModeUnsetter (Mode 'TypeD a) = ModeOp
    unset m = UnsetMode m Nothing

type HostMask = String

ban :: Mode 'TypeA HostMask
ban = ModeTypeA 'b'

exception :: Mode 'TypeA HostMask
exception = ModeTypeA 'e'

clientLimit :: Mode 'TypeC Int
clientLimit = ModeTypeC 'l'

inviteOnly :: Mode 'TypeD a
inviteOnly = ModeTypeD 'i'

inviteOnlyException :: Mode 'TypeA HostMask
inviteOnlyException = ModeTypeA 'I'

key :: Mode 'TypeB Text
key = ModeTypeB 'k'

moderated :: Mode 'TypeD a
moderated = ModeTypeD 'm'

protectedTopic :: Mode 'TypeD a
protectedTopic = ModeTypeD 't'

noExternal :: Mode 'TypeD a
noExternal = ModeTypeD 'n'

test :: ModeOp
test = set ban "foo!bar@qux" <> unset ban "qux!fuu@bar"
