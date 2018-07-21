{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Yak.Modes
    ( ModeStr
    , emitModeStr
    , fetchModeStr
    , ModeType(..)
    , Mode(..)
    , OpaqueMode(..)
    , ModeParameter(..)
    , ModeOp(..)
    , modeOps
    -- * Building Mode Strings
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
    -- * Server Modes
    , ServerModes(..)
    , defaultModes
    ) where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (ByteString)
import Data.Semigroup
import Data.Foldable
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

modeChar :: Mode t a -> Char
modeChar (ModeTypeA c) = c
modeChar (ModeTypeB c) = c
modeChar (ModeTypeC c) = c
modeChar (ModeTypeD c) = c

data OpaqueMode (t :: ModeType) =
    forall a. ModeParameter a => OpaqueMode (Mode t a)

matchOpaque :: Char -> OpaqueMode t -> Bool
matchOpaque c (OpaqueMode m) = modeChar m == c

data ServerModes = ServerModes
    { typeAModes :: [OpaqueMode 'TypeA]
    , typeBModes :: [OpaqueMode 'TypeB]
    , typeCModes :: [OpaqueMode 'TypeC]
    , typeDModes :: [OpaqueMode 'TypeD]
    }

findAMode :: ServerModes -> Char -> Maybe (OpaqueMode 'TypeA)
findAMode s c = find (matchOpaque c) (typeAModes s)

findBMode :: ServerModes -> Char -> Maybe (OpaqueMode 'TypeB)
findBMode s c = find (matchOpaque c) (typeBModes s)

findCMode :: ServerModes -> Char -> Maybe (OpaqueMode 'TypeC)
findCMode s c = find (matchOpaque c) (typeCModes s)

findDMode :: ServerModes -> Char -> Maybe (OpaqueMode 'TypeD)
findDMode s c = find (matchOpaque c) (typeDModes s)

instance Semigroup ServerModes where
    ServerModes a1 b1 c1 d1 <> ServerModes a2 b2 c2 d2 =
        ServerModes (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid ServerModes where
    mempty = ServerModes [] [] [] []
    mappend = (<>)

data FetchSt
    = Setting Char
    | Unsetting Char
    | Getting Char

fetchStChar :: FetchSt -> Char
fetchStChar (Setting c) = c
fetchStChar (Unsetting c) = c
fetchStChar (Getting c) = c

matchMode :: ServerModes -> FetchSt -> Parser ModeStr
matchMode smodes f
    | Just (OpaqueMode m) <- findAMode smodes (fetchStChar f) =
        case f of
            Setting _ -> set m <$> seizeMode
            Unsetting _ -> unset m <$> seizeMode
            Getting _ -> pure $ get m
    | Just (OpaqueMode m) <- findBMode smodes (fetchStChar f) =
        case f of
            Setting _ -> set m <$> seizeMode
            Unsetting _ -> unset m <$> seizeMode
            Getting _ -> fail "invalid modestring"
    | Just (OpaqueMode m) <- findCMode smodes (fetchStChar f) =
        case f of
            Setting _ -> set m <$> seizeMode
            Unsetting _ -> pure $ unset m
            Getting _ -> fail "invalid modestring"
    | Just (OpaqueMode m@(ModeTypeD _)) <- findDMode smodes (fetchStChar f) =
        case f of
            Setting _ -> pure $ set m
            Unsetting _ -> pure $ unset m
            Getting _ -> fail "invalid modestring"
    | otherwise = empty

fetchModeStr :: ServerModes -> ByteString -> Maybe ModeStr
fetchModeStr smodes = either (const Nothing) Just . A.parseOnly go
  where
    go = do
        ms <- modes
        A.skipSpace
        foldl1 (<>) <$> sequence ms
    modes = do
        ms <- A.takeTill A.isSpace
        let cs = snd $ 
                B.foldl
                    (\(next, xs) c ->
                         case c of
                             '+' -> (Setting, xs)
                             '-' -> (Unsetting, xs)
                             c' -> (next, next c' : xs))
                    (Getting, [])
                    ms
        pure $ map (matchMode smodes) cs

emitModeStr :: ModeStr -> ByteString
emitModeStr (ModeStr ms) =
    let cs = B.concat . toList . fmap opModeCmd $ ms
        ps = B.unwords . toList . fmap opModeParam $ ms
     in cs <> " " <> ps

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

opModeCmd :: ModeOp -> ByteString
opModeCmd (GetMode m) = B.snoc "+" $ modeChar m
opModeCmd (SetMode m _) = B.snoc "+" $ modeChar m
opModeCmd (UnsetMode m _) = B.snoc "-" $ modeChar m

opModeParam :: ModeOp -> ByteString
opModeParam (GetMode _) = mempty
opModeParam (SetMode _ a) = maybe mempty renderMode a
opModeParam (UnsetMode _ a) = maybe mempty renderMode a

newtype ModeStr = ModeStr { modeOps :: NonEmpty ModeOp }
    deriving Semigroup

get :: Mode 'TypeA a -> ModeStr
get = ModeStr . pure . GetMode

class SetMode m where
    type ModeSetter m
    set :: m -> ModeSetter m

instance ModeParameter a => SetMode (Mode 'TypeA a) where
    type ModeSetter (Mode 'TypeA a) = a -> ModeStr
    set m a = ModeStr . pure $ SetMode m (Just a)

instance ModeParameter a => SetMode (Mode 'TypeB a) where
    type ModeSetter (Mode 'TypeB a) = a -> ModeStr
    set m a = ModeStr . pure $ SetMode m (Just a)

instance ModeParameter a => SetMode (Mode 'TypeC a) where
    type ModeSetter (Mode 'TypeC a) = a -> ModeStr
    set m a = ModeStr . pure $ SetMode m (Just a)

instance SetMode (Mode 'TypeD Void) where
    type ModeSetter (Mode 'TypeD Void) = ModeStr
    set m = ModeStr . pure $ SetMode m Nothing

class UnsetMode m where
    type ModeUnsetter m
    unset :: m -> ModeUnsetter m

instance ModeParameter a => UnsetMode (Mode 'TypeA a) where
    type ModeUnsetter (Mode 'TypeA a) = a -> ModeStr
    unset m a = ModeStr . pure $ UnsetMode m (Just a)

instance ModeParameter a => UnsetMode (Mode 'TypeB a) where
    type ModeUnsetter (Mode 'TypeB a) = a -> ModeStr
    unset m a = ModeStr . pure $ UnsetMode m (Just a)

instance ModeParameter a => UnsetMode (Mode 'TypeC a) where
    type ModeUnsetter (Mode 'TypeC a) = ModeStr
    unset m = ModeStr . pure $ UnsetMode m Nothing

instance UnsetMode (Mode 'TypeD Void) where
    type ModeUnsetter (Mode 'TypeD Void) = ModeStr
    unset m = ModeStr . pure $ UnsetMode m Nothing

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

defaultModes :: ServerModes
defaultModes =
    ServerModes
    { typeAModes =
          [ OpaqueMode ban
          , OpaqueMode exception
          , OpaqueMode inviteOnlyException]
    , typeBModes = [OpaqueMode key]
    , typeCModes = [OpaqueMode clientLimit]
    , typeDModes =
          [ OpaqueMode moderated
          , OpaqueMode protectedTopic
          , OpaqueMode noExternal
          ]
    }

{-
 -test :: ModeStr
 -test = set ban "foo!bar@qux" <> unset exception "qux!fuu@bar" <> set moderated
 -}
