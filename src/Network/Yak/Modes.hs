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
{-# LANGUAGE PatternSynonyms #-}

-- | A small DSL for creating mode strings. The documentation contains parts
-- from <https://modern.ircdocs.horse> only for quick reference. Please refer to
-- the source for up to date documentation on individual modes!

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
    -- | Mode Strings can be built using the following three combinators. The
    -- types are polymorphic over different mode types and therefore do not
    -- fully reflect their use.
    --
    -- As an example consider
    --
    -- > foo :: ModeStr
    -- > foo = set ban "foo!bar@quux" <> unset moderated
    --
    -- which translates to
    --
    -- > +b+m foo!bar@quux
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
    , secret
    , moderated
    , protectedTopic
    , noExternal
    -- * Common User Modes
    , UserMode
    , pattern UserMode
    , invisible
    , oper
    , localOper
    , registered
    , wallops
    -- * Channel Member Prefix Modes
    , PrefixMode
    , pattern PrefixMode
    , founder
    , protected
    , operator
    , halfop
    , voice
    -- * Server Modes
    , ServerModes(..)
    , defaultModes
    , defaultPrefixModes
    , defaultUserModes
    -- * Types
    , HostMask
    ) where

import Control.Applicative
import Control.Monad (guard)
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

-- | A mode is defined through a character and its type.
--
--  * 'TypeA': Modes that add or remove an address to or from a list. These
--  modes MUST always have a parameter when sent from the server to a client. A
--  client MAY issue this type of mode without an argument to obtain the current
--  contents of the list. The numerics used to retrieve contents of Type A modes
--  depends on the specific mode. Also see the EXTBAN parameter.
--
--  * 'TypeB': Modes that change a setting on a channel. These modes MUST always
--  have a parameter.
--
--  * 'TypeC': Modes that change a setting on a channel. These modes MUST have a
--  parameter when being set, and MUST NOT have a parameter when being unset.
--
--  * 'TypeD': Modes that change a setting on a channel. These modes MUST NOT
--  have a parameter.
--
--  The final type parameter to 'Mode' determines the parameter type of the
--  defined mode.
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

-- | An existential wrapper around modes that hides the parameter type, such
-- that modes of a similar type can be grouped together in a simple container,
-- such as those in a 'ServerModes'.
data OpaqueMode (t :: ModeType) =
    forall a. ModeParameter a => OpaqueMode (Mode t a)

matchOpaque :: Char -> OpaqueMode t -> Bool
matchOpaque c (OpaqueMode m) = modeChar m == c

-- | 'ServerModes' lets you define a dictionary of modes to be passed to the
-- parser (see 'fetchModeStr'). Modes are very server specific and it is not
-- possible to achieve good coverage/compatibility here, so this needs to be
-- done in user code. See 'defaultModes' for a starting point. Note that
-- 'ServerModes' is a monoid, so you can easily augment the standardized modes
-- with custom server specific ones.
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

-- | Fetch a 'ModeStr' from a 'ByteString', given some defined collection of
-- modes in the form of a 'ServerModes'. Will return 'Nothing' if the parsing
-- fails.
fetchModeStr :: ServerModes -> ByteString -> Maybe ModeStr
fetchModeStr smodes = either (const Nothing) Just . A.parseOnly go
  where
    go = do
        ms <- modes
        A.skipSpace
        guard $ not . null $ ms
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

-- | Emit a 'ModeStr' to a 'ByteString' for use in a message.
emitModeStr :: ModeStr -> ByteString
emitModeStr (ModeStr ms) =
    let cs = B.concat . toList . fmap opModeCmd $ ms
        ps = B.unwords . toList . fmap opModeParam $ ms
     in cs <> " " <> ps

-- | Mode Parameters are types that can be used as parameters to some mode.
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

-- | Operations that can be performed with a mode.
data ModeOp where
    -- | Type A modes can be retrieved in list form.
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

-- | This channel mode controls a list of client masks that are ‘banned’ from
-- joining or speaking in the channel. If this mode has values, each of these
-- values should be a client mask.
ban :: Mode 'TypeA HostMask
ban = ModeTypeA 'b'

-- | This channel mode controls a list of client masks that are exempt from the
-- ‘ban’ channel mode. If this mode has values, each of these values should be a
-- client mask.
exception :: Mode 'TypeA HostMask
exception = ModeTypeA 'e'

-- | This channel mode controls whether new users may join based on the number
-- of users who already exist in the channel. If this mode is set, its value is
-- an integer and defines the limit of how many clients may be joined to the
-- channel.
clientLimit :: Mode 'TypeC Int
clientLimit = ModeTypeC 'l'

-- | This channel mode controls whether new users need to be invited to the
-- channel before being able to join.
inviteOnly :: Mode 'TypeD Void
inviteOnly = ModeTypeD 'i'

-- | This channel mode controls a list of channel masks that are exempt from the
-- invite-only channel mode. If this mode has values, each of these values
-- should be a client mask.
inviteOnlyException :: Mode 'TypeA HostMask
inviteOnlyException = ModeTypeA 'I'

-- | This mode letter sets a ‘key’ that must be supplied in order to join this
-- channel. If this mode is set, its’ value is the key that is required.
key :: Mode 'TypeB Text
key = ModeTypeB 'k'

-- | This channel mode controls whether the channel is ‘secret’, and does not
-- have any value.
secret :: Mode 'TypeD Void
secret = ModeTypeD 's'

-- | This channel mode controls whether users may freely talk on the channel,
-- and does not have any value.
moderated :: Mode 'TypeD Void
moderated = ModeTypeD 'm'

-- | This channel mode controls whether channel privileges are required to set
-- the topic, and does not have any value.
protectedTopic :: Mode 'TypeD Void
protectedTopic = ModeTypeD 't'

-- | This channel mode controls whether users who are not joined to the channel
-- can send messages to it, and does not have any value.
noExternal :: Mode 'TypeD Void
noExternal = ModeTypeD 'n'

type UserMode = Mode 'TypeD Void

pattern UserMode :: Char -> UserMode
pattern UserMode c = ModeTypeD c

-- | If a user is set to ‘invisible’, they will not show up in commands such as
-- WHO unless they share a channel with the user that submitted the command. In
-- addition, the only channels that will show up in a WHOIS of an invisible user
-- will be those they share with the user that submitted the command.
invisible :: UserMode
invisible = UserMode 'i'

-- | If a user has this mode, this indicates that they are a network operator.
oper :: UserMode
oper = UserMode 'o'

-- | If a user has this mode, this indicates that they are a server operator. A
-- local operator has operator privileges for their server, and not for the rest
-- of the network.
localOper :: UserMode
localOper = UserMode 'O'

-- | If a user has this mode, this indicates that they have logged into a user
-- account.
registered :: UserMode
registered = UserMode 'r'

-- | If a user has this mode, this indicates that they will receive WALLOPS
-- messages from the server.
wallops :: UserMode
wallops = UserMode 'w'

type PrefixMode = Mode 'TypeB HostMask

pattern PrefixMode :: Char -> PrefixMode
pattern PrefixMode c = ModeTypeB c

founder :: PrefixMode
founder = PrefixMode 'q'

protected :: PrefixMode
protected = PrefixMode 'a'

operator :: PrefixMode
operator = PrefixMode 'o'

halfop :: PrefixMode
halfop = PrefixMode 'h'

voice :: PrefixMode
voice = PrefixMode 'v'

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
          , OpaqueMode secret
          ]
    }

defaultUserModes :: ServerModes
defaultUserModes =
    ServerModes
    { typeAModes = []
    , typeBModes = []
    , typeCModes = []
    , typeDModes =
          [ OpaqueMode invisible
          , OpaqueMode oper
          , OpaqueMode localOper
          , OpaqueMode registered
          , OpaqueMode wallops
          ]
    }

defaultPrefixModes :: ServerModes
defaultPrefixModes =
    ServerModes
    { typeAModes = []
    , typeBModes =
          [ OpaqueMode founder
          , OpaqueMode protected
          , OpaqueMode operator
          , OpaqueMode halfop
          , OpaqueMode voice
          ]
    , typeCModes = []
    , typeDModes = []
    }
