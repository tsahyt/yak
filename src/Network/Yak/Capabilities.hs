-- | Capability negotiation as per IRCv3 spec. Based on v3.1
-- <http://ircv3.net/specs/core/capability-negotiation-3.1.html> and v3.2
-- <http://ircv3.net/specs/core/capability-negotiation-3.2.html>.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Network.Yak.Capabilities
(
    Identifier,
    Subcommand,
    CapModifier(..),
    _ModDisable,
    _ModAck,
    _ModSticky,
    _ModNone,
    Capability(..),
    modifier,
    capability,
    ReqAnswer(..),
    _Ack,
    _Nak,

    -- * Invalid Capabilities
    ErrInvalidcap,
    errInvalidcapIdentifier,
    errInvalidcapCommand,
    errInvalidcapMessage,

    -- * Capability Messages
    -- ** LS
    CapLs,
    capLs302,
    SrvCapLs,
    srvCapLsIdentifier,
    srvCapLsUnused,
    srvCapLsMultiLine,
    srvCapLsCapabilities,
    
    -- ** LIST
    CapList,
    SrvCapList,
    srvCapListIdentifier,
    srvCapListUnused,
    srvCapListMultiLine,
    srvCapListCapabilities,

    -- ** REQ
    CapReq,
    capReqCapabilities,
    SrvCapReq,
    srvCapReqIdentifier,
    srvCapReqAnswer,
    srvCapReqCapabilities,
    CapAck,
    capAckCapabilities,
    CapEnd,

    -- ** NEW/DEL
    SrvCapNew,
    srvCapNewIdentifier,
    srvCapNewSubcommand,
    srvCapNewCapabilities,
    SrvCapDel,
    srvCapDelIdentifier,
    srvCapDelSubcommand,
    srvCapDelCapabilities,
)
where

import Control.Lens.TH
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (string, choice, char)
import Data.Text (Text)
import Network.Yak.TH
import Network.Yak.Types

type Identifier = Either (Unused "*") Text
type Subcommand = Unused

data CapModifier
    = ModDisable
    | ModAck            -- ^ Deprecated as of v3.2
    | ModSticky         -- ^ Deprecated as of v3.2
    | ModNone
    deriving (Eq, Show, Read, Ord)

makePrisms ''CapModifier

declareLenses [d|
    data Capability = Capability
        { modifier   :: CapModifier 
        , capability :: Text
        }
        deriving (Eq, Show, Read, Ord)
    |]

instance Parameter Capability where
    render (Capability m c) = render $ modChar m `mappend` c
        where modChar ModDisable = "-"
              modChar ModAck = "~"
              modChar ModSticky = "="
              modChar ModNone = ""
    seize = Capability 
        <$> choice [ ModDisable <$ char '-'
                   , ModAck <$ char '~'
                   , ModSticky <$ char '='
                   , pure ModNone ]
        <*> seize

-- | A REQ ('CapReq', 'SrvCapReq'), can be answered either with an ACK or a
-- NAK.
data ReqAnswer = Ack | Nak
    deriving (Eq, Show, Read, Ord)

makePrisms ''ReqAnswer

instance Parameter ReqAnswer where
    render Ack = "ACK"
    render Nak = "NAK"

    seize = (Ack <$ string "ACK") <|> (Nak <$ string "NAK")


-- | > 410 * <command> :Invalid CAP command
type ErrInvalidcap = Msg "410" [Identifier, Text, Message]
makeMsgLenses ''ErrInvalidcap ["identifier", "command", "message"]

-- | The LS subcommand is used to list the capabilities supported by the
-- server.  The client should send an LS subcommand with no other arguments to
-- solicit a list of all capabilities.
--
-- Since v3.2, LS can be invoked with a 302 flag.
type CapLs = Msg "CAP LS" '[Flag "302"]
makeMsgLenses ''CapLs ["302"]

-- | All but the last line in multi-line responses must have the * enabled.
type SrvCapLs = Msg "CAP" 
    '[Identifier, Subcommand "LS", Flag "*", CList Capability]
makeMsgLenses ''SrvCapLs ["identifier", "unused", "multiLine", "capabilities"]

-- | The LIST subcommand is used to list the capabilities associated with the
-- active connection. The client should send a LIST subcommand with no other
-- arguments to solicit a list of active capabilities.  If no capabilities are
-- active, an empty parameter must be sent.
type CapList = Msg "CAP LIST" '[]

-- | All but the last line in multi-line responses must have the * enabled.
type SrvCapList = Msg "CAP" 
    '[Identifier, Subcommand "LIST", Flag "*", CList Capability]
makeMsgLenses ''SrvCapList ["identifier", "unused", "multiLine", "capabilities"]

-- | The REQ subcommand is used to request a change in capabilities associated
-- with the active connection. It’s sole parameter must be a list of
-- space-separated capability identifiers. Each capability identifier may be
-- prefixed with a dash (-) to designate that the capability should be disabled.
type CapReq = Msg "CAP REQ" '[CList Capability]
makeMsgLenses ''CapReq ["capabilities"]

type SrvCapReq = Msg "CAP" '[Identifier, ReqAnswer, CList Capability]
makeMsgLenses ''SrvCapReq ["identifier", "answer", "capabilities"]

-- | Client side ACK.
type CapAck = Msg "CAP ACK" '[CList Capability]
makeMsgLenses ''CapAck ["capabilities"]

-- | The END subcommand signals to the server that capability negotiation is
-- complete and requests that the server continue with client registration. 
type CapEnd = Msg "CAP END" '[]

type SrvCapNew = Msg "CAP" '[Identifier, Subcommand "NEW", CList Capability]
makeMsgLenses ''SrvCapNew ["identifier", "subcommand", "capabilities"]

type SrvCapDel = Msg "CAP" '[Identifier, Subcommand "DEL", CList Capability]
makeMsgLenses ''SrvCapDel ["identifier", "subcommand", "capabilities"]
