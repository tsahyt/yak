{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Network.Yak.Capabilities
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (string)
import Data.Text (Text)
import Data.Void
import Network.Yak.TH
import Network.Yak.Types

type Identifier = Either (Unused "*") Text
type Capability = Text
type Subcommand = Unused

-- | > 410 * <command> :Invalid CAP command
type ErrInvalidcap = Msg "410" [Identifier, Text, Message]
makeMsgLenses ''ErrInvalidcap ["identifier", "command", "message"]

-- | The @LS@ subcommand is used to list the capabilities supported by the
-- server.  The client should send an LS subcommand with no other arguments to
-- solicit a list of all capabilities.
--
-- Since v3.2, @LS@ can be invoked with a 302 flag.
type CapLs = Msg "CAP LS" '[Flag "302"]

-- | All but the last line in multi-line responses must have the @*@ enabled.
type SrvCapLs = Msg "CAP" 
    '[Identifier, Subcommand "LS", Flag "*", CList Capability]

-- | The @LIST@ subcommand is used to list the capabilities associated with the
-- active connection. The client should send a LIST subcommand with no other
-- arguments to solicit a list of active capabilities.  If no capabilities are
-- active, an empty parameter must be sent.
type CapList = Msg "CAP LIST" '[]

-- | All but the last line in multi-line responses must have the @*@ enabled.
type SrvCapList = Msg "CAP" 
    '[Identifier, Subcommand "LIST", Flag "*", CList Capability]

-- | The @REQ@ subcommand is used to request a change in capabilities associated
-- with the active connection. It’s sole parameter must be a list of
-- space-separated capability identifiers. Each capability identifier may be
-- prefixed with a dash (-) to designate that the capability should be disabled.
type CapReq = Msg "CAP REQ" '[CList Capability]
type SrvCapReq = Msg "CAP" '[Identifier, ReqAnswer, CList Capability]

-- | A @REQ@ can be answered either with an @ACK@ or a @NAK@.
data ReqAnswer = Ack | Nak
    deriving (Eq, Show, Read, Ord)

instance Parameter ReqAnswer where
    render Ack = "ACK"
    render Nak = "NAK"

    seize = (Ack <$ string "ACK") <|> (Nak <$ string "NAK")

-- | The @END@ subcommand signals to the server that capability negotiation is
-- complete and requests that the server continue with client registration. 
type CapEnd = Msg "CAP END" '[]
