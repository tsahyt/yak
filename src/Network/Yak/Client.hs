-- | Messages for IRC Clients, implementing the current "living standard", to be
-- found at <https://modern.ircdocs.horse/>. This module does not include the
-- responses.  See "Network.Yak.Response" for standard-compliant responses.
{-# LANGUAGE DataKinds #-}
module Network.Yak.Client
(
    -- * Connection Messages
    -- | > CAP <subcommand> [:<capabilities>]
    CapLs,
    CapList,
    CapReq,
    CapAck,
    CapNak,
    CapEnd,

    Authenticate,
    Pass,
    Nick,
    User,
    Oper,
    Quit,

    -- * Channel Operations
    Join,
    Join0,
    Part,
    Topic,
    Names,
    List,

    -- * Server Queries and Commands
    Motd,
    Lusers,
    Version,
    Admin,
    Connect,
    Time,
    Stats,
    Info,
    Mode,

    -- * Sending Messages
    Privmsg,
    Notice,

    -- * User-based queries
    Who,
    WhoIs,
    WhoWas,

    -- * Optional Messages
    Userhost,

    -- * Miscellaneous Messages
    Ping,
    Pong,
    Kill
)
where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Network.Yak.Types
import Data.Word (Word)

-- Connection Messages
type CapLs   = Msg "CAP LS" '[]
type CapList = Msg "CAP LIST" '[]
type CapReq  = Msg "CAP REQ" '[Message]
type CapAck  = Msg "CAP ACK" '[]
type CapNak  = Msg "CAP NAK" '[]
type CapEnd  = Msg "CAP END" '[]

-- | > AUTHENTICATE
type Authenticate = Msg "AUTHENTICATE" '[Text]

-- | > PASS <password>
type Pass = Msg "PASS" '[Text]

-- | > NICK <nickname>
type Nick = Msg "NICK" '[Nickname]

-- | > USER <username> 0 * <realname>
type User = Msg "USER" '[Username, Word, Unused "*", Message]

-- | > OPER <name> <password>
type Oper = Msg "OPER" '[Nickname, Text]

-- | > QUIT [<reason>]
type Quit = Msg "QUIT" '[Message]


-- Channel Operations
-- | > JOIN <channel>{,<channel>} [<key>{,<key>}]
type Join = Msg "JOIN" '[NonEmpty Channel, [Text]]
type Join0 = Msg "JOIN" '[Unused "0"]

-- | > PART <channel>{,<channel>} [<reason>]
type Part = Msg "PART" '[NonEmpty Channel, Maybe Message]

-- | > TOPIC <channel> [<topic>]
type Topic = Msg "TOPIC" '[Channel, Maybe Message]

-- | > NAMES [<channel>{,<channel>}]
type Names = Msg "NAMES" '[[Channel]]

-- | > LIST [<channel>{,<channel>}] [<elistcond>{,<elistcond>}]
type List = Msg "LIST" '[[Channel], Maybe Text]


-- Server Queries and Commands
-- | > MOTD [<target>]
type Motd = Msg "MOTD" '[Hostname]

-- | > LUSERS [<mask> [<target>]]
type Lusers = Msg "LUSERS" '[Maybe (Mask, Maybe Hostname)]

-- | > VERSION [<target>]
type Version = Msg "VERSION" '[Maybe Hostname]

-- | > ADMIN [<target>]
type Admin = Msg "ADMIN" '[Maybe Hostname]

-- | > CONNECT <target server> [<port> [<remote server>]]
type Connect = Msg "CONNECT" '[Hostname, Maybe (Int, Maybe Hostname)]

-- | > TIME [<server>]
type Time = Msg "TIME" '[Maybe Hostname]

-- | > STATS <query> [<server>]
type Stats = Msg "STATS" '[Char, Maybe Hostname]

-- | > INFO [<target>]
type Info = Msg "INFO" '[Maybe Hostname]

-- | > MODE <target> [<modestring> [<mode arguments>...]]
type Mode = Msg "MODE" 
    '[Either Channel Nickname, Maybe (ModeString, SList Text)]

-- Sending Messages
-- | > PRIVMSG <target>{,<target>} <text to be sent>
type Privmsg = Msg "PRIVMSG" '[NonEmpty (Either Channel Nickname), Message]

-- | > NOTICE <target>{,<target>} <text to be sent>
type Notice = Msg "NOTICE" '[NonEmpty (Either Channel Nickname), Message]

-- User based queries
-- | > WHO [<mask>] [o]
type Who    = Msg "WHO" '[Maybe Mask, Flag "o"]

-- | > WHOIS [<target>] <mask>{,<mask>}
type WhoIs  = Msg "WHOIS" '[Maybe Hostname, NonEmpty Mask]

-- | > WHOWAS <nickname>{,<nickname} [<count>, [<target>]]
type WhoWas = Msg "WHOWAS" '[NonEmpty Nickname, Maybe (Int, Maybe Hostname)]

-- Optional Messages
-- | > USERHOST <nickname>{ <nickname>}
type Userhost = Msg "USERHOST" 
    '[Nickname, Maybe Nickname, Maybe Nickname, Maybe Nickname, Maybe Nickname]
    -- because more than 5 would be ridiculous, and obviously we need space
    -- separation in random places. Thank you, IRC.

-- Miscellaneous Messages
-- | > PING <server1> [<server2>]
type Ping = Msg "PING" '[Hostname, Maybe Hostname]

-- | > PONG <server1> [<server2>]
type Pong = Msg "PONG" '[Hostname, Maybe Hostname]

-- | > KILL <nickname> <comment>
type Kill = Msg "KILL" '[Nickname, Message]
