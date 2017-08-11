-- | Messages for IRC Clients, implementing the current "living standard", to be
-- found at <https://modern.ircdocs.horse/>. This module does not include the
-- responses.  See "Network.Yak.Response" for standard-compliant responses.
{-# LANGUAGE TemplateHaskell #-}
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
    passPassword,
    Nick,
    nickNickname,
    User,
    userUsername,
    userMode,
    userUnused,
    userRealname,
    Oper,
    operName,
    operPassword,
    Quit,
    quitMessage,

    -- * Channel Operations
    Join,
    Join0,
    joinChannels,
    joinKeys,
    Part,
    partChannels,
    partMessage,
    Topic,
    topicChannel,
    topicMessage,
    Names,
    namesChannels,
    List,
    listChannels,
    listElistCond,

    -- * Server Queries and Commands
    Motd,
    motdTarget,
    Lusers,
    lusersParam,
    lusersMask,
    lusersTarget,
    Version,
    versionTarget,
    Admin,
    adminTarget,
    Connect,
    connectTarget,
    connectConnInfo,
    Time,
    timeTarget,
    Stats,
    statsQuery,
    statsTarget,
    Info,
    infoTarget,
    Mode,
    modeTarget,
    modeSetter,
    modeChannel,
    modeNick,
    modeString,
    modeParams,

    -- * Sending Messages
    Privmsg,
    privmsgTargets,
    privmsgMessage,
    privmsgChannel,
    privmsgNick,
    Notice,
    noticeTargets,
    noticeMessage,
    noticeChannel,
    noticeNick,

    -- * User-based queries
    Who,
    whoMask,
    whoFlag,
    WhoIs,
    whoIsTarget,
    whoIsMasks,
    WhoWas,
    whoWasNicks,
    whoWasParam,
    whoWasCount,
    whoWasTarget,

    -- * Optional Messages
    Userhost,
    userhostNick1,
    userhostNick2,
    userhostNick3,
    userhostNick4,
    userhostNick5,

    -- * Miscellaneous Messages
    Ping,
    pingServer1,
    pingServer2,
    Pong,
    pongServer1,
    pongServer2,
    Kill,
    killNick,
    killMessage
)
where

import Control.Lens
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word)
import Network.Yak.TH
import Network.Yak.Types

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
makeMsgLenses ''Pass     ["password"]

-- | > NICK <nickname>
type Nick = Msg "NICK" '[Nickname]
makeMsgLenses ''Nick     ["nickname"]

-- | > USER <username> 0 * <realname>
type User = Msg "USER" '[Username, Word, Unused "*", Message]
makeMsgLenses ''User     ["username", "mode", "unused", "realname"]

-- | > OPER <name> <password>
type Oper = Msg "OPER" '[Nickname, Text]
makeMsgLenses ''Oper     ["name", "password"]

-- | > QUIT [<reason>]
type Quit = Msg "QUIT" '[Message]
makeMsgLenses ''Quit     ["message"]


-- Channel Operations
-- | > JOIN <channel>{,<channel>} [<key>{,<key>}]
type Join = Msg "JOIN" '[NonEmpty Channel, [Text]]
type Join0 = Msg "JOIN" '[Unused "0"]
makeMsgLenses ''Join     ["channels", "keys"]

-- | > PART <channel>{,<channel>} [<reason>]
type Part = Msg "PART" '[NonEmpty Channel, Maybe Message]
makeMsgLenses ''Part     ["channels", "message"]

-- | > TOPIC <channel> [<topic>]
type Topic = Msg "TOPIC" '[Channel, Maybe Message]
makeMsgLenses ''Topic    ["channel", "message"]

-- | > NAMES [<channel>{,<channel>}]
type Names = Msg "NAMES" '[[Channel]]
makeMsgLenses ''Names    ["channels"]

-- | > LIST [<channel>{,<channel>}] [<elistcond>{,<elistcond>}]
type List = Msg "LIST" '[[Channel], Maybe Text]
makeMsgLenses ''List     ["channels", "elistCond"]


-- Server Queries and Commands
-- | > MOTD [<target>]
type Motd = Msg "MOTD" '[Hostname]
makeMsgLenses ''Motd     ["target"]

-- | > LUSERS [<mask> [<target>]]
type Lusers = Msg "LUSERS" '[Maybe (Mask, Maybe Hostname)]
makeMsgLenses ''Lusers   ["param"]

lusersMask :: Traversal' Lusers Mask
lusersMask = lusersParam . _Just . _1

lusersTarget :: Traversal' Lusers Hostname
lusersTarget = lusersParam . _Just . _2 . _Just


-- | > VERSION [<target>]
type Version = Msg "VERSION" '[Maybe Hostname]
makeMsgLenses ''Version  ["target"]

-- | > ADMIN [<target>]
type Admin = Msg "ADMIN" '[Maybe Hostname]
makeMsgLenses ''Admin    ["target"]

-- | > CONNECT <target server> [<port> [<remote server>]]
type Connect = Msg "CONNECT" '[Hostname, Maybe (Int, Maybe Hostname)]
makeMsgLenses ''Connect  ["target", "connInfo"]

-- | > TIME [<server>]
type Time = Msg "TIME" '[Maybe Hostname]
makeMsgLenses ''Time     ["target"]

-- | > STATS <query> [<server>]
type Stats = Msg "STATS" '[Char, Maybe Hostname]
makeMsgLenses ''Stats    ["query", "target"]

-- | > INFO [<target>]
type Info = Msg "INFO" '[Maybe Hostname]
makeMsgLenses ''Info     ["target"]

-- | > MODE <target> [<modestring> [<mode arguments>...]]
type Mode = Msg "MODE" 
    '[Either Channel Nickname, Maybe (ModeString, SList Text)]
makeMsgLenses ''Mode     ["target", "setter"]

modeChannel :: Traversal' Mode Channel
modeChannel = modeTarget . _Left

modeNick :: Traversal' Mode Nickname
modeNick = modeTarget . _Right

modeString :: Traversal' Mode ModeString
modeString = modeSetter . _Just . _1

modeParams :: Traversal' Mode [Text]
modeParams = modeSetter . _Just . _2 . _Wrapped


-- Sending Messages
-- | > PRIVMSG <target>{,<target>} <text to be sent>
type Privmsg = Msg "PRIVMSG" '[NonEmpty (Either Channel Nickname), Message]
makeMsgLenses ''Privmsg  ["targets", "message"]

privmsgChannel :: Traversal' Privmsg Channel
privmsgChannel = privmsgTargets . traverse . _Left

privmsgNick :: Traversal' Privmsg Nickname
privmsgNick = privmsgTargets . traverse . _Right

-- | > NOTICE <target>{,<target>} <text to be sent>
type Notice = Msg "NOTICE" '[NonEmpty (Either Channel Nickname), Message]
makeMsgLenses ''Notice   ["targets", "message"]

noticeChannel :: Traversal' Notice Channel
noticeChannel = noticeTargets . traverse . _Left

noticeNick :: Traversal' Notice Nickname
noticeNick = noticeTargets . traverse . _Right

-- User based queries
-- | > WHO [<mask>] [o]
type Who    = Msg "WHO" '[Maybe Mask, Flag "o"]
makeMsgLenses ''Who      ["mask", "flag"]

-- | > WHOIS [<target>] <mask>{,<mask>}
type WhoIs  = Msg "WHOIS" '[Maybe Hostname, NonEmpty Mask]
makeMsgLenses ''WhoIs    ["target", "masks"]

-- | > WHOWAS <nickname>{,<nickname} [<count>, [<target>]]
type WhoWas = Msg "WHOWAS" '[NonEmpty Nickname, Maybe (Int, Maybe Hostname)]
makeMsgLenses ''WhoWas   ["nicks", "param"]

whoWasCount :: Traversal' WhoWas Int
whoWasCount = whoWasParam . _Just . _1

whoWasTarget :: Traversal' WhoWas Hostname
whoWasTarget = whoWasParam . _Just . _2 . _Just


-- Optional Messages
-- | > USERHOST <nickname>{ <nickname>}
type Userhost = Msg "USERHOST" 
    '[Nickname, Maybe Nickname, Maybe Nickname, Maybe Nickname, Maybe Nickname]
makeMsgLenses ''Userhost ["nick1", "nick2", "nick3", "nick4", "nick5"]
    -- because more than 5 would be ridiculous, and obviously we need space
    -- separation in random places. Thank you, IRC.

-- Miscellaneous Messages
-- | > PING <server1> [<server2>]
type Ping = Msg "PING" '[Hostname, Maybe Hostname]
makeMsgLenses ''Ping     ["server1", "server2"]

-- | > PONG <server1> [<server2>]
type Pong = Msg "PONG" '[Hostname, Maybe Hostname]
makeMsgLenses ''Pong     ["server1", "server2"]

-- | > KILL <nickname> <comment>
type Kill = Msg "KILL" '[Nickname, Message]
makeMsgLenses ''Kill     ["nick", "message"]
