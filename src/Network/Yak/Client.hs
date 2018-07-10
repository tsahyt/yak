-- | Messages for IRC Clients, implementing the current "living standard", to be
-- found at <https://modern.ircdocs.horse/>. This module does not include the
-- responses.  See "Network.Yak.Response" for standard-compliant responses.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Yak.Client
(
    -- * Connection Messages
    Authenticate,
    authenticateArgument,
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
    killMessage,

    -- * Extras
    -- | Common messages that are not specified out in the document above.
    Kick,
    kickChannels,
    kickNicknames,
    kickMessage,
    Invite,
    inviteNickname,
    inviteChannel,

    -- * Common Accessors
    HasChannel(..),
    HasNick(..),
    HasHostname(..)
)
where

import Control.Lens
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word)
import Network.Yak.TH
import Network.Yak.Types

-- Connection Messages
-- | > AUTHENTICATE
type Authenticate = Msg "AUTHENTICATE" '[Text]
makeMsgLenses ''Authenticate ["argument"]

-- | > PASS <password>
type Pass = Msg "PASS" '[Text]
makeMsgLenses ''Pass ["password"]

-- | > NICK <nickname>
type Nick = Msg "NICK" '[Nickname]
makeMsgLenses ''Nick ["nickname"]

-- | > USER <username> 0 * <realname>
type User = Msg "USER" '[Username, Word, Unused "*", Message]
makeMsgLenses ''User ["username", "mode", "unused", "realname"]

-- | > OPER <name> <password>
type Oper = Msg "OPER" '[Nickname, Text]
makeMsgLenses ''Oper ["name", "password"]

-- | > QUIT [<reason>]
type Quit = Msg "QUIT" '[Message]
makeMsgLenses ''Quit ["message"]


-- Channel Operations
-- | > JOIN <channel>{,<channel>} [<key>{,<key>}]
type Join = Msg "JOIN" '[NonEmpty Channel, [Text]]
type Join0 = Msg "JOIN" '[Unused "0"]
makeMsgLenses ''Join ["channels", "keys"]

-- | > PART <channel>{,<channel>} [<reason>]
type Part = Msg "PART" '[NonEmpty Channel, Maybe Message]
makeMsgLenses ''Part ["channels", "message"]

-- | > TOPIC <channel> [<topic>]
type Topic = Msg "TOPIC" '[Channel, Maybe Message]
makeMsgLenses ''Topic ["channel", "message"]

-- | > NAMES [<channel>{,<channel>}]
type Names = Msg "NAMES" '[[Channel]]
makeMsgLenses ''Names ["channels"]

-- | > LIST [<channel>{,<channel>}] [<elistcond>{,<elistcond>}]
type List = Msg "LIST" '[[Channel], Maybe Text]
makeMsgLenses ''List ["channels", "elistCond"]


-- Server Queries and Commands
-- | > MOTD [<target>]
type Motd = Msg "MOTD" '[Hostname]
makeMsgLenses ''Motd ["target"]

-- | > LUSERS [<mask> [<target>]]
type Lusers = Msg "LUSERS" '[Maybe (Mask, Maybe Hostname)]
makeMsgLenses ''Lusers ["param"]

lusersMask :: Traversal' Lusers Mask
lusersMask = lusersParam . _Just . _1

lusersTarget :: Traversal' Lusers Hostname
lusersTarget = lusersParam . _Just . _2 . _Just


-- | > VERSION [<target>]
type Version = Msg "VERSION" '[Maybe Hostname]
makeMsgLenses ''Version ["target"]

-- | > ADMIN [<target>]
type Admin = Msg "ADMIN" '[Maybe Hostname]
makeMsgLenses ''Admin ["target"]

-- | > CONNECT <target server> [<port> [<remote server>]]
type Connect = Msg "CONNECT" '[Hostname, Maybe (Int, Maybe Hostname)]
makeMsgLenses ''Connect ["target", "connInfo"]

-- | > TIME [<server>]
type Time = Msg "TIME" '[Maybe Hostname]
makeMsgLenses ''Time ["target"]

-- | > STATS <query> [<server>]
type Stats = Msg "STATS" '[Char, Maybe Hostname]
makeMsgLenses ''Stats ["query", "target"]

-- | > INFO [<target>]
type Info = Msg "INFO" '[Maybe Hostname]
makeMsgLenses ''Info ["target"]

-- | > MODE <target> [<modestring> [<mode arguments>...]]
type Mode = Msg "MODE" 
    '[Either Channel Nickname, Maybe (ModeString, SList Text)]
makeMsgLenses ''Mode ["target", "setter"]

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
makeMsgLenses ''Privmsg ["targets", "message"]

privmsgChannel :: Traversal' Privmsg Channel
privmsgChannel = privmsgTargets . traverse . _Left

privmsgNick :: Traversal' Privmsg Nickname
privmsgNick = privmsgTargets . traverse . _Right

-- | > NOTICE <target>{,<target>} <text to be sent>
type Notice = Msg "NOTICE" '[NonEmpty (Either Channel Nickname), Message]
makeMsgLenses ''Notice ["targets", "message"]

noticeChannel :: Traversal' Notice Channel
noticeChannel = noticeTargets . traverse . _Left

noticeNick :: Traversal' Notice Nickname
noticeNick = noticeTargets . traverse . _Right

-- User based queries
-- | > WHO [<mask>] [o]
type Who    = Msg "WHO" '[Maybe Mask, Flag "o"]
makeMsgLenses ''Who ["mask", "flag"]

-- | > WHOIS [<target>] <mask>{,<mask>}
type WhoIs  = Msg "WHOIS" '[Maybe Hostname, NonEmpty Mask]
makeMsgLenses ''WhoIs ["target", "masks"]

-- | > WHOWAS <nickname>{,<nickname} [<count>, [<target>]]
type WhoWas = Msg "WHOWAS" '[NonEmpty Nickname, Maybe (Int, Maybe Hostname)]
makeMsgLenses ''WhoWas ["nicks", "param"]

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
makeMsgLenses ''Ping ["server1", "server2"]

-- | > PONG <server1> [<server2>]
type Pong = Msg "PONG" '[Hostname, Maybe Hostname]
makeMsgLenses ''Pong ["server1", "server2"]

-- | > KILL <nickname> <comment>
type Kill = Msg "KILL" '[Nickname, Message]
makeMsgLenses ''Kill ["nick", "message"]

-- | > KICK <channel>(,<channel>)* <nickname>(,<nickname>)* [message]
-- As defined in RFC 2812
type Kick = Msg "KICK" '[NonEmpty Channel, NonEmpty Nickname, Maybe Message]
makeMsgLenses ''Kick ["channels", "nicknames", "message"]

-- | > INVITE <nickname> <channel>
-- As defined in RFC 2812
type Invite = Msg "INVITE" '[Nickname, Channel]
makeMsgLenses ''Invite ["nickname", "channel"]

-- | Class for extracting the channel out of messages that may contain channels
class HasChannel a where
    channel :: Traversal' a Channel

instance HasChannel Join where
    channel = joinChannels . traverse

instance HasChannel Part where
    channel = partChannels . traverse

instance HasChannel Topic where
    channel = topicChannel

instance HasChannel Names where
    channel = namesChannels . traverse

instance HasChannel List where
    channel = listChannels . traverse

instance HasChannel Privmsg where
    channel = privmsgChannel

instance HasChannel Mode where
    channel = modeChannel

instance HasChannel Notice where
    channel = noticeChannel

instance HasChannel Kick where
    channel = kickChannels . traverse

instance HasChannel Invite where
    channel = inviteChannel

-- | Messages containing a nickname in one (unambigious) position.
class HasNick a where
    nick :: Traversal' a Nickname

instance HasNick Nick where
    nick = nickNickname

instance HasNick Oper where
    nick = operName

instance HasNick Mode where
    nick = modeNick

instance HasNick Privmsg where
    nick = privmsgNick

instance HasNick Notice where
    nick = noticeNick

instance HasNick WhoWas where
    nick = whoWasNicks . traverse

instance HasNick Kill where
    nick = killNick

instance HasNick Kick where
    nick = kickNicknames . traverse

instance HasNick Invite where
    nick = inviteNickname

-- | Messages containing a hostname in one (unambigious) position
class HasHostname a where
    hostname :: Traversal' a Hostname

instance HasHostname Motd where
    hostname = motdTarget

instance HasHostname Lusers where
    hostname = lusersTarget

instance HasHostname Version where
    hostname = versionTarget . _Just

instance HasHostname Admin where
    hostname = adminTarget . _Just

instance HasHostname Time where
    hostname = timeTarget . _Just

instance HasHostname Stats where
    hostname = statsTarget . _Just

instance HasHostname Info where
    hostname = infoTarget . _Just

instance HasHostname WhoIs where
    hostname = whoIsTarget . _Just

instance HasHostname WhoWas where
    hostname = whoWasTarget
