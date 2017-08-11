-- | Lenses for "Network.Yak.Client".
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Network.Yak.Client.Lens
(
    -- * Connection Messages
    passPassword,
    nickNickname,
    userUsername,
    userMode,
    userUnused,
    userRealname,
    operName,
    operPassword,
    quitMessage,
    
    -- * Channel Operations
    joinChannels,
    joinKeys,
    partChannels,
    partMessage,
    topicChannel,
    topicMessage,
    namesChannels,
    listChannels,
    listElistCond,
 
    -- * Server Queries and Commands
    motdTarget,
    lusersParam,
    lusersMask,
    lusersTarget,
    versionTarget,
    adminTarget,
    connectTarget,
    connectConnInfo,
    timeTarget,
    statsQuery,
    statsTarget,
    infoTarget,
    modeTarget,
    modeSetter,
    modeChannel,
    modeNick,
    modeString,
    modeParams,
 
    -- * Sending Messages
    privmsgTargets,
    privmsgMessage,
    privmsgChannel,
    privmsgNick,
    noticeTargets,
    noticeMessage,
    noticeChannel,
    noticeNick,

    -- * User-based Queries
    whoMask,
    whoFlag,
    whoIsTarget,
    whoIsMasks,
    whoWasNicks,
    whoWasParam,
    whoWasCount,
    whoWasTarget,

    -- * Optional Messages
    userhostNick1,
    userhostNick2,
    userhostNick3,
    userhostNick4,
    userhostNick5,
    
    -- * Miscellaneous Messages
    pingServer1,
    pingServer2,
    pongServer1,
    pongServer2,
    killNick,
    killMessage,
)
where

import Control.Lens
import Data.Text (Text)
import Network.Yak.Client
import Network.Yak.Types
import Network.Yak.TH

makeMsgLenses ''Pass     ["password"]
makeMsgLenses ''Nick     ["nickname"]
makeMsgLenses ''User     ["username", "mode", "unused", "realname"]
makeMsgLenses ''Oper     ["name", "password"]
makeMsgLenses ''Quit     ["message"]
makeMsgLenses ''Join     ["channels", "keys"]
makeMsgLenses ''Part     ["channels", "message"]
makeMsgLenses ''Topic    ["channel", "message"]
makeMsgLenses ''Names    ["channels"]
makeMsgLenses ''List     ["channels", "elistCond"]
makeMsgLenses ''Motd     ["target"]
makeMsgLenses ''Lusers   ["param"]
makeMsgLenses ''Version  ["target"]
makeMsgLenses ''Admin    ["target"]
makeMsgLenses ''Connect  ["target", "connInfo"]
makeMsgLenses ''Time     ["target"]
makeMsgLenses ''Stats    ["query", "target"]
makeMsgLenses ''Info     ["target"]
makeMsgLenses ''Mode     ["target", "setter"]
makeMsgLenses ''Privmsg  ["targets", "message"]
makeMsgLenses ''Notice   ["targets", "message"]
makeMsgLenses ''Who      ["mask", "flag"]
makeMsgLenses ''WhoIs    ["target", "masks"]
makeMsgLenses ''WhoWas   ["nicks", "param"]
makeMsgLenses ''Userhost ["nick1", "nick2", "nick3", "nick4", "nick5"]
makeMsgLenses ''Ping     ["server1", "server2"]
makeMsgLenses ''Pong     ["server1", "server2"]
makeMsgLenses ''Kill     ["nick", "message"]

lusersMask :: Traversal' Lusers Mask
lusersMask = lusersParam . _Just . _1

lusersTarget :: Traversal' Lusers Hostname
lusersTarget = lusersParam . _Just . _2 . _Just

whoWasCount :: Traversal' WhoWas Int
whoWasCount = whoWasParam . _Just . _1

whoWasTarget :: Traversal' WhoWas Hostname
whoWasTarget = whoWasParam . _Just . _2 . _Just

modeChannel :: Traversal' Mode Channel
modeChannel = modeTarget . _Left

modeNick :: Traversal' Mode Nickname
modeNick = modeTarget . _Right

modeString :: Traversal' Mode ModeString
modeString = modeSetter . _Just . _1

modeParams :: Traversal' Mode [Text]
modeParams = modeSetter . _Just . _2 . _Wrapped

privmsgChannel :: Traversal' Privmsg Channel
privmsgChannel = privmsgTargets . traverse . _Left

privmsgNick :: Traversal' Privmsg Nickname
privmsgNick = privmsgTargets . traverse . _Right

noticeChannel :: Traversal' Notice Channel
noticeChannel = noticeTargets . traverse . _Left

noticeNick :: Traversal' Notice Nickname
noticeNick = noticeTargets . traverse . _Right
