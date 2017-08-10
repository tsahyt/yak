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

    -- * Optional Messages
    userhostNick1,
    userhostNick2,
    userhostNick3,
    userhostNick4,
    userhostNick5,
    
    -- * Miscellaneous Messages
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
makeMsgLenses ''Version  ["target"]
makeMsgLenses ''Admin    ["target"]
makeMsgLenses ''Connect  ["target", "connInfo"]
makeMsgLenses ''Time     ["target"]
makeMsgLenses ''Stats    ["query", "target"]
makeMsgLenses ''Info     ["target"]
makeMsgLenses ''Mode     ["target", "setter"]
makeMsgLenses ''Privmsg  ["targets", "message"]
makeMsgLenses ''Notice   ["targets", "message"]
makeMsgLenses ''Userhost ["nick1", "nick2", "nick3", "nick4", "nick5"]
makeMsgLenses ''Kill     ["nick", "message"]

modeChannel :: Traversal' Mode Channel
modeChannel = modeTarget . _Right

modeNick :: Traversal' Mode Nickname
modeNick = modeTarget . _Left

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
