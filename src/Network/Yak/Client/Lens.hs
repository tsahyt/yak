-- | Lenses for "Network.Yak.Client".
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Network.Yak.Client.Lens
(
    -- * Connection Registration
    -- ** Pass
    passPassword,

    -- ** Nick
    nickNickname,

    -- ** User
    userUser,
    userMode,
    userUnused,
    userRealname,

    -- ** Server
    serverServername,
    serverHopCount,
    serverInfo,

    -- ** Oper
    operUsername,
    operPassword,

    -- * Channel Operations
    -- ** Join
    joinChannels,
    
    -- ** Part
    partChannels,

    -- ** Quit
    quitMessage,

    -- ** SQuit
    sQuitServer,
    sQuitComment,

    -- ** UMode
    uModeNickname,
    uModeModes,

    -- ** CMode
    cModeChannel,
    cModeModes,

    -- ** GetModes
    getModeChannel,
    getModeModes,

    -- ** Names
    namesChannels,
    namesTarget,

    -- ** List
    listChannels,
    listTarget,

    -- ** Invite
    inviteNickname,
    inviteChannel,

    -- ** Kick
    kickChannels,
    kickUsers,
    kickMessage,

    -- ** Topic
    topicChannel,
    topicMessage,

    -- ** Version
    versionTarget,

    -- ** Motd
    motdTarget,
    
    -- * Sending Messages
    -- ** PrivMsg
    privMsgChannels,
    privMsgMessage,

    -- ** Notice
    noticeChannels,
    noticeMessage,

    -- * User-based Queries
    -- ** Who
    whoMask,
    whoOnlyOps,

    -- ** WhoIs
    whoIsTarget,
    whoIsMasks,

    -- ** WhoWas
    whoWasNickname,
    whoWasTarget,

    -- * Misc
    -- ** Ping
    pingServer,
    pingTarget,

    -- ** Pong
    pongServer,
    pongTarget,

    -- ** Error
    errorMessage
)
where

import Network.Yak.TH
import Network.Yak.Types
import Network.Yak.Client

makeMsgLenses ''Pass    ["password"]
makeMsgLenses ''Nick    ["nickname"]
makeMsgLenses ''User    ["user", "mode", "unused", "realname"]
makeMsgLenses ''Server  ["servername", "hopCount", "info"]
makeMsgLenses ''Oper    ["username", "password"]
makeMsgLenses ''Join    ["channels"]
makeMsgLenses ''Part    ["channels"]
makeMsgLenses ''Quit    ["message"]
makeMsgLenses ''SQuit   ["server", "comment"]
makeMsgLenses ''UMode   ["nickname", "modes"]
makeMsgLenses ''CMode   ["channel", "modes"]
makeMsgLenses ''GetMode ["channel", "modes"]
makeMsgLenses ''Names   ["channels", "target"]
makeMsgLenses ''List    ["channels", "target"]
makeMsgLenses ''Invite  ["nickname", "channel"]
makeMsgLenses ''Kick    ["channels", "users", "message"]
makeMsgLenses ''Topic   ["channel", "message"]
makeMsgLenses ''Version ["target"]
makeMsgLenses ''Motd    ["target"]
makeMsgLenses ''PrivMsg ["channels", "message"]
makeMsgLenses ''Notice  ["channels", "message"]
makeMsgLenses ''Who     ["mask", "onlyOps"]
makeMsgLenses ''WhoIs   ["target", "masks"]
makeMsgLenses ''WhoWas  ["nickname", "target"]
makeMsgLenses ''Ping    ["server", "target"]
makeMsgLenses ''Pong    ["server", "target"]
makeMsgLenses ''Error   ["message"]
