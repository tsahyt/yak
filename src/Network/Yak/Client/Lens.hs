{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Network.Yak.Client.Lens where

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
