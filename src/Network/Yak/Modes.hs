{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Yak.Modes where

import Data.Semigroup
import Data.Text
import Language.Haskell.TH

class ModeParameter a

type ModeName = Char

data ModeSet where
    MsAGet :: ModeName -> ModeSet
    MsAPut :: ModeParameter a => ModeName -> a -> ModeSet
    MsADel :: ModeParameter a => ModeName -> a -> ModeSet

    MsBPut :: ModeParameter a => ModeName -> a -> ModeSet
    MsBDel :: ModeParameter a => ModeName -> a -> ModeSet

    MsCPut :: ModeParameter a => ModeName -> a -> ModeSet
    MsCDel :: ModeName -> ModeSet

    MsDPut :: ModeName -> ModeSet
    MsDDel :: ModeName -> ModeSet

newtype ModeStr = ModeStr [ModeSet]
    deriving (Monoid, Semigroup)

ms :: ModeSet -> ModeStr
ms = ModeStr . pure

data HostMask = HostMask

instance ModeParameter HostMask
instance ModeParameter Text
instance ModeParameter Int
instance (ModeParameter a, ModeParameter b) => ModeParameter (a, b)

-- | Template Haskell function for generating Type A modes.
typeA :: String -> ModeName -> Type -> DecsQ
typeA = undefined

typeA "ban" 'b' HostMask

-- should generate the following:
{-
 -putBan :: HostMask -> ModeStr
 -putBan = ms . MsAPut 'b'
 -
 -getBan :: ModeStr
 -getBan = ms $ MsAGet 'b'
 -
 -delBan :: HostMask -> ModeStr
 -delBan = ms . MsADel 'b'
 -
 -}
putException :: HostMask -> ModeStr
putException = ms . MsAPut 'e'

getException :: ModeStr
getException = ms $ MsAGet 'e'

delException :: HostMask -> ModeStr
delException = ms . MsADel 'e'

putModerated :: ModeStr
putModerated = ms $ MsDPut 'm'

delModerated :: ModeStr
delModerated = ms $ MsDDel 'm'

putSecret :: ModeStr
putSecret = ms $ MsDPut 's'

delSecret :: ModeStr
delSecret = ms $ MsDDel 's'

putProtected :: ModeStr
putProtected = ms $ MsDPut 't'

delProtected :: ModeStr
delProtected = ms $ MsDDel 't'

putKey :: Text -> ModeStr
putKey = ms . MsBPut 'k'

delKey :: Text -> ModeStr
delKey = ms . MsBDel 'k'

putClientLimit :: Int -> ModeStr
putClientLimit = ms . MsCPut 'l'

delClientLimit :: ModeStr
delClientLimit = ms $ MsCDel 'l'

{-
 -test :: ModeStr
 -test = putBan HostMask <> delKey "foobar" <> putModerated
 -
 -test' :: ModeStr
 -test' = mconcat [ putBan HostMask, delKey "foobar", putModerated ]
 -}
