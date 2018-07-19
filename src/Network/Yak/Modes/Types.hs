{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Network.Yak.Modes.Types
    ( ModeSet
    , ModeName
    , ModeParameter(..)
    , ModeStr
    , ms
    , typeA
    , typeB
    , typeC
    , typeD
    ) where

import Control.Lens
import Data.Char
import Data.Semigroup
import Data.Text (Text)
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

instance ModeParameter Text
instance ModeParameter Int
instance (ModeParameter a, ModeParameter b) => ModeParameter (a, b)

alternate :: [a] -> [a] -> [a]
alternate as bs = concat $ zipWith (\a b -> [a,b]) as bs

-- | Template Haskell function for generating Type A modes.
typeA :: String -> ModeName -> Name -> DecsQ
typeA name char t = 
    let defs = [d|
                $(pat "put") = ms . MsAPut $(char')
                $(pat "get") = ms $ MsAGet $(char')
                $(pat "del") = ms . MsADel $(char')
                |]
        ts = [ SigD (name' "put") $ AppT (AppT ArrowT (ConT t)) (ConT ''ModeStr)
             , SigD (name' "get") $ ConT ''ModeStr
             , SigD (name' "del") $ AppT (AppT ArrowT (ConT t)) (ConT ''ModeStr)
             ]
     in (alternate ts) <$> defs
  where
    char' = return . LitE . CharL $ char
    name' p = mkName $ p ++ (over _head toUpper name)
    pat = return . VarP . name'

-- | Template Haskell function for generating Type B modes.
typeB :: String -> ModeName -> Name -> DecsQ
typeB name char t = 
    let defs = [d|
                $(pat "put") = ms . MsBPut $(char')
                $(pat "del") = ms . MsBDel $(char')
                |]
        ts = [ SigD (name' "put") $ AppT (AppT ArrowT (ConT t)) (ConT ''ModeStr)
             , SigD (name' "del") $ AppT (AppT ArrowT (ConT t)) (ConT ''ModeStr)
             ]
     in (alternate ts) <$> defs
  where
    char' = return . LitE . CharL $ char
    name' p = mkName $ p ++ (over _head toUpper name)
    pat = return . VarP . name'

-- | Template Haskell function for generating Type C modes.
typeC :: String -> ModeName -> Name -> DecsQ
typeC name char t = 
    let defs = [d|
                $(pat "put") = ms . MsCPut $(char')
                $(pat "del") = ms $ MsCDel $(char')
                |]
        ts = [ SigD (name' "put") $ AppT (AppT ArrowT (ConT t)) (ConT ''ModeStr)
             , SigD (name' "del") $ ConT ''ModeStr
             ]
     in (alternate ts) <$> defs
  where
    char' = return . LitE . CharL $ char
    name' p = mkName $ p ++ (over _head toUpper name)
    pat = return . VarP . name'

-- | Template Haskell function for generating Type D modes.
typeD :: String -> ModeName -> DecsQ
typeD name char = 
    let defs = [d|
                $(pat "put") = ms $ MsDPut $(char')
                $(pat "del") = ms $ MsDDel $(char')
                |]
        ts = [ SigD (name' "put") $ ConT ''ModeStr
             , SigD (name' "del") $ ConT ''ModeStr
             ]
     in (alternate ts) <$> defs
  where
    char' = return . LitE . CharL $ char
    name' p = mkName $ p ++ (over _head toUpper name)
    pat = return . VarP . name'
