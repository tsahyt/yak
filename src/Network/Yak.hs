{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Yak where

import Network.Yak.Types
import Data.Maybe
import GHC.TypeLits
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Proxy

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

-- | Encode an IRC message to a 'ByteString', ready for the network
emit :: forall c p. (Parameter (PList p), KnownSymbol c) 
     => Msg c p -> ByteString
emit Msg{..} = fromMaybe "" (mappend ":" <$> _prefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> render _params
            <> "\n"

fetch :: forall c p. (Parameter (PList p), KnownSymbol c) 
      => ByteString -> Maybe (Msg c p)
fetch = A.maybeResult . A.parse go
    where go  = Msg <$> pure Nothing <*> (cmd *> A.space *> seize)
          cmd = A.string . B.pack . symbolVal $ Proxy @c
