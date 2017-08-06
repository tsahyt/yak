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
module Network.Yak
(
    emit,
    fetch
)
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Text.Encoding
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import Network.Yak.Types

import qualified Data.ByteString.Char8 as B

-- | Encode an IRC message to a 'ByteString', ready for the network
emit :: forall c p. (Parameter (PList p), KnownSymbol c) 
     => Msg c p -> ByteString
emit Msg{..} = fromMaybe "" undefined -- (mappend ":" <$> msgPrefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> render msgParams
            <> "\n"

fetch :: forall c p. (Parameter (PList p), KnownSymbol c) 
      => ByteString -> Maybe (Msg c p)
fetch = maybeResult . parse go
    where go  = Msg <$> optional pfx <*> (cmd *> skipSpace *> seize)
          cmd = string . B.pack . symbolVal $ Proxy @c
          pfx = (PrefixUser <$> hst) <|> (PrefixServer <$> srv)

          hst = Host
            <$> (decodeUtf8 <$> takeTill (inClass "!@ "))
            <*> optional (decodeUtf8 <$> (char '!' *> takeTill (inClass "@ ")))
            <*> optional (decodeUtf8 <$> (char '@' *> takeTill isSpace))

          srv = decodeUtf8 <$> takeTill isSpace
