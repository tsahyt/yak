{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Yak where

import Network.Yak.Types
import Data.Maybe
import GHC.TypeLits
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Monoid
import Data.Proxy

import qualified Data.ByteString.Char8 as B

-- | Encode an IRC message to a 'ByteString', ready for the network
emit :: forall c p. KnownSymbol c => Raw c p -> ByteString
emit Raw{..} = fromMaybe "" (mappend ":" <$> rawPrefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> B.unwords (params rawParams)
            <> "\n"

-- | A Join command must have at least one channel to join
type Join = Raw "JOIN" '[NonEmpty Text]

-- | A Part command must have at least one channel to part from
type Part = Raw "PART" '[NonEmpty Text]

-- | Quitting needs a quit message
type Quit = Raw "QUIT" '[Text]

-- | A PrivMsg has two parameters, one is the reciever, the other is the message
type PrivMsg = Raw "PRIVMSG" '[Text, Text]
