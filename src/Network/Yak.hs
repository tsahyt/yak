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

import Control.Applicative
import Control.Lens
import Network.Yak.Types
import Data.Maybe
import GHC.TypeLits
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Monoid
import Data.Proxy

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.List.NonEmpty as N
import qualified Data.Attoparsec.ByteString.Char8 as A

-- | Encode an IRC message to a 'ByteString', ready for the network
emit :: forall c p. (Parameter (PList p), KnownSymbol c) 
     => Raw c p -> ByteString
emit Raw{..} = fromMaybe "" (mappend ":" <$> _prefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> render _params
            <> "\n"

-- | A Join command must have at least one channel to join
type Join = Raw "JOIN" '[NonEmpty Text]

join :: NonEmpty Text -> Join
join xs = Raw Nothing (PCons xs PNil)

class HasChannels s where
    channels :: Lens' s (NonEmpty Text)

instance HasChannels Join where
    channels = lens (view (params . _1)) (flip (set (params . _1)))

-- | A Part command must have at least one channel to part from
type Part = Raw "PART" '[NonEmpty Text]

part :: NonEmpty Text -> Part
part xs = Raw Nothing (PCons xs PNil)

instance HasChannels Part where
    channels = lens (view (params . _1)) (flip (set (params . _1)))

-- | Quitting needs a quit message
type Quit = Raw "QUIT" '[Text]

quit :: Text -> Quit
quit x = Raw Nothing (PCons x PNil)

class HasMessage s where
    message :: Lens' s Text

instance HasMessage Quit where
    message = lens (view (params . _1)) (flip (set (params . _1)))

-- | A PrivMsg has two parameters, one is the non-empty lists of receivers, the
-- other is the message
type PrivMsg = Raw "PRIVMSG" '[NonEmpty Text, Text]

instance HasMessage PrivMsg where
    message = lens (view (params . _2)) (flip (set (params . _2)))

fetch :: forall c p. (Parameter (PList p), KnownSymbol c) 
      => ByteString -> Maybe (Raw c p)
fetch = A.maybeResult . A.parse go
    where go  = Raw <$> pure Nothing <*> (cmd *> A.space *> seize)
          cmd = A.string . B.pack . symbolVal $ Proxy @c
