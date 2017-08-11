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
    emitSome,
    fetch,
    fetch'
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
emit Msg{..} = fromMaybe "" 
               (flip mappend " " . mappend ":" . rpfx <$> msgPrefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> render msgParams
            <> "\n"

    where rpfx (PrefixServer x) = render x
          rpfx (PrefixUser h) = render h

-- | Encode existentially quantified message.
emitSome :: SomeMsg -> ByteString
emitSome (SomeMsg r) = emit r

-- | Decode an IRC message from a 'ByteString' into a 'Msg'. This function is
-- return type polymorphic and will pick a parser that fits the requested type,
-- which is determined either by type inference or can be picked by explicit
-- type annotation.
fetch :: forall c p. (Parameter (PList p), KnownSymbol c) 
      => ByteString -> Maybe (Msg c p)
fetch = either (const Nothing) Just . parseOnly fetch'

-- | Like 'fetch' but offers the underlying attoparsec Parser. This can be used
-- for e.g. the construction of ad-hoc sum types catching multiple message
-- types.
fetch' :: forall c p. (Parameter (PList p), KnownSymbol c) => Parser (Msg c p)
fetch' = Msg 
     <$> optional (char ':' *> pfx) 
     <*> (skipSpace *> cmd *> skipSpace *> seize)
    where cmd = string . B.pack . symbolVal $ Proxy @c
          pfx = (PrefixUser <$> seize) <|> (PrefixServer <$> srv)
          srv = decodeUtf8 <$> takeTill isSpace
