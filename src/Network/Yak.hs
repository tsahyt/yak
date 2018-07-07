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
    ( emit
    , emitSome
    , Fetch
    , fetch
    , fetch'
    -- * Messages
    , T.build
    , T.buildPrefix
    , T.vacant
    , T.castMsg
    , (T.<:>)
    -- * Types
    , module Network.Yak.Types
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text.Encoding
import GHC.TypeLits
import Network.Yak.Types hiding (build, buildPrefix, vacant, castMsg, (<:>))

import qualified Network.Yak.Types as T
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

class Fetch a where
    -- | Decode an IRC message from a 'ByteString' into a 'Msg' or a coproduct
    -- thereof. This function is return type polymorphic and will pick a parser
    -- that fits the requested type, which is determined either by type
    -- inference or can be picked by explicit type annotation.
    fetch :: ByteString -> Maybe a

instance (Parameter (PList p), KnownSymbol c) => Fetch (Msg c p) where
    fetch = either (const Nothing) Just . parseOnly fetch'

instance (Fetch a, Fetch b) => Fetch (Either a b) where
    fetch x =
        case fetch @a x of
            Just l -> Just (Left l)
            Nothing -> Right <$> fetch x

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
