{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}
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
emit :: forall c p. KnownSymbol c => Raw c p -> ByteString
emit Raw{..} = fromMaybe "" (mappend ":" <$> rawPrefix)
            <> (B.pack $ symbolVal (Proxy @c))
            <> " "
            <> B.unwords (params rawParams)
            <> "\n"

-- | A Join command must have at least one channel to join
type Join = Raw "JOIN" '[NonEmpty Text]

join :: NonEmpty Text -> Join
join xs = Raw Nothing (PCons xs PNil)

joinChannels :: Lens' Join (NonEmpty Text)
joinChannels = lens (phead . rawParams) go
    where go x t = x { rawParams = PCons t PNil }

parseJoin :: A.Parser Join
parseJoin = do
    _  <- A.string "JOIN "
    cs <- A.sepBy1 (A.many1 $ A.satisfy A.isAlpha_ascii) (A.char ',')
    pure $ join (N.fromList $ map T.pack cs)

-- | A Part command must have at least one channel to part from
type Part = Raw "PART" '[NonEmpty Text]

partChannels :: Lens' Part (NonEmpty Text)
partChannels = lens (phead . rawParams) go
    where go x t = x { rawParams = PCons t PNil }

part :: NonEmpty Text -> Part
part xs = Raw Nothing (PCons xs PNil)

parsePart :: A.Parser Part
parsePart = do
    _  <- A.string "PART "
    cs <- A.sepBy1 (A.many1 $ A.satisfy A.isAlpha_ascii) (A.char ',')
    pure $ part (N.fromList $ map T.pack cs)

-- | Quitting needs a quit message
type Quit = Raw "QUIT" '[Text]

quitMessage :: Lens' Quit Text
quitMessage = lens (phead . rawParams) go
    where go x t = x { rawParams = PCons t PNil }

quit :: Text -> Quit
quit x = Raw Nothing (PCons x PNil)

parseQuit :: A.Parser Quit
parseQuit = do
    _  <- A.string "QUIT "
    cs <- T.pack <$> many (A.satisfy A.isAlpha_ascii)
    pure $ quit cs

-- | A PrivMsg has two parameters, one is the non-empty lists of receivers, the
-- other is the message
type PrivMsg = Raw "PRIVMSG" '[NonEmpty Text, Text]

data CoreMsg
    = MJoin Join
    | MPart Part
    | MQuit Quit

fetch :: ByteString -> Maybe CoreMsg
fetch = A.maybeResult . A.parse go
    where go = MJoin <$> parseJoin
           <|> MPart <$> parsePart
           <|> MQuit <$> parseQuit

foo = case fetch "QUIT foo\n" of
    Nothing -> return ()
    Just (MJoin r) -> putStrLn "join!"
    Just (MQuit r) -> putStrLn "quit!"
