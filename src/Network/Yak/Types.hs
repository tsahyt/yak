{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Network.Yak.Types
(
    Raw(..),
    Render(..),
    PList(..),
    params
)
where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)

import qualified Data.Text.Encoding as E

-- | Proxy type for holding IRC messages
data Raw command params = Raw
    { rawPrefix  :: Maybe ByteString
    , rawParams  :: PList params }

-- | Class for any kind of IRC parameter that can be rendered to 'ByteString'
class Render a where
    render :: a -> ByteString

-- | Text is encoded as UTF-8
instance Render Text where
    render = E.encodeUtf8

-- | Lists are comma separated
instance Render a => Render [a] where
    render = mconcat . intersperse "," . map render

instance Render a => Render (NonEmpty a) where
    render (x :| []) = render x
    render (x :| xs) = render (x : xs)

-- | Heterogenous list of parameters. Every element must be renderable!
data PList a where
    PNil  :: PList '[]
    PCons :: forall x xs. Render x => x -> PList xs -> PList (x ': xs)

-- | Transform parameter list into list of 'ByteString's by rendering each
-- element.
params :: PList xs -> [ByteString]
params PNil = []
params (PCons x xs) = render x : params xs
