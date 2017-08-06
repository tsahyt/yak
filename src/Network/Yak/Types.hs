{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Yak.Types
(
    Raw(..),
    Render(..),
    PList(..),
    renderParams,
    phead,
    ptail,
    params,
    prefix
)
where

import Control.Lens
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import GHC.TypeLits
import Data.Kind

import qualified Data.Text.Encoding as E

-- | Proxy type for holding IRC messages
data Raw command params = Raw
    { _prefix  :: Maybe ByteString
    , _params  :: PList params }

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

instance Render a => Render (Maybe a) where
    render (Just x) = render x
    render Nothing  = ""

-- | Heterogenous list of parameters. Every element must be renderable!
data PList a where
    PNil  :: PList '[]
    PCons :: forall x xs. Render x => x -> PList xs -> PList (x ': xs)

phead :: Render x' => Lens (PList (x ': xs)) (PList (x' ': xs)) x x'
phead = lens (\(PCons x _) -> x) (\(PCons _ xs) x -> PCons x xs)

ptail :: Lens (PList (x ': xs)) (PList (x ': xs')) (PList xs) (PList xs')
ptail = lens (\(PCons _ xs) -> xs) (\(PCons x _) xs -> PCons x xs)

instance (Render x, Render x') => Field1 (PList (x ': xs)) (PList (x' ': xs)) x x' where
    _1 = lens (view phead) (flip (set phead))

instance (Render x, Render x') => Field2 (PList (a ': x ': xs)) (PList (a ': x' ': xs)) x x' where
    _2 = lens (view (ptail . phead)) (flip (set (ptail . phead)))

instance (Render x, Render x') => Field3 (PList (a ': b ': x ': xs)) (PList (a ': b ': x' ': xs)) x x' where
    _3 = lens (view (ptail . ptail . phead)) (flip (set (ptail . ptail . phead)))

instance (Render x, Render x') => Field4 (PList (a ': b ': c ': x ': xs)) (PList (a ': b ': c ': x' ': xs)) x x' where
    _4 = lens (view (ptail . ptail . ptail . phead)) (flip (set (ptail . ptail . ptail . phead)))

-- | Transform parameter list into list of 'ByteString's by rendering each
-- element.
renderParams :: PList xs -> [ByteString]
renderParams PNil = []
renderParams (PCons x xs) = render x : renderParams xs

makeLenses ''Raw
