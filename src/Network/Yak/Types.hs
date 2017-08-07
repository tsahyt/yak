{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Yak.Types
(
    Prefix(..),
    Host(..),
    Msg(..),
    vacant,
    (<:>),
    castMsg,
    SomeMsg(..),
    Unused(..),
    Parameter(..),
    PList(..),
    phead,
    ptail,
    params,
    prefix
)
where

import Control.Applicative
import Control.Lens
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Text (Text)
import Data.Monoid
import Data.Word (Word)
import Data.Proxy
import Data.Kind (Type)
import GHC.TypeLits

import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

data Prefix 
    = PrefixServer Text
    | PrefixUser Host
    deriving (Eq, Show, Read, Ord)

data Host = Host
    { hostNick :: Text
    , hostUser :: Maybe Text
    , hostHost :: Maybe Text
    }
    deriving (Eq, Show, Read, Ord)

-- | Proxy type for holding IRC messages
data Msg command params = Msg
    { msgPrefix  :: Maybe Prefix
    , msgParams  :: PList params }

instance Eq (PList params) => Eq (Msg command params) where
    Msg a b == Msg c d = a == c && b == d

instance Show (PList params) => Show (Msg command params) where
    show (Msg a b) = "Msg (" ++ show a ++ ") (" ++ show b ++ ")"

vacant :: Msg c '[]
vacant = Msg Nothing PNil

(<:>) :: x -> Msg c xs -> Msg c (x ': xs)
x <:> (Msg p xs) = Msg p (PCons x xs)
infixr 5 <:>

-- | Safely cast one message to another. Equality of parameter lists is
-- statically enforced.
castMsg :: (KnownSymbol c1, KnownSymbol c2) => Msg c1 p -> Msg c2 p
castMsg (Msg c p) = Msg c p

data SomeMsg where
    SomeMsg :: (KnownSymbol c, Parameter (PList p)) => Msg c p -> SomeMsg

-- | Class for any kind of IRC parameter that can be rendered to 'ByteString'
-- and read from a 'ByteString'
class Parameter a where
    render :: a -> ByteString
    seize  :: Parser a

-- | Text is encoded as UTF-8. Pieces of Text are space normally separated.
instance Parameter Text where
    render = E.encodeUtf8
    seize  = E.decodeUtf8 <$> takeTill isSpace

-- | Lists are comma separated
instance Parameter a => Parameter [a] where
    render = mconcat . intersperse "," . map render
    seize  = sepBy seize (char ',')

-- | Like lists but non-empty
instance Parameter a => Parameter (NonEmpty a) where
    render (x :| []) = render x
    render (x :| xs) = render (x : xs)
    
    seize = fromList <$> sepBy1 seize (char ',')

-- | A Maybe parameter can be totally absent
instance Parameter a => Parameter (Maybe a) where
    render (Just x) = render x
    render Nothing  = ""

    seize = optional seize

instance (Parameter x, Parameter (PList xs)) 
      => Parameter (PList (x ': xs)) where
    render (PCons x PNil) = render x
    render (PCons x xs) = render x <> " " <> render xs

    seize = PCons <$> seize <*> (skipSpace *> seize)

instance Parameter (PList '[]) where
    render _ = ""
    seize  = pure PNil

-- | Proxy type for inserting special syntax
data Unused a = Unused deriving (Show, Eq, Ord, Read)

instance KnownSymbol a => Parameter (Unused (a :: Symbol)) where
    render _ = B.pack . symbolVal $ Proxy @a
    seize = pure Unused

instance Parameter Word where
    render = render . T.pack . show
    seize  = decimal

-- | Only positive
instance Parameter Int where
    render = render . T.pack . show
    seize  = decimal

-- | Heterogeneous list of parameters.
data PList a where
    PNil  :: PList '[]
    PCons :: forall x xs. x -> PList xs -> PList (x ': xs)

instance Eq (PList '[]) where
    PNil == PNil = True

instance (Eq x, Eq (PList xs)) => Eq (PList (x ': xs)) where
    (PCons x xs) == (PCons y ys) = x == y && xs == ys

instance Show (PList '[]) where
    show PNil = "PNil"

instance (Show x, Show (PList xs)) => Show (PList (x ': xs)) where
    show (PCons x xs) = show x ++ " `PCons` " ++ show xs

phead :: Parameter x' => Lens (PList (x ': xs)) (PList (x' ': xs)) x x'
phead = lens (\(PCons x _) -> x) (\(PCons _ xs) x -> PCons x xs)

ptail :: Lens (PList (x ': xs)) (PList (x ': xs')) (PList xs) (PList xs')
ptail = lens (\(PCons _ xs) -> xs) (\(PCons x _) xs -> PCons x xs)

instance (Parameter x, Parameter x') 
      => Field1 (PList (x ': xs)) (PList (x' ': xs)) x x' where
    _1 = lens (view phead) (flip (set phead))

instance (Parameter x, Parameter x') 
      => Field2 (PList (a ': x ': xs)) (PList (a ': x' ': xs)) x x' where
    _2 = lens (view (ptail . phead)) (flip (set (ptail . phead)))

instance (Parameter x, Parameter x') 
      => Field3 (PList (a ': b ': x ': xs)) (PList (a ': b ': x' ': xs)) x x' 
      where
    _3 = lens (view (ptail . ptail . phead)) 
              (flip (set (ptail . ptail . phead)))

instance (Parameter x, Parameter x') 
      => Field4 (PList (a ': b ': c ': x ': xs)) 
                (PList (a ': b ': c ': x' ': xs)) x x'
      where
    _4 = lens (view (ptail . ptail . ptail . phead)) 
              (flip (set (ptail . ptail . ptail . phead)))

class Build (xs :: [Type]) f where
    build' :: PList xs -> f

instance (xs ~ ys) => Build xs (PList ys) where
    build' = id

instance Build (x ': xs) g => Build xs (x -> g) where
    build' xs x = build' (PCons x xs)

build :: Build '[] f => f
build = build' PNil

makeFields ''Msg
