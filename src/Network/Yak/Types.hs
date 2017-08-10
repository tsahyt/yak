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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

module Network.Yak.Types
(
    Prefix(..),
    Host(..),
    Msg(..),
    build,
    buildPrefix,
    vacant,
    (<:>),
    castMsg,
    SomeMsg(..),
    Unused(..),
    Flag(..),
    SList(..),
    Parameter(..),
    PList(..),
    phead,
    ptail,
    params,
    prefix,
    Channel(..),
    Message(..),
    Nickname,
    Username,
    Hostname,
    Mask,
    ModeSign(..),
    ModeString,
    modes,
    unmodes,
    ModeString'(..),
)
where

import Control.Applicative
import Control.Lens
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Text (Text)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Word (Word)
import Data.Proxy
import Data.Kind (Type)
import Data.Text.Encoding
import Data.String
import GHC.TypeLits
import GHC.Exts (IsList (..))

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

data Prefix 
    = PrefixServer Text
    | PrefixUser Host
    deriving (Eq, Show, Read, Ord)

type Username = Text
type Nickname = Text
type Mask = Text
type Hostname = Text

data Host = Host
    { hostNick :: Nickname
    , hostUser :: Maybe Username
    , hostHost :: Maybe Hostname
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

-- | Text is encoded as UTF-8. Pieces of Text are space normally separated. Text
-- parsing enforces the text to be non-empty, and commas are disallowed.
instance Parameter Text where
    render = encodeUtf8
    seize  = decodeUtf8 <$> do
                 x <- takeTill (\x -> isSpace x || x == ',')
                 if B.null x then empty else pure x

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
    seize = let x = B.pack $ symbolVal (Proxy @a) in Unused <$ string x

-- | Type for expressing possibly missing flags.
data Flag a = Set | Unset deriving (Show, Eq, Ord, Read)

instance KnownSymbol a => Parameter (Flag (a :: Symbol)) where
    render Set   = B.pack . symbolVal $ Proxy @a
    render Unset = ""
    seize = let x = B.pack $ symbolVal (Proxy @a) 
             in fromMaybe Unset <$> optional (Set <$ string x)

instance Parameter Word where
    render = render . T.pack . show
    seize  = decimal

-- | Only positive
instance Parameter Int where
    render = render . T.pack . show
    seize  = decimal

instance Parameter Char where
    render = render . T.singleton
    seize = satisfy (not . isSpace)

instance (Parameter a, Parameter b) => Parameter (a, b) where
    render (a,b) = render a <> " " <> render b
    seize = (,) <$> seize <*> (skipSpace *> seize)

instance (Parameter a, Parameter b) => Parameter (Either a b) where
    render (Left x)  = render x
    render (Right x) = render x

    seize = (Left <$> seize) <|> (Right <$> seize)

-- | Space separated lists. Use with caution, since spaces are also the
-- separator for 'PList'!
newtype SList a = SList { getSList :: [a] }
    deriving (Eq, Show, Ord, Read, Functor, Applicative, Monad, Foldable, 
              Traversable, Monoid, Alternative)

instance Wrapped (SList a) where
    type Unwrapped (SList a) = [a]
    _Wrapped' = iso getSList SList

instance (t ~ SList b) => Rewrapped (SList a) t

-- | Syntactic sugar for construction
instance IsList (SList a) where
    type Item (SList a) = a
    fromList = SList
    toList = getSList

instance Parameter a => Parameter (SList a) where
    render = mconcat . intersperse " " . map render . getSList
    seize  = SList <$> sepBy seize space

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
      => Field1 (PList (x ': xs)) 
                (PList (x' ': xs)) x x' 
      where
    _1 = phead

instance (Parameter x, Parameter x') 
      => Field2 (PList (a ': x ': xs)) 
                (PList (a ': x' ': xs)) x x'
      where
    _2 = ptail . phead

instance (Parameter x, Parameter x') 
      => Field3 (PList (a ': b ': x ': xs)) 
                (PList (a ': b ': x' ': xs)) x x' 
      where
    _3 = ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field4 (PList (a ': b ': c ': x ': xs)) 
                (PList (a ': b ': c ': x' ': xs)) x x'
      where
    _4 = ptail . ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field5 (PList (a ': b ': c ': d ': x ': xs)) 
                (PList (a ': b ': c ': d ': x' ': xs)) x x'
      where
    _5 = ptail . ptail . ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field6 (PList (a ': b ': c ': d ': e ': x ': xs)) 
                (PList (a ': b ': c ': d ': e ': x' ': xs)) x x'
      where
    _6 = ptail . ptail . ptail . ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field7 (PList (a ': b ': c ': d ': e ': f ': x ': xs)) 
                (PList (a ': b ': c ': d ': e ': f ': x' ': xs)) x x'
      where
    _7 = ptail . ptail . ptail . ptail . ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field8 (PList (a ': b ': c ': d ': e ': f ': g ': x ': xs)) 
                (PList (a ': b ': c ': d ': e ': f ': g ': x' ': xs)) x x'
      where
    _8 = ptail . ptail . ptail . ptail . ptail . ptail . ptail . phead

instance (Parameter x, Parameter x') 
      => Field9 (PList (a ': b ': c ': d ': e ': f ': g ': h ': x ': xs)) 
                (PList (a ': b ': c ': d ': e ': f ': g ': h ': x' ': xs)) x x'
      where
    _9 = ptail . ptail . ptail . ptail . ptail . ptail . ptail . ptail . phead

makeLensesFor [("msgParams", "params"), ("msgPrefix", "prefix")] ''Msg

-- The following bit of code for Build and Reverse is adapted from an example
-- for building HLists with variadic functions, courtesy of lyxia on #haskell.
type family Reverse' (acc :: [Type]) (xs :: [Type]) :: [Type]
type instance Reverse' acc '[] = acc
type instance Reverse' acc (x ': xs) = Reverse' (x ': acc) xs

class HReverse acc xs where
    hReverse' :: PList acc -> PList xs -> PList (Reverse' acc xs)

instance HReverse acc '[] where
    hReverse' = const

instance HReverse (x ': acc) xs => HReverse acc (x ': xs) where
    hReverse' acc (PCons x xs) = hReverse' (PCons x acc) xs

class Build (xs :: [Type]) f where
    build' :: Msg c xs -> f

instance (ys ~ Reverse' '[] xs, HReverse '[] xs) => Build xs (Msg c ys) where
    build' (Msg a b) = Msg a (hReverse' PNil b)

instance Build (x ': xs) g => Build xs (x -> g) where
    build' xs x = build' (x <:> xs)

-- | Generalized constructor function for the creation of 'Msg' values. The
-- types here may seem opaque, but essentially this is a variadic type-safe
-- constructor.
--
-- > build "hunter2" :: Pass
-- > build [Channel "#haskell"] (Message "hello world!") :: PrivMsg
--
-- The type annotations may or may not be necessary, depending on the
-- information available to the compiler at the use site.
build :: Build '[] f => f
build = build' vacant

-- | Like 'build' but takes a 'Prefix' that will be added to the message.
buildPrefix :: Build '[] f => Prefix -> f
buildPrefix p = build' (vacant & prefix .~ Just p)

newtype Channel = Channel { getChannel :: Text }
    deriving (Eq, Show, Ord, Read, IsString)

makeWrapped ''Channel

instance Parameter Channel where
    render = render . getChannel
    seize  = do
        mark <- satisfy (inClass "&#")
        name <- many1 $ satisfy (notInClass " \7,\n")
        pure . Channel . T.pack $ mark : name

newtype Message = Message { getMessage :: Text }
    deriving (Eq, Show, Ord, Read, IsString)

makeWrapped ''Message

instance Parameter Message where
    render = render . T.cons ':' . getMessage
    seize  = Message . decodeUtf8 <$> (char ':' *> takeTill (inClass "\n"))

data ModeSign = AddMode | RemoveMode
    deriving (Eq, Show, Ord, Read)

modeSignChar :: ModeSign -> Char
modeSignChar AddMode = '+'
modeSignChar RemoveMode = '-'

instance Parameter ModeSign where
    render = B.singleton . modeSignChar
    seize = (AddMode <$ char '+') <|> (RemoveMode <$ char '-')

-- | Datatype for precisely representing valid mode strings, i.e. strings
-- starting with @+@ or @-@, continuing with some characters where all @+@/@-@
-- have special position.
type ModeString = ModeString' ModeSign

-- | Workhorse for 'ModeString'. The type parameter determines the type in the
-- head and can be on of 'ModeSign' or 'Char', as there are no other valid
-- constructors for non-empty mode strings.
data ModeString' head where
    MSNil  :: ModeString' empty
    MSSign :: ModeSign -> ModeString' Char -> ModeString' ModeSign
    MSChar :: Char -> ModeString' j -> ModeString' Char

instance Show (ModeString' head) where
    show MSNil = ""
    show (MSSign x xs) = modeSignChar x : show xs
    show (MSChar x xs) = x : show xs

instance Eq (ModeString' head) where
    a == b = show a == show b

instance Parameter (ModeString' ModeSign) where
    render = B.pack . show
    seize  = do
        SomeModeString x <- parseSomeModeString
        case x of
            MSSign _ _ -> pure x
            _ -> empty

-- | Partial for malformed literals!
instance IsString (ModeString' ModeSign) where
    fromString = either (error "Invalid ModeString Literal") id 
               . parseOnly seize . B.pack

-- | Parse a 'ModeString' into a list of mode characters with their changes.
modes :: ModeString -> NonEmpty (ModeSign, [Char])
modes ms = 
    let ms' = SomeModeString ms 
     in fromList . reverse $ zip (signs [] ms') (map reverse $ chars [] ms')

    where chars :: [[Char]] -> SomeModeString -> [[Char]]
          chars acc (SomeModeString MSNil) = acc
          chars acc (SomeModeString (MSSign _ xs)) = 
              chars ([] : acc) (SomeModeString xs)
          chars acc (SomeModeString (MSChar x xs)) =
              case acc of
                  (a:as) -> chars ((x : a) : as) (SomeModeString xs)
                  [] -> chars [[x]] (SomeModeString xs)

          signs :: [ModeSign] -> SomeModeString -> [ModeSign]
          signs acc (SomeModeString MSNil) = acc
          signs acc (SomeModeString (MSSign x xs)) =
              signs (x : acc) (SomeModeString xs)
          signs acc (SomeModeString (MSChar _ xs)) =
              signs acc (SomeModeString xs)

unmodes :: NonEmpty (ModeSign, [Char]) -> ModeString
unmodes = fromString . concat . toList . fmap go
    where go (x, xs) = modeSignChar x : xs

data SomeModeString where
    SomeModeString :: forall head. ModeString' head -> SomeModeString

instance Show SomeModeString where
    show (SomeModeString x) = show x

parseSomeModeString :: Parser SomeModeString
parseSomeModeString = 
    choice [ SomeModeString <$> msign
           , SomeModeString <$> mchar 
           , pure $ SomeModeString MSNil ]
    where msign :: Parser (ModeString' ModeSign)
          msign = do
              s <- (AddMode <$ char '+') <|> (RemoveMode <$ char '-')
              x <- parseSomeModeString
              case x of
                  SomeModeString c@(MSChar{}) -> pure $ MSSign s c
                  _ -> empty

          mchar :: Parser (ModeString' Char)
          mchar = do
              c <- satisfy isAlpha_ascii
              SomeModeString x <- parseSomeModeString
              pure $ MSChar c x
