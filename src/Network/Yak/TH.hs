{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Yak.TH
(
    makeMsgLenses
)
where

import Control.Lens
import Data.Char
import Control.Monad
import Language.Haskell.TH

listParams :: Name -> Q [Type]
listParams name = do
    (TyConI (TySynD _ _ (AppT _ l))) <- reify name
    return $ go l

    where go :: Type -> [Type]
          go (SigT PromotedNilT _) = []
          go (SigT (AppT (AppT PromotedConsT x) xs) _) = x : go xs
          go x = error $ "unexpected type: " ++ pprint x ++ "(" ++ show x ++ ")"

lensName :: Name -> String -> Name
lensName base field =
    let base' = nameBase base
     in mkName $ over _head toLower base' ++ over _head toUpper field

-- | Function to build lenses for type synonyms of 'Msg'.
--
-- > makeMsgLenses ''Pass ["password"]
--
-- The resulting lens will be named according to the synonym and the given field
-- names. Note that no fields can be skipped!
makeMsgLenses :: Name -> [String] -> DecsQ
makeMsgLenses name ss = do
    ps <- zip3 ss [0..] <$> listParams name

    xs <- forM ps $ \(n,k,t) -> do
        let ty = [t|Lens' $(conT name) $(return t)|]
        signature <- sigD (lensName name n) ty
        impl <- [d|$(return . VarP $ lensName name n) = params . $(field k)|]
        return $ signature : impl

    return $ concat xs

field :: Int -> Q Exp
field 0 = [|phead|]
field n = [|ptail . $(field (n - 1))|]
