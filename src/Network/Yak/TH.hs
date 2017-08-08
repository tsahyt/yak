module Network.Yak.TH
(
)
where

import Language.Haskell.TH

listParams :: Name -> Q [Type]
listParams name = do
    (TyConI (TySynD _ _ (AppT _ l))) <- reify name
    return $ go l

    where go :: Type -> [Type]
          go (SigT PromotedNilT _) = []
          go (SigT (AppT (AppT PromotedConsT x) xs) _) = x : go xs
          go x = error $ "unexpected type: " ++ pprint x ++ "(" ++ show x ++ ")"
