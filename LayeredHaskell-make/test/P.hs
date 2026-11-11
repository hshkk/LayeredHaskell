-- fromList [LHsData "Maybe" (fromList ["a"]) (fromList [("Nothing",fromList []),("Just",fromList [LHsTyVar "a"])]),LHsData "Pair" (fromList ["a","b"]) (fromList [("Pair",fromList [LHsTyVar "a",LHsTyVar "b"])]),LHsData "List" (fromList ["a"]) (fromList [("Nil",fromList []),("Cons",fromList [LHsTyVar "a",LHsTyCon "List" (fromList [LHsTyVar "a"])])]),LHsSig "x" (LHsTyCon "Pair" (fromList [LHsTyCon "List" (fromList [LHsTyVar "a"]),LHsTyCon "Maybe" (fromList [LHsTyVar "a"])])),LHsEqn "x" (fromList []) (LHsApp (LHsApp (LHsCon "Pair") (LHsCon "Nil")) (LHsCon "Nothing")),LHsSig "y" (LHsTyCon "Pair" (fromList [LHsTyCon "Pair" (fromList [LHsTyCon "List" (fromList [LHsTyVar "a"]),LHsTyCon "Maybe" (fromList [LHsTyVar "b"])]),LHsTyCon "Pair" (fromList [LHsTyCon "Maybe" (fromList [LHsTyVar "c"]),LHsTyCon "List" (fromList [LHsTyVar "d"])])])),LHsEqn "y" (fromList []) (LHsLet (fromList [LHsEqn "first" (fromList []) (LHsApp (LHsApp (LHsCon "Pair") (LHsCon "Nil")) (LHsCon "Nothing")),LHsEqn "second" (fromList []) (LHsApp (LHsApp (LHsCon "Pair") (LHsCon "Nothing")) (LHsCon "Nil"))]) (LHsApp (LHsApp (LHsCon "Pair") (LHsVar "first")) (LHsVar "second")))]

-- P depends on D, so we can define datatypes at will
data Maybe a = Nothing | Just a
data Pair a b = Pair a b
data List a = Nil | Cons a (List a)

-- Since P need not be enabled alongside F, we can only construct values
x :: Pair (List a) (Maybe a)
x = Pair Nil Nothing

y :: Pair (Pair (List a) (Maybe b)) (Pair (Maybe c) (List d))
y = let first  = Pair Nil Nothing
        second = Pair Nothing Nil
    in Pair first second