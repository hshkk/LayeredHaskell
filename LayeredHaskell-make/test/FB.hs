-- fromList [LHsSig "suc" (LHsTyFn (LHsTyCon "Int" (fromList [])) (LHsTyCon "Int" (fromList []))),LHsEqn "suc" (fromList [LHsPVar "n"]) (LHsBin LHsAdd (LHsVar "n") (LHsInt 1)),LHsSig "pred" (LHsTyFn (LHsTyCon "Int" (fromList [])) (LHsTyCon "Int" (fromList []))),LHsEqn "pred" (fromList [LHsPVar "n"]) (LHsBin LHsSub (LHsVar "n") (LHsInt 1)),LHsSig "fac" (LHsTyFn (LHsTyCon "Int" (fromList [])) (LHsTyCon "Int" (fromList []))),LHsEqn "fac" (fromList [LHsPInt 0]) (LHsInt 1),LHsEqn "fac" (fromList [LHsPVar "n"]) (LHsBin LHsMul (LHsVar "n") (LHsApp (LHsVar "fac") (LHsBin LHsSub (LHsVar "n") (LHsInt 1)))),LHsSig "xor" (LHsTyFn (LHsTyCon "Bool" (fromList [])) (LHsTyFn (LHsTyCon "Bool" (fromList [])) (LHsTyCon "Bool" (fromList [])))),LHsEqn "xor" (fromList [LHsPVar "a",LHsPVar "b"]) (LHsBin LHsOr (LHsBin LHsAnd (LHsVar "a") (LHsApp (LHsVar "not") (LHsVar "b"))) (LHsBin LHsAnd (LHsApp (LHsVar "not") (LHsVar "a")) (LHsVar "b"))),LHsSig "xor'" (LHsTyFn (LHsTyCon "Bool" (fromList [])) (LHsTyFn (LHsTyCon "Bool" (fromList [])) (LHsTyCon "Bool" (fromList [])))),LHsEqn "xor'" (fromList [LHsPVar "a",LHsPVar "b"]) (LHsBin LHsAnd (LHsBin LHsOr (LHsVar "a") (LHsVar "b")) (LHsApp (LHsVar "not") (LHsBin LHsAnd (LHsVar "a") (LHsVar "b"))))]

-- Basic function definitions on integers
suc :: Int -> Int
suc n = n + 1

pred :: Int -> Int
pred n = n - 1

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- Basic function definitions on Booleans
xor :: Bool -> Bool -> Bool
xor a b = a && not b || not a && b

xor' :: Bool -> Bool -> Bool
xor' a b = (a || b) && not (a && b)