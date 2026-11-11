-- fromList [LHsSig "x" (LHsTyCon "Int" (fromList [])),LHsEqn "x" (fromList []) (LHsBin LHsMul (LHsInt 5) (LHsInt 3)),LHsSig "f" (LHsTyCon "Int" (fromList [])),LHsEqn "f" (fromList []) (LHsBin LHsAdd (LHsVar "x") (LHsInt 1)),LHsSig "g" (LHsTyCon "Bool" (fromList [])),LHsEqn "g" (fromList []) (LHsLet (fromList [LHsEqn "h" (fromList []) (LHsBin LHsAnd (LHsCon "True") (LHsCon "False"))]) (LHsBin LHsOr (LHsVar "h") (LHsVar "h")))]

-- Binary operation between integers
x :: Int
x = 5 * 3

-- Binary operation between variables and integers
f :: Int
f = x + 1

-- Let expressions and binary operation between Booleans and variables, respectively
g :: Bool
g = let h = True && False in h || h