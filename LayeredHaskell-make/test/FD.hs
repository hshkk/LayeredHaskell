-- fromList [LHsData "Input" (fromList []) (fromList [("Low",fromList []),("High",fromList [])]),LHsData "JoinList" (fromList []) (fromList [("Nil",fromList []),("One",fromList [LHsTyCon "Input" (fromList [])]),("Append",fromList [LHsTyCon "JoinList" (fromList []),LHsTyCon "JoinList" (fromList [])])]),LHsSig "reverse" (LHsTyFn (LHsTyCon "JoinList" (fromList [])) (LHsTyCon "JoinList" (fromList []))),LHsEqn "reverse" (fromList [LHsPCon "Nil" (fromList [])]) (LHsCon "Nil"),LHsEqn "reverse" (fromList [LHsPCon "One" (fromList [LHsPVar "x"])]) (LHsApp (LHsCon "One") (LHsVar "x")),LHsEqn "reverse" (fromList [LHsPCon "Append" (fromList [LHsPVar "xs",LHsPVar "ys"])]) (LHsApp (LHsApp (LHsCon "Append") (LHsApp (LHsVar "reverse") (LHsVar "ys"))) (LHsApp (LHsVar "reverse") (LHsVar "xs"))),LHsSig "duplicate" (LHsTyFn (LHsTyCon "JoinList" (fromList [])) (LHsTyCon "JoinList" (fromList []))),LHsEqn "duplicate" (fromList [LHsPCon "Nil" (fromList [])]) (LHsCon "Nil"),LHsEqn "duplicate" (fromList [LHsPCon "One" (fromList [LHsPVar "x"])]) (LHsApp (LHsApp (LHsCon "Append") (LHsApp (LHsCon "One") (LHsVar "x"))) (LHsApp (LHsCon "One") (LHsVar "x"))),LHsEqn "duplicate" (fromList [LHsPCon "Append" (fromList [LHsPVar "xs",LHsPVar "ys"])]) (LHsApp (LHsApp (LHsCon "Append") (LHsApp (LHsVar "duplicate") (LHsVar "xs"))) (LHsApp (LHsVar "duplicate") (LHsVar "ys"))),LHsSig "invert" (LHsTyFn (LHsTyCon "Bin" (fromList [])) (LHsTyCon "Bin" (fromList []))),LHsEqn "invert" (fromList [LHsPCon "Low" (fromList [])]) (LHsCon "High"),LHsEqn "invert" (fromList [LHsPCon "High" (fromList [])]) (LHsCon "Low"),LHsSig "or" (LHsTyFn (LHsTyCon "Bin" (fromList [])) (LHsTyFn (LHsTyCon "Bin" (fromList [])) (LHsTyCon "Bin" (fromList [])))),LHsEqn "or" (fromList [LHsPCon "Low" (fromList []),LHsPCon "Low" (fromList [])]) (LHsCon "Low"),LHsEqn "or" (fromList [LHsPCon "Low" (fromList []),LHsPCon "High" (fromList [])]) (LHsCon "High"),LHsEqn "or" (fromList [LHsPCon "High" (fromList []),LHsPCon "Low" (fromList [])]) (LHsCon "High"),LHsEqn "or" (fromList [LHsPCon "High" (fromList []),LHsPCon "High" (fromList [])]) (LHsCon "High"),LHsSig "singleton" (LHsTyFn (LHsTyCon "Bin" (fromList [])) (LHsTyCon "JoinList" (fromList []))),LHsEqn "singleton" (fromList [LHsPVar "x"]) (LHsApp (LHsCon "One") (LHsVar "x")),LHsData "MaybeInput" (fromList []) (fromList [("Nothing",fromList []),("Just",fromList [LHsTyCon "Input" (fromList [])])]),LHsSig "first" (LHsTyFn (LHsTyCon "JoinList" (fromList [])) (LHsTyCon "MaybeInput" (fromList []))),LHsEqn "first" (fromList [LHsPCon "Nil" (fromList [])]) (LHsCon "Nothing"),LHsEqn "first" (fromList [LHsPCon "One" (fromList [LHsPVar "x"])]) (LHsApp (LHsCon "Just") (LHsVar "x")),LHsEqn "first" (fromList [LHsPCon "Append" (fromList [LHsPVar "xs",LHsPVar "ys"])]) (LHsCase (LHsApp (LHsVar "first") (LHsVar "xs")) (fromList [(LHsPCon "Nothing" (fromList []),LHsApp (LHsVar "first") (LHsVar "ys")),(LHsPCon "Just" (fromList [LHsPVar "x"]),LHsApp (LHsCon "Just") (LHsVar "x"))])),LHsSig "append" (LHsTyFn (LHsTyCon "JoinList" (fromList [])) (LHsTyFn (LHsTyCon "JoinList" (fromList [])) (LHsTyCon "JoinList" (fromList [])))),LHsEqn "append" (fromList [LHsPCon "Nil" (fromList []),LHsPVar "ys"]) (LHsVar "ys"),LHsEqn "append" (fromList [LHsPVar "xs",LHsPCon "Nil" (fromList [])]) (LHsVar "xs"),LHsEqn "append" (fromList [LHsPVar "xs",LHsPVar "ys"]) (LHsApp (LHsApp (LHsCon "Append") (LHsVar "xs")) (LHsVar "ys"))]

data Input    = Low | High
data JoinList = Nil | One Input | Append JoinList JoinList

-- Various definitions for a set of user-defined datatypes
reverse :: JoinList -> JoinList
reverse Nil = Nil
reverse (One x) = One x
reverse (Append xs ys) = Append (reverse ys) (reverse xs)

duplicate :: JoinList -> JoinList
duplicate Nil = Nil
duplicate (One x) = Append (One x) (One x)
duplicate (Append xs ys) = Append (duplicate xs) (duplicate ys)

invert :: Bin -> Bin
invert Low = High
invert High = Low

or :: Bin -> Bin -> Bin
or Low Low = Low
or Low High = High
or High Low = High
or High High = High

singleton :: Bin -> JoinList
singleton x = One x

-- P is not enabled, so all datatypes are monomorphic
data MaybeInput = Nothing | Just Input

first :: JoinList -> MaybeInput
first Nil = Nothing
first (One x) = Just x
first (Append xs ys) =
    case first xs of
        Nothing -> first ys
        Just x -> Just x

append :: JoinList -> JoinList -> JoinList
append Nil ys = ys
append xs Nil = xs
append xs ys = Append xs ys
