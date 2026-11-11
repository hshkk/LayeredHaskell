-- fromList [LHsEqn "x" (fromList []) (LHsVar "y"),LHsEqn "y" (fromList []) (LHsLet (fromList [LHsEqn "z" (fromList []) (LHsVar "x")]) (LHsVar "z")),LHsEqn "z" (fromList []) (LHsLet (fromList [LHsEqn "w" (fromList []) (LHsVar "y"),LHsEqn "y" (fromList []) (LHsVar "w"),LHsEqn "u" (fromList []) (LHsVar "y")]) (LHsVar "u")),LHsEqn "w" (fromList []) (LHsCon "Con")]

-- Basic declaration
x = y

-- Let expressions
y = let z = x in z

-- Let expressions with layout
z = let w = y
        y = w
        u = y in u

-- Constructors (E parses them by default)
w = Con