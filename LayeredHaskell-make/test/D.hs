-- fromList [LHsData "Color" (fromList []) (fromList [("Cyan",fromList []),("Magenta",fromList []),("Yellow",fromList []),("Black",fromList [])]),LHsData "Shape" (fromList []) (fromList [("Circle",fromList []),("Triangle",fromList []),("Square",fromList []),("RectifiedTruncatedIcosahedron",fromList [])]),LHsData "ColoredShape" (fromList []) (fromList [("CS",fromList [LHsTyCon "Color" (fromList []),LHsTyCon "Shape" (fromList [])]),("Pair",fromList [LHsTyCon "ColoredShape" (fromList []),LHsTyCon "ColoredShape" (fromList [])])]),LHsSig "cyanCircle" (LHsTyCon "ColoredShape" (fromList [])),LHsEqn "cyanCircle" (fromList []) (LHsApp (LHsApp (LHsCon "CS") (LHsCon "Cyan")) (LHsCon "Circle")),LHsSig "yellowSquare" (LHsTyCon "ColoredShape" (fromList [])),LHsEqn "yellowSquare" (fromList []) (LHsApp (LHsApp (LHsCon "CS") (LHsCon "Yellow")) (LHsCon "Square")),LHsSig "cyanCircleAndYellowSquare" (LHsTyCon "ColoredShape" (fromList [])),LHsEqn "cyanCircleAndYellowSquare" (fromList []) (LHsLet (fromList [LHsEqn "c" (fromList []) (LHsVar "cyanCircle"),LHsEqn "y" (fromList []) (LHsVar "yellowSquare")]) (LHsApp (LHsApp (LHsCon "Pair") (LHsVar "c")) (LHsVar "y")))]

-- Datatype declaration with nullary constructors
data Color = Cyan | Magenta | Yellow | Black

-- Datatype declaration with layout
data Shape 
    = Circle
    | Triangle
    | Square
    | RectifiedTruncatedIcosahedron

-- Datatype declaration with n-ary constructors
data ColoredShape = CS Color Shape | Pair ColoredShape ColoredShape

-- Various declarations
cyanCircle :: ColoredShape
cyanCircle = CS Cyan Circle

yellowSquare :: ColoredShape
yellowSquare = CS Yellow Square

cyanCircleAndYellowSquare :: ColoredShape
cyanCircleAndYellowSquare = 
    let c = cyanCircle
        y = yellowSquare
    in Pair c y