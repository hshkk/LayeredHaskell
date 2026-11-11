-- fromList [LHsData "Item" (fromList []) (fromList [("Box",fromList []),("Cup",fromList []),("Piano",fromList [])]),LHsSig "x" (LHsTyList (LHsTyCon "Item" (fromList []))),LHsEqn "x" (fromList []) (LHsList (fromList [LHsCon "Box",LHsCon "Cup",LHsCon "Piano"])),LHsSig "y" (LHsTyList (LHsTyFn (LHsTyCon "Item" (fromList [])) (LHsTyCon "Item" (fromList [])))),LHsEqn "y" (fromList []) (LHsList (fromList [LHsLam (fromList [LHsPVar "x"]) (LHsCon "Box"),LHsLam (fromList [LHsPVar "y"]) (LHsCon "Cup"),LHsLam (fromList [LHsPVar "z"]) (LHsCon "Piano")])),LHsSig "u" (LHsTyTuple (fromList [LHsTyCon "Item" (fromList []),LHsTyCon "Item" (fromList [])])),LHsEqn "u" (fromList []) (LHsTuple (fromList [LHsCon "Cup",LHsCon "Piano"])),LHsSig "v" (LHsTyFn (LHsTyTuple (fromList [LHsTyFn (LHsTyCon "Item" (fromList [])) (LHsTyCon "Item" (fromList [])),LHsTyCon "Item" (fromList [])])) (LHsTyFn (LHsTyFn (LHsTyCon "Item" (fromList [])) (LHsTyCon "Item" (fromList []))) (LHsTyCon "Item" (fromList [])))),LHsEqn "v" (fromList [LHsPTuple (fromList [LHsPVar "f",LHsPVar "z"]),LHsPVar "g"]) (LHsApp (LHsVar "f") (LHsApp (LHsVar "g") (LHsVar "z"))),LHsSig "z" (LHsTyFn (LHsTyList (LHsTyFn (LHsTyCon "Item" (fromList [])) (LHsTyCon "Item" (fromList [])))) (LHsTyFn (LHsTyFn (LHsTyCon "Item" (fromList [])) (LHsTyCon "Item" (fromList []))) (LHsTyCon "Item" (fromList [])))),LHsEqn "z" (fromList [LHsPList (fromList [LHsPVar "a",LHsPVar "b",LHsPVar "c"]),LHsPVar "f"]) (LHsApp (LHsVar "f") (LHsApp (LHsVar "a") (LHsApp (LHsVar "b") (LHsApp (LHsVar "c") (LHsCon "Piano"))))),LHsEqn "z" (fromList [LHsPListCons (LHsPVar "i") LHsPWild,LHsPVar "f"]) (LHsApp (LHsVar "i") (LHsApp (LHsVar "f") (LHsCon "Cup"))),LHsEqn "z" (fromList [LHsPWild,LHsPVar "f"]) (LHsApp (LHsVar "f") (LHsCon "Box"))]

-- L depends on D, so we can define datatypes at will
data Item = Box | Cup | Piano

-- Various (but somewhat contrived) definitions on lists
x :: [Item]
x = [Box, Cup, Piano]

-- L depends on F, so we can also use function types at will
y :: [Item -> Item]
y = [\x -> Box, \y -> Cup, \z -> Piano]

u :: (Item, Item)
u = (Cup, Piano)

v :: (Item -> Item, Item) -> (Item -> Item) -> Item
v (f,z) g = f (g z)

z :: [Item -> Item] -> (Item -> Item) -> Item
z [a,b,c] f = f (a (b (c Piano)))
z (i:_) f = i (f Cup)
z _ f = f Box