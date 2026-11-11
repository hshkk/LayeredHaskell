-- fromList [LHsData "Tree" (fromList ["a"]) (fromList [("Empty",fromList []),("Node",fromList [LHsTyVar "a",LHsTyCon "Tree" (fromList [LHsTyVar "a"]),LHsTyCon "Tree" (fromList [LHsTyVar "a"])])]),LHsSig "map" (LHsTyFn (LHsTyFn (LHsTyVar "a") (LHsTyVar "b")) (LHsTyFn (LHsTyCon "Tree" (fromList [LHsTyVar "a"])) (LHsTyCon "Tree" (fromList [LHsTyVar "b"])))),LHsEqn "map" (fromList [LHsPWild,LHsPCon "Empty" (fromList [])]) (LHsCon "Empty"),LHsEqn "map" (fromList [LHsPVar "f",LHsPCon "Node" (fromList [LHsPVar "x",LHsPVar "l",LHsPVar "r"])]) (LHsApp (LHsApp (LHsApp (LHsCon "Node") (LHsApp (LHsVar "f") (LHsVar "x"))) (LHsApp (LHsApp (LHsVar "map") (LHsVar "f")) (LHsVar "l"))) (LHsApp (LHsApp (LHsVar "map") (LHsVar "f")) (LHsVar "r"))),LHsSig "fold" (LHsTyFn (LHsTyFn (LHsTyVar "a") (LHsTyFn (LHsTyVar "b") (LHsTyFn (LHsTyVar "b") (LHsTyVar "b")))) (LHsTyFn (LHsTyVar "b") (LHsTyFn (LHsTyCon "Tree" (fromList [LHsTyVar "a"])) (LHsTyVar "b")))),LHsEqn "fold" (fromList [LHsPWild,LHsPVar "acc",LHsPCon "Empty" (fromList [])]) (LHsVar "acc"),LHsEqn "fold" (fromList [LHsPVar "f",LHsPVar "acc",LHsPCon "Node" (fromList [LHsPVar "x",LHsPVar "l",LHsPVar "r"])]) (LHsApp (LHsApp (LHsApp (LHsVar "f") (LHsVar "x")) (LHsApp (LHsApp (LHsApp (LHsVar "fold") (LHsVar "f")) (LHsVar "acc")) (LHsVar "l"))) (LHsApp (LHsApp (LHsApp (LHsVar "fold") (LHsVar "f")) (LHsVar "acc")) (LHsVar "r"))),LHsSig "mirror" (LHsTyFn (LHsTyCon "Tree" (fromList [LHsTyVar "a"])) (LHsTyCon "Tree" (fromList [LHsTyVar "a"]))),LHsEqn "mirror" (fromList [LHsPCon "Empty" (fromList [])]) (LHsCon "Empty"),LHsEqn "mirror" (fromList [LHsPCon "Node" (fromList [LHsPVar "x",LHsPVar "l",LHsPVar "r"])]) (LHsApp (LHsApp (LHsApp (LHsCon "Node") (LHsVar "x")) (LHsApp (LHsVar "mirror") (LHsVar "r"))) (LHsApp (LHsVar "mirror") (LHsVar "l"))),LHsSig "zip" (LHsTyFn (LHsTyFn (LHsTyVar "a") (LHsTyFn (LHsTyVar "b") (LHsTyVar "c"))) (LHsTyFn (LHsTyCon "Tree" (fromList [LHsTyVar "a"])) (LHsTyFn (LHsTyCon "Tree" (fromList [LHsTyVar "b"])) (LHsTyCon "Tree" (fromList [LHsTyVar "c"]))))),LHsEqn "zip" (fromList [LHsPWild,LHsPCon "Empty" (fromList []),LHsPWild]) (LHsCon "Empty"),LHsEqn "zip" (fromList [LHsPWild,LHsPWild,LHsPCon "Empty" (fromList [])]) (LHsCon "Empty"),LHsEqn "zip" (fromList [LHsPVar "f",LHsPCon "Node" (fromList [LHsPVar "x",LHsPVar "l1",LHsPVar "r1"]),LHsPCon "Node" (fromList [LHsPVar "y",LHsPVar "l2",LHsPVar "r2"])]) (LHsApp (LHsApp (LHsApp (LHsCon "Node") (LHsApp (LHsApp (LHsVar "f") (LHsVar "x")) (LHsVar "y"))) (LHsApp (LHsApp (LHsApp (LHsVar "zip") (LHsVar "f")) (LHsVar "l1")) (LHsVar "l2"))) (LHsApp (LHsApp (LHsApp (LHsVar "zip") (LHsVar "f")) (LHsVar "r1")) (LHsVar "r2")))]

data Tree a
    = Empty
    | Node a (Tree a) (Tree a)

-- Various definitions for a user-defined polymorphic datatype
-- ...this feature combination is quite expressive!
map :: (a -> b) -> Tree a -> Tree b
map _ Empty        = Empty
map f (Node x l r) = Node (f x) (map f l) (map f r)

fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold _ acc Empty        = acc
fold f acc (Node x l r) = f x (fold f acc l) (fold f acc r)

mirror :: Tree a -> Tree a
mirror Empty        = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

zip :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zip _ Empty _ = Empty
zip _ _ Empty = Empty
zip f (Node x l1 r1) (Node y l2 r2) =
    Node (f x y) (zip f l1 l2) (zip f r1 r2)