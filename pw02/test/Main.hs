import Set
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Set.empty" $ do
    it "returns False for any input" $ do
      empty 1 `shouldBe` False
      empty (-1) `shouldBe` False
      empty 0 `shouldBe` False

  describe "Set.singleton" $ do
    it "returns True for the singleton element" $ do
      let s = singleton 5
      s 5 `shouldBe` True

    it "returns False for any other element" $ do
      let s = singleton 5
      s 4 `shouldBe` False
      s 6 `shouldBe` False

  describe "Set.insert" $ do
    it "inserts an element into an empty set" $ do
      let s = insert 3 empty
      s 3 `shouldBe` True

    it "keeps existing elements and adds a new one" $ do
      let s = insert 3 . insert 2 $ empty
      s 2 `shouldBe` True
      s 3 `shouldBe` True

  describe "Set.union" $ do
    it "contains all elements from both sets" $ do
      let s1 = insert 1 empty
      let s2 = insert 2 empty
      let sUnion = union s1 s2
      sUnion 1 `shouldBe` True
      sUnion 2 `shouldBe` True

  describe "Set.delete" $ do
    it "removes an element from a set" $ do
      let s = insert 5 empty
      let s' = delete 5 s
      s' 5 `shouldBe` False

    it "leaves set unchanged if element not present" $ do
      let s = insert 3 empty
      let s' = delete 4 s
      s' 3 `shouldBe` True
      s' 4 `shouldBe` False

  describe "Set.pop" $ do
    it "removes and returns the smallest element from a set" $ do
      let s = insert 3 . insert 1 . insert 2 $ empty
      let (minElem, s') = pop s
      minElem `shouldBe` Just 1
      s' 1 `shouldBe` False

    it "returns Nothing from an empty set" $ do
      let (minElem, _) = pop empty
      minElem `shouldBe` Nothing
    
  describe "Set.member" $ do
    it "returns True if an element is in the set" $ do
      let s = insert 5 empty
      member s 5 `shouldBe` True

    it "returns False if an element is not in the set" $ do
      member empty 5 `shouldBe` False

  describe "Set.size" $ do
    it "returns 0 for an empty set" $ do
      size empty `shouldBe` 0

    it "returns the correct size for a non-empty set" $ do
      let s = insert 3 . insert 5 . insert (-5) $ empty
      size s `shouldBe` 3

    it "does not count duplicates" $ do
      let s = insert 3 . insert 3 $ empty
      size s `shouldBe` 1

  describe "Set.union" $ do
    it "contains all elements from both sets" $ do
      let s1 = insert 1 empty
      let s2 = insert 2 empty
      let sUnion = union s1 s2
      sUnion 1 `shouldBe` True
      sUnion 2 `shouldBe` True

    it "contains no other elements" $ do
      let s1 = insert 1 empty
      let s2 = insert 2 empty
      let sUnion = union s1 s2
      sUnion 3 `shouldBe` False

  describe "Set.intersection" $ do
    it "contains only elements present in both sets" $ do
      let s1 = insert 1 . insert 2 $ empty
      let s2 = insert 2 . insert 3 $ empty
      let sIntersect = intersection s1 s2
      sIntersect 2 `shouldBe` True

    it "does not contain elements not present in both sets" $ do
      let s1 = insert 1 . insert 2 $ empty
      let s2 = insert 2 . insert 3 $ empty
      let sIntersect = intersection s1 s2
      sIntersect 1 `shouldBe` False
      sIntersect 3 `shouldBe` False
    
  describe "Set.difference" $ do
    it "contains elements from the first set not in the second set" $ do
      let s1 = insert 1 . insert 2 $ empty
      let s2 = insert 2 . insert 3 $ empty
      let sDiff = difference s1 s2
      sDiff 1 `shouldBe` True

    it "does not contain elements present in the second set" $ do
      let s1 = insert 1 . insert 2 $ empty
      let s2 = insert 2 . insert 3 $ empty
      let sDiff = difference s1 s2
      sDiff 2 `shouldBe` False
      sDiff 3 `shouldBe` False
    
  describe "Set.subset" $ do
    it "returns True if the first set is a subset of the second set" $ do
      let s1 = insert 1 empty
      let s2 = insert 1 . insert 2 $ empty
      subset s1 s2 `shouldBe` True

    it "returns False if the first set is not a subset of the second set" $ do
      let s1 = insert 1 . insert 3 $ empty
      let s2 = insert 1 . insert 2 $ empty
      subset s1 s2 `shouldBe` False
      
  describe "Set.disjoint" $ do
    it "returns True if the sets have no elements in common" $ do
      let s1 = insert 1 empty
      let s2 = insert 2 empty
      disjoint s1 s2 `shouldBe` True

    it "returns False if the sets have at least one element in common" $ do
      let s1 = insert 1 . insert 2 $ empty
      let s2 = insert 2 . insert 3 $ empty
      disjoint s1 s2 `shouldBe` False

  describe "Set.apply" $ do
    it "applies a function to each element of the set" $ do
      let s = fromList [1, 2, 3]
      let sApplied = apply s (+1)
      toList sApplied `shouldMatchList` [2, 3, 4]

    it "can produce a set with fewer elements if the function is not injective" $ do
      let s = fromList [1, 2, 3]
      let sApplied = apply s (const 1)
      toList sApplied `shouldMatchList` [1]

  describe "Set.select" $ do
    it "selects elements that satisfy the predicate" $ do
      let s = fromList [1, 2, 3, 4]
      let sSelected = select s even
      toList sSelected `shouldMatchList` [2, 4]

    it "returns an empty set if no element satisfies the predicate" $ do
      let s = fromList [1, 3, 5]
      let sSelected = select s even
      toList sSelected `shouldBe` []

  describe "Set.every" $ do
    it "returns True if every element satisfies the predicate" $ do
      let s = fromList [2, 4, 6]
      every s even `shouldBe` True

    it "returns False if at least one element does not satisfy the predicate" $ do
      let s = fromList [2, 4, 5]
      every s even `shouldBe` False

  describe "Set.reduce" $ do
    it "reduces the set to a single value using the provided function" $ do
      let s = fromList [1, 2, 3]
      reduce s (+) 0 `shouldBe` 6

    it "returns the initial value for an empty set" $ do
      reduce empty (+) 0 `shouldBe` 0

  describe "Set.some" $ do
    it "returns True if some element satisfies the predicate" $ do
      let s = fromList [1, 2, 3]
      some s even `shouldBe` True

    it "returns False if no element satisfies the predicate" $ do
      let s = fromList [1, 3, 5]
      some s even `shouldBe` False

  describe "Set.none" $ do
    it "returns True if no element satisfies the predicate" $ do
      let s = fromList [1, 3, 5]
      none s even `shouldBe` True

    it "returns False if some element satisfies the predicate" $ do
      let s = fromList [1, 2, 3]
      none s even `shouldBe` False

  describe "Set.findMin" $ do
    it "finds the smallest element in a set" $ do
      let s = fromList [5, 1, 3]
      findMin s `shouldBe` Just 1

    it "returns Nothing for an empty set" $ do
      findMin empty `shouldBe` Nothing

  describe "Set.findMax" $ do
    it "finds the largest element in a set" $ do
      let s = fromList [5, 1, 3]
      findMax s `shouldBe` Just 5

    it "returns Nothing for an empty set" $ do
      findMax empty `shouldBe` Nothing

  describe "Set.partition" $ do
    it "partitions a set into two based on a predicate" $ do
      let s = fromList [1, 2, 3, 4]
      let (left, right) = partition s even
      toList left `shouldMatchList` [2, 4]
      toList right `shouldMatchList` [1, 3]

  describe "Set.split" $ do
    it "splits a set into two at a specified value" $ do
      let s = fromList [1, 2, 3, 4]
      let (below, atAndAbove) = split s 3
      toList below `shouldMatchList` [1, 2]
      toList atAndAbove `shouldMatchList` [3, 4]

  describe "Set.fromList and Set.toList" $ do
    it "fromList creates a set from a list" $ do
      let s = fromList [1, 2, 3]
      (s 1 && s 2 && s 3) `shouldBe` True

    it "toList creates a list from a set" $ do
      let s = fromList [3, 1, 2]
      toList s `shouldMatchList` [1, 2, 3]

    it "toList and fromList are inverses" $ do
      let list = [1, 2, 3]
      let set = fromList list
      toList set `shouldMatchList` list

  describe "Show instance for Set" $ do
    it "correctly shows an empty set" $ do
      show empty `shouldBe` "{}"

    it "correctly shows a singleton set" $ do
      let s = singleton 5
      show s `shouldBe` "{5}"

    it "correctly shows a set with multiple elements in ascending order" $ do
      let s = fromList [3, 1, 2]
      show s `shouldBe` "{1, 2, 3}"

    it "correctly shows a set with negative and positive elements in ascending order" $ do
      let s = fromList [0, -1, 1, 0]
      show s `shouldBe` "{-1, 0, 1}"

    it "correctly shows a set with elements up to the bounds" $ do
      let s = insert (-bound) . insert bound $ empty
      show s `shouldBe` "{" ++ show (-bound) ++ ", " ++ show bound ++ "}"
