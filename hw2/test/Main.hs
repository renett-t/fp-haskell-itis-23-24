module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (foldl', sort)

import MyLib

main :: IO ()
main = defaultMain tests

prop_rev :: Property
prop_rev = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  rev (rev xs) === xs

tests :: TestTree
tests = testGroup "All Tests"
  [
    testProperty "reverse works" $ prop_rev
  , treeTests
  ]

treeTests :: TestTree
treeTests = testGroup "Tree Tests"
  [ traversalTests
  , insertTests
  , rotateTests
  ]

traversalTests :: TestTree
traversalTests = testGroup "traversal"
  [ testCase "empty" $ traversal empty @?= ([] :: [Int])
  , testCase "single elt" $ traversal (Node Nothing 1 Nothing) @?= [1]
  , testCase "three elts" $
    traversal (Node (Just $ leaf 1) 2 (Just $ leaf 3)) @?= [1,2,3]
  ]

isCorrect :: Ord a => Tree a -> Bool
isCorrect Empty = True
isCorrect (Node ml v mr) = maybe True isCorrect ml &&
  maybe True isCorrect mr &&
  all (<v) (maybe [] traversal ml) &&
  all (>=v) (maybe [] traversal mr)


arbitraryTree :: Size -> Gen (Tree Int)
arbitraryTree 0 = pure empty
arbitraryTree size = do
  leftSize <- Size <$> Gen.int (Range.linear 0 $ unSize size - 1)
  let rightSize = size - leftSize - 1
  l <- if leftSize == 0
       then pure Nothing
       else Just <$> arbitraryTree leftSize
  v <- Gen.int $ Range.linear 0 10000
  r <- if rightSize == 0
       then pure Nothing
       else Just <$> arbitraryTree rightSize
  pure $ Node l v r

treeGen :: Gen (Tree Int)
treeGen = Gen.sized arbitraryTree

-- Поменять генератор двоичных деревьев поиска так,
-- чтобы задавался диапазон элементов дерева и
-- в левом и в правом поддереве генерировались только
-- нужные элементы (чтобы BST было корректным)
arbitraryTreeInRange :: Int -> Int -> Size -> Gen (Tree Int)
arbitraryTreeInRange minValue maxValue 0 = pure Empty
arbitraryTreeInRange minValue maxValue size = do
  value <- Gen.int (Range.linear minValue maxValue)
  let leftRange = Range.linear minValue (value - 1)
  let rightRange = Range.linear (value + 1) maxValue

  leftSize <- Size <$> Gen.int (Range.linear 0 (unSize size - 1))
  rightSize <- Size <$> Gen.int (Range.linear 0 (unSize size - 1))

  leftSubtree <- if leftSize == 0
                   then pure Nothing
                   else Just <$> arbitraryTreeInRange minValue (value - 1) leftSize

  rightSubtree <- if rightSize == 0
                    then pure Nothing
                    else Just <$> arbitraryTreeInRange (value + 1) maxValue rightSize

  pure $ Node leftSubtree value rightSubtree

treeGenInRange :: Gen (Tree Int)
treeGenInRange = Gen.sized $ arbitraryTreeInRange 0 10000


prop_bst :: Property
prop_bst = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  assert $ isCorrect t

prop_inserts :: Property
prop_inserts = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  sort xs === traversal t

insertTests :: TestTree
insertTests = testGroup "insert"
  [ testProperty "BST is correct" prop_bst
  , testProperty "Insert really inserts" prop_inserts
  , testCase "1" $ isCorrect (leaf 1) @? "leaf 1"
  , testCase "3" $
    isCorrect (Node (Just $ leaf 1) 2 (Just $ leaf 2)) @? "tree 3"
  ]

 
prop_rotateLeft :: Property
prop_rotateLeft = property $ do
  t <- forAll treeGen
  let rotated = rotateLeft t
  assert $ isCorrect rotated

prop_rotateRight :: Property
prop_rotateRight = property $ do
  t <- forAll treeGen
  let rotated = rotateRight t
  assert $ isCorrect rotated

rotateTests :: TestTree
rotateTests = testGroup "rotateLeft n rotateRight"
  [ 
    testProperty "rotateLeft preserves BST" prop_rotateLeft
  , testProperty "rotateRight preserves BST" prop_rotateRight
  ]
