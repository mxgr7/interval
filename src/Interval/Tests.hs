
module Interval.Tests where

-- import qualified Data.Map as M
import qualified Data.Maybe
import           Interval
import qualified Prelude
import           Test.Hspec
-- import           Test.QuickCheck
import           Yahp hiding (complement)

main :: IO ()
main = hspec $ do
  describe "nowhere" $ it "should return nothing" $
    nowhere ! 'a' `shouldBe` (Nothing @Int)
  describe "everywhere" $ it "should return the input" $
    forM_ [minBound, 2, maxBound @Int] $ \x -> everywhere 'a' ! x `shouldBe` Just 'a'
  describe "toStepFunction" $ do
    it "should give constant function for unbounded interval (Int)" $
      shouldBe (Just 'a') $ eitherException (toStepFunction' [(unbounded, 'a')]) ! (3::Int)
    it "should give constant function for unbounded interval (Bool)" $
      shouldBe (Just 'a') $ eitherException (toStepFunction' [(unbounded, 'a')]) ! True
    let cmp i (input,at,res) = it ("should satisfy case A." <> show i) $ shouldBe (-- traceShow (M.toList $ fromUnsafeStepFunction sf) $
          sf ! at) res
            where sf = either (Prelude.error . toS) id $ toStepFunction' $ (first $ Data.Maybe.fromJust . uncurry fromBounds) <$> input
    zipWithM_ cmp [(1::Int)..] $ 
      [([((NegativeInfinity :: Extended Int, PositiveInfinity), 3 :: Int)], minBound, Just 3)
      ,([((NegativeInfinity :: Extended Int, PositiveInfinity), 3 :: Int)], maxBound, Just 3)
      ]
      <>
      let g = [((Finite 2, Finite 5),1)] in
        [(g, 1, Nothing)
        ,(g, 2, Just 1)
        ,(g, 3, Just 1)
        ,(g, 5, Just 1)
        ,(g, 6, Nothing)
        ,([], 6, Nothing)
        ]
      <>
      let g = [((NegativeInfinity, Finite 5),1), ((Finite 7, PositiveInfinity),0)] in
        [(g, 1, Just 1)
        ,(g, 5, Just 1)
        ,(g, 6, Nothing)
        ,(g, 7, Just 0)
        ,(g, 8, Just 0)
        ]
      <>
      let g = [((Finite 2, Finite 5),1), ((Finite 1, Finite 1),0)] in
        [(g, 1, Just 0)
        ,(g, 2, Just 1)
        ,(g, 3, Just 1)
        ,(g, 5, Just 1)
        ,(g, 6, Nothing)
        ,(g, 0, Nothing)
        ]

    let cmp i input =  it ("should satisfy case B." <> show i) $ shouldBe sf (Left ())
            where sf = either (Left . const ()) Right $ toStepFunction' $ (first $ Data.Maybe.fromJust . uncurry fromBounds) <$> input
                  sf :: Either () (StepFunction Int Int)
    zipWithM_ cmp [(1::Int)..] $ 
      [[((Finite (2::Int), Finite 5),1), ((Finite 1, Finite 2),0)]
      ,[((NegativeInfinity, Finite 5),1), ((Finite 1, Finite 2),0)]
      ,[((NegativeInfinity, Finite 1),1), ((Finite 1, Finite 2),0)]
      ]
