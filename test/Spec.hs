import Test.Hspec

import Utils ( generateList, generateUVec, generateReverseUVec )

import qualified Scan
import qualified Data.Vector.Unboxed         as UV
import qualified Sort
import qualified Sort

main :: IO ()
main = hspec $ do
    describe "Scan.sps" $ do
        it "correctly calculates prefix sum" $ do
            Scan.sps (+) (generateList 2) `shouldBe` [1, 3, 6, 10]

    describe "Scan.parSps2" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parSps2 (+) 2 (generateList 2) `shouldBe` [1, 3, 6, 10]

    describe "Scan.parSps3" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parSps3 (+) 10 6 (generateList 6) `shouldBe` scanl1 (+) [1..2^6]
    
    describe "Scan.ldf" $ do
        it "correctly calculates prefix sum" $ do
            Scan.ldf (+) (generateList 6) `shouldBe` scanl1 (+) [1..2^6]

    describe "Scan.parldf" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parLdf (+) 10 6 (generateList 6) `shouldBe` scanl1 (+) [1..2^6]

    describe "Scan.parSpsUBVec" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parSpsUBVec (+) 10 6 (generateUVec 6) `shouldBe` UV.fromList (scanl1 (+) [1..2^6])

    describe "Scan.parLdfUBVec" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parLdfUBVec (+) 10 6 (generateUVec 6) `shouldBe` UV.fromList (scanl1 (+) [1..2^6])    

    describe "Scan.parLdfUBVecNC" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parLdfUBVecNC (+) 6 (generateUVec 6) `shouldBe` UV.fromList (scanl1 (+) [1..2^6])

    describe "Scan.parLdfChunkUBVec" $ do
        it "correctly calculates prefix sum" $ do
            Scan.parLdfChunkUBVec (+) 2 (generateUVec 8) `shouldBe` UV.fromList (scanl1 (+) [1..2^8])

    describe "Sort.batcherMergeSort" $ do
        it "correctly sorts the input vector" $ do
            Sort.batcherMergeSort (generateReverseUVec 8) `shouldBe` UV.fromList [1..2^8]

    describe "Sort.parBatcherMergeSort" $ do
        it "correctly sorts the input vector" $ do
            Sort.parBatcherMergeSort 8 (generateReverseUVec 8) `shouldBe` UV.fromList [1..2^8]