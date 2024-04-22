{-
  test/Main.hs - Unit test suite for the CSV library.
  Authors: Loïc Herman, Massimo Stefani
-}
import CSV
import Control.Exception (evaluate)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    describe "create" $ do
      it "correctly constructs a CSV from a string" $ do
        let input = "Name,Age\nAlice,24\nBob,30"
        let expected = File ["Name", "Age"] [["Alice", "24"], ["Bob", "30"]]
        create input `shouldBe` expected

      it "throws an error on empty input" $ do
        evaluate (create "") `shouldThrow` anyErrorCall

    describe "Show instance for CSV" $ do
      it "converts a CSV object back to its string representation" $ do
        let csv = File ["Name", "Age"] [["Alice", "24"], ["Bob", "30"]]
        show csv `shouldBe` "Name,Age\nAlice,24\nBob,30\n"

    describe "Read instance for CSV" $ do
      it "parses a string into a CSV object" $ do
        let input = "Name,Age\nAlice,24\nBob,30\n"
        read input `shouldBe` File ["Name", "Age"] [["Alice", "24"], ["Bob", "30"]]

      it "handles reading an empty last line gracefully" $ do
        let input = "Name,Age\nAlice,24\nBob,30\n"
        read input `shouldBe` File ["Name", "Age"] [["Alice", "24"], ["Bob", "30"]]

      it "handles reading carriage returns" $ do
        let input = "Name,Age\r\nAlice,24\r\nBob,30\r\n"
        read input `shouldBe` File ["Name", "Age"] [["Alice", "24"], ["Bob", "30"]]

  describe "Aggregations" $ do
    describe "combine" $ do
      it "successfully merges two CSV files with matching headers" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"], ["2", "Bob"]]
        let csv2 = File ["ID", "Name"] [["3", "Charlie"], ["4", "David"]]
        combine csv1 csv2 `shouldBe` File ["ID", "Name"] [["1", "Alice"], ["2", "Bob"], ["3", "Charlie"], ["4", "David"]]

      it "throws an error when CSV files have different headers" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        let csv2 = File ["ID", "Age"] [["1", "24"]]
        evaluate (combine csv1 csv2) `shouldThrow` anyErrorCall

    describe "merge" $ do
      it "merges multiple CSV files into one" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        let csv2 = File ["ID", "Name"] [["2", "Bob"]]
        let csv3 = File ["ID", "Name"] [["3", "Charlie"]]
        merge [csv1, csv2, csv3] `shouldBe` File ["ID", "Name"] [["1", "Alice"], ["2", "Bob"], ["3", "Charlie"]]

      it "throws an error if no CSV files are provided" $ do
        evaluate (merge []) `shouldThrow` anyErrorCall

    describe "join" $ do
      it "joins two CSV files on a common column" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"], ["2", "Bob"]]
        let csv2 = File ["ID", "Age"] [["1", "24"], ["2", "30"]]
        join "ID" csv1 csv2 `shouldBe` File ["ID", "Name", "Age"] [["1", "Alice", "24"], ["2", "Bob", "30"]]

      it "returns a CSV with headers only if no common column values are found" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        let csv2 = File ["ID", "Age"] [["2", "30"]]
        join "ID" csv1 csv2 `shouldBe` File ["ID", "Name", "Age"] []

      it "returns a CSV with headers only if the common column does not exist in one of the CSVs" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        let csv2 = File ["Age"] [["24"]]
        join "ID" csv1 csv2 `shouldBe` File ["ID", "Name", "Age"] []

    describe "joinMany" $ do
      it "joins multiple CSV files on a common column" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        let csv2 = File ["ID", "Age"] [["1", "24"]]
        let csv3 = File ["ID", "Country"] [["1", "Switzerland"]]
        joinMany "ID" [csv1, csv2, csv3] `shouldBe` File ["ID", "Name", "Age", "Country"] [["1", "Alice", "24", "Switzerland"]]

      it "throws an error if no CSV files are provided for joining" $ do
        evaluate (joinMany "ID" []) `shouldThrow` anyErrorCall

      it "returns a CSV with headers only if only one CSV and column is not present" $ do
        let csv1 = File ["ID", "Name"] [["1", "Alice"]]
        joinMany "ZZZ" [csv1] `shouldBe` File ["ID", "Name"] []

  describe "Filtering" $ do
    describe "Read instance for Equality Filter" $ do
      it "parses a string into an EqualityFilter object" $ do
        (read "Name=Alice" :: EqualityFilter) `shouldBe` EqFilter ("Name", "Alice")

    describe "filterRecords" $ do
      let header = ["ID", "Name", "Age"]
      let records =
            [ ["1", "Alice", "24"],
              ["2", "Bob", "30"],
              ["3", "Charlie", "25"],
              ["4", "David", "24"]
            ]
      let csv = File header records

      it "filters records by a single equality filter" $ do
        let filters = [EqFilter ("Name", "Alice")]
        let expected = File header [["1", "Alice", "24"]]
        filterRecords filters csv `shouldBe` expected

      it "filters records by multiple equality filters" $ do
        let filters = [EqFilter ("Name", "David"), EqFilter ("Age", "24")]
        let expected = File header [["4", "David", "24"]]
        filterRecords filters csv `shouldBe` expected

      it "returns an empty result when no records match" $ do
        let filters = [EqFilter ("Name", "Zoe")]
        let expected = File header []
        filterRecords filters csv `shouldBe` expected

      it "returns an empty result with filters with non-existent columns" $ do
        let filters = [EqFilter ("Location", "Switzerland")]
        let expected = File header []
        filterRecords filters csv `shouldBe` expected

  describe "Formatting" $ do
    describe "format" $ do
      it "formats a CSV with columns of varying widths correctly" $ do
        let csv = File ["ID", "Name", "Age"] [["1", "Alice", "24"], ["2", "Bob", "30"], ["10", "Charlie", "25"]]
        let expected =
              unlines
                [ "ID Name    Age",
                  "1  Alice   24 ",
                  "2  Bob     30 ",
                  "10 Charlie 25 "
                ]
        format csv `shouldBe` expected

      it "formats a CSV with an empty body correctly" $ do
        let csv = File ["ID", "Name", "Age"] []
        let expected = "ID Name Age\n"
        format csv `shouldBe` expected

      it "formats a CSV with only one column and multiple rows" $ do
        let csv = File ["Name"] [["Alice"], ["Bob"], ["Charlie"], ["David"]]
        let expected = unlines ["Name   ", "Alice  ", "Bob    ", "Charlie", "David  "]
        format csv `shouldBe` expected

      it "formats a CSV with single-character columns and records" $ do
        let csv = File ["I", "N", "A"] [["1", "A", "2"], ["2", "B", "3"], ["3", "C", "4"]]
        let expected = unlines ["I N A", "1 A 2", "2 B 3", "3 C 4"]
        format csv `shouldBe` expected

      it "handles formatting a CSV where one field is significantly longer than others" $ do
        let csv = File ["ID", "Description", "Age"] [["1", "Short", "24"], ["2", "A very long description indeed", "30"]]
        let expected =
              unlines
                [ "ID Description                    Age",
                  "1  Short                          24 ",
                  "2  A very long description indeed 30 "
                ]
        format csv `shouldBe` expected

  describe "Sorting" $ do
    describe "sortRecords" $ do
      let header = ["ID", "Name", "Age"]
      let records =
            [ ["2", "Charlie", "25"],
              ["1", "Alice", "24"],
              ["4", "David", "24"],
              ["3", "Bob", "30"]
            ]
      let csv = File header records

      it "sorts records by a single column" $ do
        let sortedCSV = sortRecords ["ID"] csv
        let expected = File header [["1", "Alice", "24"], ["2", "Charlie", "25"], ["3", "Bob", "30"], ["4", "David", "24"]]
        sortedCSV `shouldBe` expected

      it "sorts records by multiple columns" $ do
        let sortedCSV = sortRecords ["Age", "Name"] csv
        let expected = File header [["1", "Alice", "24"], ["4", "David", "24"], ["2", "Charlie", "25"], ["3", "Bob", "30"]]
        sortedCSV `shouldBe` expected

      it "handles sorting by descending order when rows are already ordered by another column" $ do
        let preSortedCSV = File header [["1", "Alice", "30"], ["2", "Bob", "25"], ["3", "Charlie", "25"], ["4", "David", "24"]]
        let sortedCSV = sortRecords ["Age", "Name"] preSortedCSV
        let expected = File header [["4", "David", "24"], ["2", "Bob", "25"], ["3", "Charlie", "25"], ["1", "Alice", "30"]]
        sortedCSV `shouldBe` expected
