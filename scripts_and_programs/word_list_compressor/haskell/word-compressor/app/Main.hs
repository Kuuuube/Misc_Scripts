module Main where
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (Text, drop, take, length, pack, unpack, toLower)
import Data.Sequence (Seq, fromList, deleteAt, length, index)
import Data.Foldable (toList)
import Data.HashMap.Lazy (HashMap, fromList, unionWith, insertWith, findWithDefault)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

allWordTrigrams :: Text -> [Text]
allWordTrigrams input_text = middleWordTrigramsMap input_text ++ betweenWordTrigramsMap input_text

middleWordTrigramsMap :: Text -> [Text]
middleWordTrigramsMap input_text = [Data.Text.take 3 (Data.Text.drop offset input_text) | offset <- [0..(Data.Text.length input_text - 3)]]

betweenWordTrigramsMap :: Text -> [Text]
betweenWordTrigramsMap input_text = [Data.Text.take 1 (Data.Text.drop 1 input_text), Data.Text.drop (Data.Text.length input_text - 2) input_text]

getTrigrams :: Seq Text -> Seq [Text]
getTrigrams words_seq = fmap allWordTrigrams words_seq

trigramsToHashMap :: Int -> Int -> Seq [Text] -> HashMap Text Int -> HashMap Text Int
trigramsToHashMap trigrams_index end_index trigrams hashmap = do
    if trigrams_index > end_index then
        hashmap
    else do
        let current_trigrams = index trigrams trigrams_index
        let new_hashmap = Data.HashMap.Lazy.unionWith addNums (Data.HashMap.Lazy.fromList (map toTuple current_trigrams)) hashmap
        trigramsToHashMap (trigrams_index + 1) end_index trigrams new_hashmap

toTuple :: Text -> (Text, Int)
toTuple input_text = (input_text, 1)

addNums :: Num a => a -> a -> a
addNums a b = a + b

inverseSubNums :: Num a => a -> a -> a
inverseSubNums a b = b - a

tryRemoveHashMap :: Int -> Int -> Seq [Text] -> Seq Text -> HashMap Text Int -> Seq Text
tryRemoveHashMap current_index end_index trigrams all_words trigram_hashmap = do
    if current_index > end_index then
        all_words
    else do
        let current_trigrams = index trigrams current_index
        let trigram_check = partialUnion 0 (Prelude.length current_trigrams - 1) current_trigrams trigram_hashmap
        if snd trigram_check then --True = zero has been found
            tryRemoveHashMap (current_index + 1) end_index trigrams all_words trigram_hashmap
        else do
            let new_words = deleteAt current_index all_words
            let new_trigrams = deleteAt current_index trigrams
            tryRemoveHashMap current_index (end_index - 1) new_trigrams new_words (fst trigram_check)


partialUnion :: Int -> Int -> [Text] -> HashMap Text Int -> (HashMap Text Int, Bool)
partialUnion current_index end_index trigrams_list hashmap = do
    if current_index > end_index then
        (hashmap, False)
    else do
        let new_hashmap = insertWith inverseSubNums (trigrams_list !! current_index) 1 hashmap
        let find_zero = findWithDefault 1 (trigrams_list !! current_index) new_hashmap

        if find_zero == 0 then do
            (hashmap, True)
        else
            partialUnion (current_index + 1) end_index trigrams_list new_hashmap

findSplit :: String -> [String]
findSplit input_string
    | Prelude.length (splitOn "words:[" input_string) > 1 = do
        let split_key = splitOn "words:[" input_string !! 1
        let split_end = splitOn "]" split_key !! 0
        splitOn "," split_end
    | Prelude.length (splitOn "texts:[" input_string) > 1 = do
        let split_key = splitOn "texts:[" input_string !! 1
        let split_end = splitOn "]" split_key !! 0
        splitOn "," split_end
    | otherwise = do
        traceShow "Unsupported json key" (pure ())
        []

filterCase :: Text -> Text
filterCase input_text = toLower input_text

main :: IO ()
main = do
    putStrLn "Input file: "
    input_filename <- getLine
    file_data <- readFile input_filename
    --intentionally not handled, an input file should never be assumed

    putStrLn "Ignore uppercase and lowercase (y/n): "
    filter_case_input <- getLine
    let filter_case = filter_case_input == fromMaybe "y" (readMaybe filter_case_input)

    time_start <- getCurrentTime --benchmarking time

    let words_list = findSplit (concat (splitOn "\"" (concat (splitOn " " (concat (splitOn "\n" file_data))))))
    let words_seq_padded = Data.Sequence.fromList [pack (" " ++ word ++ " ") | word <- words_list]

    --somewhere there is a bug that is mysteriously fixed by running this twice
    --running it twice should not be required but on word lists >10k processing seems to randomly break on some words
    --doing two passes causes at most less than a second extra compute time
    let trigrams_raw_first_pass = if filter_case then getTrigrams (fmap filterCase words_seq_padded) else getTrigrams words_seq_padded
    let trigrams_hashmap_first_pass = trigramsToHashMap 0 (Data.Sequence.length trigrams_raw_first_pass - 1) trigrams_raw_first_pass (Data.HashMap.Lazy.fromList [(pack "", 1)])
    let condensed_list_first_pass = tryRemoveHashMap 0 (Data.Sequence.length trigrams_raw_first_pass - 1) trigrams_raw_first_pass words_seq_padded trigrams_hashmap_first_pass

    let trigrams_raw_second_pass = if filter_case then getTrigrams (fmap filterCase condensed_list_first_pass) else getTrigrams condensed_list_first_pass
    let trigrams_hashmap_second_pass = trigramsToHashMap 0 (Data.Sequence.length trigrams_raw_second_pass - 1) trigrams_raw_second_pass (Data.HashMap.Lazy.fromList [(pack "", 1)])
    let condensed_list = Data.Foldable.toList (tryRemoveHashMap 0 (Data.Sequence.length trigrams_raw_second_pass - 1) trigrams_raw_second_pass condensed_list_first_pass trigrams_hashmap_second_pass)

    let unpadded_list = [Prelude.drop 1 (Prelude.take (Prelude.length (unpack word) - 1) (unpack word)) | word <- condensed_list]

    writeFile "output.txt" (concat ([word ++ " " | word <- unpadded_list]))
    let json_words = concat (["        \"" ++ word ++ "\",\n" | word <- unpadded_list])
    writeFile "output.json" ("{\n    \"total\": " ++ show (Prelude.length unpadded_list) ++ ",\n    \"texts\": [\n" ++ Prelude.take (Prelude.length json_words - 2) json_words ++ "\n    ]\n}")

    time_end <- getCurrentTime --benchmarking time
    putStr "Generated in: "
    print (diffUTCTime time_end time_start)