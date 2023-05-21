{-# LANGUAGE OverloadedLists #-}

module Main where
import Data.List (nub)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (Text, drop, take, length, pack, unpack)
import Data.Sequence (Seq, fromList, deleteAt, length, (><))
import Data.Foldable (toList)

trigrams :: Seq Text -> Seq Text
trigrams input_seq = fromList (nub (concatMap trigrams_map input_seq))

trigrams_map :: Text -> [Text]
trigrams_map input_string = [Data.Text.take 3 (Data.Text.drop offset input_string) | offset <- [0..(Data.Text.length input_string - 3)]]

between_word_trigrams :: Seq Text -> Seq Text
between_word_trigrams input_list = fromList (nub (concatMap between_word_trigrams_map input_list))

between_word_trigrams_map :: Text -> [Text]
between_word_trigrams_map input_text = [Data.Text.take 1 (Data.Text.drop 1 input_text), Data.Text.drop (Data.Text.length input_text - 2) input_text]

get_trigrams :: Seq Text -> Seq Text
get_trigrams words_seq = trigrams words_seq >< between_word_trigrams words_seq

try_remove :: Int -> Int -> Seq Text -> Seq Text -> Seq Text
try_remove word_index end_index all_words trigrams_seq = do
    if word_index >= end_index then do
        all_words
    else do
        let new_seq = deleteAt word_index all_words
        if Data.Sequence.length trigrams_seq == Data.Sequence.length (get_trigrams new_seq) then do
            try_remove word_index (end_index - 1) new_seq trigrams_seq
        else do
            try_remove (word_index + 1) end_index all_words trigrams_seq

find_split :: String -> [String]
find_split input_string
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

main :: IO ()
main = do
    putStrLn "Input file: "
    input_filename <- getLine
    file_data <- readFile input_filename

    time_start <- getCurrentTime --benchmarking time

    let words_list = find_split (concat (splitOn "\"" (concat (splitOn " " (concat (splitOn "\n" file_data))))))
    let words_seq_padded = fromList [pack (" " ++ word ++ " ")| word <- words_list]

    let trigrams_seq = get_trigrams words_seq_padded

    let condensed_list = toList (try_remove 0 (Prelude.length words_list) words_seq_padded trigrams_seq)

    let unpadded_list = [Prelude.drop 1 (Prelude.take (Prelude.length (unpack word) - 1) (unpack word)) | word <- condensed_list]

    writeFile "output.txt" (concat ([word ++ " " | word <- unpadded_list]))

    let json_words = concat (["        \"" ++ word ++ "\",\n" | word <- unpadded_list])

    writeFile "output.json" ("{\n    \"total\": " ++ show (Prelude.length unpadded_list) ++ ",\n    \"texts\": [\n" ++ Prelude.take (Prelude.length json_words - 2) json_words ++ "\n    ]\n}")

    time_end <- getCurrentTime --benchmarking time
    putStr "Generated in: "
    print (diffUTCTime time_end time_start)