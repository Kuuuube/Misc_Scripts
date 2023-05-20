module Main where
import Data.List (nub, delete, sort)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (Text, drop, take, length, pack, unpack)

trigrams :: Text -> [Text]
trigrams input_string = [Data.Text.take 3 (Data.Text.drop offset input_string) | offset <- [0..(Data.Text.length input_string - 3)]]

between_word_trigrams :: [Text] -> [Text]
between_word_trigrams input_list = do
    let starts_and_ends_lists = [[Data.Text.take 1 (Data.Text.drop 1 current_word), Data.Text.drop (Data.Text.length current_word - 2) current_word] | current_word <- input_list]
    let starts_and_ends = nub (concat starts_and_ends_lists)
    starts_and_ends

get_trigrams :: [Text] -> [Text]
get_trigrams words_list = do
    sort (nub (concat [trigrams word | word <- words_list]) ++ between_word_trigrams words_list)

try_remove :: Int -> Int -> [Text] -> [Text] -> [Text]
try_remove word_index end_index all_words current_trigrams_list = do
    if word_index >= end_index then do
        all_words
    else do
        let current_word = all_words !! word_index
        let new_list = delete current_word all_words
        let new_trigrams_list = get_trigrams new_list
        if current_trigrams_list == new_trigrams_list then do
            try_remove word_index (end_index - 1) new_list new_trigrams_list
        else do
            try_remove (word_index + 1) end_index all_words current_trigrams_list

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
    let words_list_padded = [pack (" " ++ word ++ " ")| word <- words_list]

    let trigrams_list = get_trigrams words_list_padded

    let condensed_list = try_remove 0 (Prelude.length words_list) words_list_padded trigrams_list

    let unpadded_list = [Prelude.drop 1 (Prelude.take (Prelude.length (unpack word) - 1) (unpack word)) | word <- condensed_list]

    writeFile "output.txt" (concat ([word ++ " " | word <- unpadded_list]))

    let json_words = concat (["        \"" ++ word ++ "\",\n" | word <- unpadded_list])

    writeFile "output.json" ("{\n    \"total\": " ++ show (Prelude.length unpadded_list) ++ ",\n    \"texts\": [\n" ++ Prelude.take (Prelude.length json_words - 2) json_words ++ "\n    ]\n}")

    time_end <- getCurrentTime --benchmarking time
    putStr "Generated in: "
    print (diffUTCTime time_end time_start)