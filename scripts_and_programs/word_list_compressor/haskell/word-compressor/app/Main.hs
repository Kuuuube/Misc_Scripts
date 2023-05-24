module Main where
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (Text, drop, take, length, pack, unpack, toLower)
import Data.Sequence (Seq, fromList, deleteAt, (><), length, chunksOf, index)
import Data.Foldable (toList)
import Data.HashSet (fromList, size)

middleWordTrigrams :: Seq Text -> Seq [Text]
middleWordTrigrams input_seq = fmap middleWordTrigramsMap input_seq

middleWordTrigramsMap :: Text -> [Text]
middleWordTrigramsMap input_string = [Data.Text.take 3 (Data.Text.drop offset input_string) | offset <- [0..(Data.Text.length input_string - 3)]]

betweenWordTrigrams :: Seq Text -> Seq [Text]
betweenWordTrigrams input_list = fmap betweenWordTrigramsMap input_list

betweenWordTrigramsMap :: Text -> [Text]
betweenWordTrigramsMap input_text = [Data.Text.take 1 (Data.Text.drop 1 input_text), Data.Text.drop (Data.Text.length input_text - 2) input_text]

getTrigrams :: Seq Text -> Seq [Text]
getTrigrams words_seq = middleWordTrigrams words_seq >< betweenWordTrigrams words_seq

uniqueTrigrams :: Seq [Text] -> Int
uniqueTrigrams input_seq_list = size (Data.HashSet.fromList (concat input_seq_list))

tryRemove :: Int -> Int -> Seq Text -> Seq [Text] -> (Seq Text, Seq [Text])
tryRemove word_index trigrams_len all_words trigrams_raw = do
    let end_index = Data.Sequence.length all_words - 1
    if word_index > end_index then do
        (all_words, trigrams_raw)
    else do
        let new_words = deleteAt word_index all_words
        let new_trigrams_raw = deleteAt word_index trigrams_raw
        if trigrams_len == uniqueTrigrams new_trigrams_raw then do
            tryRemove word_index trigrams_len new_words new_trigrams_raw
        else do
            tryRemove (word_index + 1) trigrams_len all_words trigrams_raw

tryRemoveProgressive :: Int -> Int -> Seq Text -> Seq [Text] -> Seq Text
tryRemoveProgressive slice_size slice_multiplier all_words trigrams_raw = do
    if slice_size > Data.Sequence.length all_words then do
        let trigrams_len = uniqueTrigrams trigrams_raw
        fst (tryRemove 0 trigrams_len all_words trigrams_raw)
    else do
        let chunked_words = chunksOf slice_size all_words
        let chunked_trigrams = chunksOf slice_size trigrams_raw
        let chunks_processed = [tryRemove 0 (uniqueTrigrams (index chunked_trigrams chunk_index)) (index chunked_words chunk_index) (index chunked_trigrams chunk_index) | chunk_index <- [0..(Data.Sequence.length chunked_words - 1)]]
        let new_words = unChunkerFst chunks_processed
        let new_trigrams_raw = unChunkerSnd chunks_processed
        tryRemoveProgressive (slice_size * slice_multiplier) slice_multiplier new_words new_trigrams_raw

unChunkerFst :: [(Seq Text, Seq [Text])] -> Seq Text
unChunkerFst input_list = Data.Sequence.fromList (concat [toList (fst item) | item <- input_list])

unChunkerSnd :: [(Seq Text, Seq [Text])] -> Seq [Text]
unChunkerSnd input_list = Data.Sequence.fromList (concat [toList (snd item) | item <- input_list])

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

filterCapitals :: Text -> Text
filterCapitals input_text = toLower input_text

main :: IO ()
main = do
    putStrLn "Input file: "
    input_filename <- getLine
    file_data <- readFile input_filename

    putStrLn "Treat capital and lowercase the same (y/n): "
    capital_lowercase_handling <- getLine

    putStrLn "Chunk size: "
    chunk_size_input <- getLine
    let chunk_size = read chunk_size_input :: Int

    putStrLn "Chunk multiplier: "
    chunk_multiplier_input <- getLine
    let chunk_multiplier = read chunk_multiplier_input :: Int

    time_start <- getCurrentTime --benchmarking time

    let words_list = findSplit (concat (splitOn "\"" (concat (splitOn " " (concat (splitOn "\n" file_data))))))
    let words_seq_padded = Data.Sequence.fromList [pack (" " ++ word ++ " ") | word <- words_list]

    let trigrams_raw = if capital_lowercase_handling == "y" then getTrigrams (fmap filterCapitals words_seq_padded) else getTrigrams words_seq_padded

    let condensed_list = toList (tryRemoveProgressive chunk_size chunk_multiplier words_seq_padded trigrams_raw)

    let unpadded_list = [Prelude.drop 1 (Prelude.take (Prelude.length (unpack word) - 1) (unpack word)) | word <- condensed_list]

    writeFile "output.txt" (concat ([word ++ " " | word <- unpadded_list]))

    let json_words = concat (["        \"" ++ word ++ "\",\n" | word <- unpadded_list])

    writeFile "output.json" ("{\n    \"total\": " ++ show (Prelude.length unpadded_list) ++ ",\n    \"texts\": [\n" ++ Prelude.take (Prelude.length json_words - 2) json_words ++ "\n    ]\n}")

    time_end <- getCurrentTime --benchmarking time
    putStr "Generated in: "
    print (diffUTCTime time_end time_start)