import Data.Time (getCurrentTime, diffUTCTime)

main :: IO()
main = do
    let file = "output.txt"

    putStrLn "Start Number: "
    start_number_input <- getLine
    let start_number = read start_number_input :: Int

    putStrLn "End Number: "
    end_number_input <- getLine
    let end_number = read end_number_input :: Int

    putStrLn "Number Prefix: "
    number_prefix <- getLine

    putStrLn "Number Suffix: "
    number_suffix <- getLine

    time_start <- getCurrentTime --benchmarking time

    writeFile file (concat [number_prefix ++ show current_number ++ number_suffix ++ "\n" | current_number <- [start_number..end_number]])

    time_end <- getCurrentTime --benchmarking time
    putStr "Generated in: "
    print (diffUTCTime time_end time_start)