use std::fs;
use std::collections::HashMap;
use std::io::stdin;
use std::time::Instant;

mod greek_prefix;
mod file_handler;

fn main() {
    let mut input_filepath: String = Default::default();
    println!("Input file: ");
    match stdin().read_line(&mut input_filepath) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    input_filepath = input_filepath.trim().to_owned();

    let mut filter_case_input: String = Default::default();
    println!("Ignore uppercase and lowercase (y/n): ");
    match stdin().read_line(&mut filter_case_input) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    let filter_case = filter_case_input.trim() == "y";

    let mut ngram_size: String = Default::default();
    println!("Size of ngram to calculate (Default: 3): ");
    match stdin().read_line(&mut ngram_size) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    ngram_size = ngram_size.trim().to_string(); 
    let ngram_size_int: usize = if ngram_size == "" {ngram_size = "3".to_string(); 3} else {ngram_size.parse().unwrap()};

    //benchmarking code
    let start_time = Instant::now();

    let raw_file_string = fs::read_to_string(&input_filepath).unwrap();
    let mut raw_words: Vec<String> = file_handler::parse_json(raw_file_string).unwrap();
    let padded_words = raw_words.iter().map(|x| format!(" {} ", x));

    let per_word_ngrams: Vec<Vec<String>> = padded_words.map(|x| get_trigrams(x, ngram_size_int, filter_case).unwrap_or_default()).collect();

    let flattened_ngrams = per_word_ngrams.clone().into_iter().flatten();
    let mut ngrams_hashmap: HashMap<String, i32> = flattened_ngrams.fold(HashMap::new(), |mut map, c| {*map.entry(c).or_insert(0) += 1; map});

    let mut removal_indexes: Vec<usize> = Default::default();

    for (i, word) in per_word_ngrams.iter().enumerate() {
        let mut found_zero = false;
        for ngram in word {
            if ngrams_hashmap.get_key_value(ngram).unwrap_or((&"".to_owned(), &1)) <= (&ngram, &1) {
                found_zero = true;
            }
        }
        if !found_zero {
            for ngram in word {
                ngrams_hashmap.entry(ngram.to_string()).and_modify(|x| {*x -= 1}).or_insert(0);
            }
            removal_indexes.push(i);
        }
    }

    removal_indexes.sort();
    removal_indexes.reverse();
    for index in removal_indexes {
        raw_words.remove(index);
    }

    let output_filenames = file_handler::get_output_filename(input_filepath, filter_case, greek_prefix::greek_prefix(ngram_size));
    file_handler::plaintext_write(&raw_words, output_filenames.0);
    file_handler::json_write(&raw_words, output_filenames.1);

    //benchmarking code
    let time_elapsed = start_time.elapsed();
    println!("Generated in: {time_elapsed:.6?}");
}

fn get_trigrams(input_string: String, ngram: usize, filter_case: bool) -> Option<Vec<String>> {
    if input_string.len() < ngram {
        return None;
    }

    let working_input_string = if filter_case {input_string.to_lowercase()} else {input_string};

    let range = 0..(working_input_string.len() - (ngram - 1));

    let middle_mapslices: Vec<String> = range.map(|i| working_input_string.split_at(i).1.split_at(ngram).0.to_owned()).collect();
    let start_slice: String = "|".to_owned() + working_input_string.split_at(1).1.split_at(ngram / 2).0;
    let end_slice: String = working_input_string.split_at(working_input_string.len() - ngram).1.split_at((ngram - 1) / 2).0.to_owned() + "|";

    let mut slices: Vec<String> = middle_mapslices;
    slices.push(start_slice);
    slices.push(end_slice);

    return Some(slices);
}