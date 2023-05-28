use std::fs;
use std::collections::HashMap;
use std::io::{stdin};
use std::io::Write;
use std::time::Instant;

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

    //benchmarking code
    let start_time = Instant::now();

    let raw_file_string = fs::read_to_string(&input_filepath).unwrap();
    let mut raw_words: Vec<String> = dirty_parse_json(raw_file_string).unwrap();
    let padded_words = raw_words.iter().map(|x| format!(" {} ", x));

    let ngram = 3;
    let per_word_ngrams: Vec<Vec<String>> = padded_words.map(|x| get_trigrams(x, ngram, filter_case).unwrap_or_default()).collect();

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

    let output_filenames = get_output_filename(input_filepath, filter_case);
    plaintext_write(&raw_words, output_filenames.0);
    json_write(&raw_words, output_filenames.1);

    //benchmarking code
    let time_elapsed = start_time.elapsed();
    println!("Generated in: {time_elapsed:.6?}");
}

fn dirty_parse_json(json: String) -> Option<Vec<String>> {
    let replaced_string: String = json.replace(&[' ', '\t', '\"', '\n', '\r'], "");
    let filtered_string_words: Vec<&str> = replaced_string.split("words:[").collect();
    let filtered_string_texts: Vec<&str> = replaced_string.split("texts:[").collect();
    let word_list: Vec<&str> = if filtered_string_words.len() > 1 {
        filtered_string_words
    } else if filtered_string_texts.len() > 1 {
        filtered_string_texts
    } else {
        println!("Unsupported json key");
        return None;
    };

    let split_end: Vec<&str> = word_list.get(1).unwrap().split("]").collect();
    let split_commas: Vec<String> = split_end.get(0).unwrap().split(",").map(str::to_string).collect();
    return Some(split_commas);
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

fn plaintext_write(input_words: &Vec<String>, filepath: String) {
    let mut plaintext_output_file = match fs::OpenOptions::new().create(true).write(true).truncate(true).open(filepath) {
        Ok(file_ok) => file_ok,
        Err(_err) => panic!("Couldn't open file.")
    };

    let mut plaintext_string: String = Default::default();
    for i in 0..input_words.len() {
        plaintext_string += input_words.get(i).unwrap_or(&"".to_owned());
        plaintext_string += " ";
    }

    plaintext_output_file.write(plaintext_string.as_bytes()).unwrap_or_default();
}

fn json_write(input_words: &Vec<String>, filepath: String) {
    let mut json_output_file = match fs::OpenOptions::new().create(true).write(true).truncate(true).open(filepath) {
        Ok(file_ok) => file_ok,
        Err(_err) => panic!("Couldn't open file.")
    };

    let mut json_string: String = format!("{}{}{}", "{\n    \"total\": ", input_words.len(), ",\n    \"texts\": [\n");
    for i in 0..input_words.len() {
        json_string += "        \"";
        json_string += input_words.get(i).unwrap_or(&"".to_owned());
        json_string += "\"";
        if i < input_words.len() - 1 {
            json_string += ",\n";
        }
    }
    json_string += "\n    ]\n}";

    json_output_file.write(json_string.as_bytes()).unwrap_or_default();
}

fn get_output_filename(input_filename: String, filter_case: bool) -> (String, String) {
    let mut split_string: Vec<&str> = input_filename.split(".").collect();
    let split_len = split_string.len();
    if split_len > 1 {
        split_string.remove(split_len - 1);
    }
    let base_filename = split_string.join(".");
    let plaintext_filename = if filter_case {base_filename.to_owned() + "_compressed_ignorecase.txt"} else {base_filename.to_owned() + "_compressed.txt"};
    let json_filename = if filter_case {base_filename.to_owned() + "_compressed_ignorecase.json"} else {base_filename.to_owned() + "_compressed.json"};
    
    return (plaintext_filename, json_filename);
}