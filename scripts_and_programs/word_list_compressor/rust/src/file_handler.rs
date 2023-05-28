use std::io::Write;
use std::fs;

pub fn parse_json(json: String) -> Option<Vec<String>> {
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

pub fn plaintext_write(input_words: &Vec<String>, filepath: String) {
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

pub fn json_write(input_words: &Vec<String>, filepath: String) {
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

pub fn get_output_filename(input_filename: String, filter_case: bool, ngram_greek_prefix: String) -> (String, String) {
    let mut split_string: Vec<&str> = input_filename.split(".").collect();
    let split_len = split_string.len();
    if split_len > 1 {
        split_string.remove(split_len - 1);
    }

    let ignorecase_string = if filter_case {"_ignorecase"} else {""};

    let base_filename = split_string.join(".");
    let plaintext_filename = format!("{}_{}gram_compressed{}.txt", base_filename, ngram_greek_prefix, ignorecase_string);
    let json_filename = format!("{}_{}gram_compressed{}.json", base_filename, ngram_greek_prefix, ignorecase_string);
    
    return (plaintext_filename, json_filename);
}